/*
 * Copyright Red Hat, Inc. and/or its affiliates
 * and other contributors as indicated by the @author tags and
 * the COPYRIGHT.txt file distributed with this work.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.rest.connections;

import java.io.IOException;
import java.util.Collection;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.relational.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.KomodoService;
import org.komodo.rest.connections.SyndesisConnectionMonitor.EventMsg;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.spi.KEngine;
import org.komodo.spi.KException;
import org.komodo.spi.SystemConstants;
import org.komodo.spi.repository.UnitOfWork;
import org.teiid.adminapi.AdminException;

import com.fasterxml.jackson.databind.ObjectMapper;

import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;

/**
 * This class provides the communication and hooks
 *
 */
public class SyndesisConnectionSynchronizer {
	private static final Log LOGGER = LogFactory.getLog(SyndesisConnectionSynchronizer.class);
	private static final String LOCAL_REST = "http://localhost:8080/vdb-builder/v1";
	
	private ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1);
	private TeiidOpenShiftClient openshiftClient;
	private OkHttpClient client;

	OAuthCredentials bogusCredentials = new OAuthCredentials("supersecret", "developer");
	private KEngine kengine;

	public SyndesisConnectionSynchronizer(TeiidOpenShiftClient toc, KEngine kengine) {
		this.openshiftClient = toc;
		this.client = buildHttpClient();
		this.kengine = kengine;
	}

	/*
	 * This method processes each connection event and delegates to appropriate
	 * connection operation
	 */
	public Future<Boolean> handleConnectionEvent(final EventMsg event) {
		Future<Boolean> future = executor.submit(new Callable<Boolean>() {
			public Boolean call() throws Exception {
				switch (event.getAction()) {
				case created:
					LOGGER.info("Handling CREATE connection with Event ID = " + event.getId());
					handleAddConnection(event);
					break;
				case deleted:
					LOGGER.info("Handling DELETE connection with Event ID = " + event.getId());
					handleDeleteConnection(event);
					break;
				case updated:
					LOGGER.info("Handling UPDATE connection with Event ID = " + event.getId());
					handleUpdateConnection(event);
					break;
				}
				synchronzePreviewVDB();
				return true;
			}
		});		
		return future;
	}

	/*
	 * This method checks each applicable syndesis connection and updates all
	 * associated syndesisSource vdbs and schema
	 */
	public Future<Boolean> synchronizeConnections() {
		 return executor.submit(new Callable<Boolean>() {
			public Boolean call() throws Exception{
				try {
					// Get syndesis sources
					Collection<DefaultSyndesisDataSource> dataSources = openshiftClient
							.getSyndesisSources(bogusCredentials);
					for (DefaultSyndesisDataSource sds : dataSources) {
						addConnection(sds);
					}
					synchronzePreviewVDB();
					return true;
				} catch (Exception e) {
					LOGGER.error(e);
				}
				return false;
			}
		});
	}

	private void handleAddConnection(EventMsg event) throws KException {
		DefaultSyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceByEventId(bogusCredentials,
				event.getId());
		if (sds != null) {
			addConnection(sds);
			LOGGER.info("Add connection completed for " + sds.getName());
		} else {
			LOGGER.info("failed find data source with id " + event.getId());
		}
	}

	/*
	 * Updating a connection requires deleting the existing connection and
	 * associated objects and then re-adding the same connection.
	 */
	private void handleUpdateConnection(EventMsg event) throws KException{
		DefaultSyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceByEventId(bogusCredentials,
				event.getId());
		if (sds != null) {
			deleteConnection(sds.getName());
			handleAddConnection(event);
		}
		LOGGER.info("UPDATE connection completed for " + sds.getName());
	}
	
	private boolean handleDeleteConnection(EventMsg event) throws KException {
		try {
			// note here that the datasource is already deleted from the syndesis
			// so we would need to search by local cached event id
			String dsName = this.openshiftClient.findDataSourceNameByEventId(event.getId());
			if (dsName == null) {
				return true;
			}
			
			return deleteConnection(dsName);
		} catch (Exception e) {
			throw handleError(e);
		}
	}	
	
	private void addConnection(DefaultSyndesisDataSource sds) throws KException {
		if (!sds.isBound()) {
			this.openshiftClient.bindToSyndesisSource(bogusCredentials, sds);
		}

		// check if the metadata is already available, then skip it.
		RestSyndesisSourceStatus status = checkMetadataStatus(sds.getName());
		if (status != null && status.getSchemaState() == RestSyndesisSourceStatus.EntityState.ACTIVE
				&& status.getVdbState() == RestSyndesisSourceStatus.EntityState.ACTIVE) {
			LOGGER.info("Schema already in repo for source " + sds.getName() +" skipping refresh");
			return;
		}

		// check for 5 mins
		boolean schemaRequestSubmitted = false;
		boolean vdbRequestSubmitted = false;
		long start = System.currentTimeMillis();
		while (System.currentTimeMillis() - start < (5 * 60 * 1000)) {
			status = checkMetadataStatus(sds.getName());
			if (status.getSchemaState() == RestSyndesisSourceStatus.EntityState.ACTIVE
					&& status.getVdbState() == RestSyndesisSourceStatus.EntityState.ACTIVE) {
				// we are done.
				LOGGER.info("Schema Generation Success for source " + sds.getName());
				break;
			} else if (status.getSchemaState() == RestSyndesisSourceStatus.EntityState.FAILED) {
				LOGGER.warn("Schema Generation Failed for fetching metadata for source " + sds.getName());
				break;
			} else if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.FAILED) {
				LOGGER.warn("VDB deployment Failed for fetching metadata for source " + sds.getName());
				break;
			} else if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.MISSING && !vdbRequestSubmitted) {
				requestMetadataForDataSource(sds, false);
				vdbRequestSubmitted = true;
			} else if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.ACTIVE
					&& status.getSchemaState() == RestSyndesisSourceStatus.EntityState.MISSING
					&& !schemaRequestSubmitted) {
				// request to read metadata and add to komodo repo
				requestMetadataForDataSource(sds, true);
				schemaRequestSubmitted = true;
			}
			
			// sleep for 5 seconds
			try {
				Thread.sleep(3000);
			} catch (InterruptedException e) {
				break;
			}			
		}
	}

	private RestSyndesisSourceStatus checkMetadataStatus(String dsName) throws KException {
		ObjectMapper mapper = new ObjectMapper();
		
		Request request = SyndesisConnectionMonitor.buildRequest().url(LOCAL_REST + "/metadata/syndesisSourceStatuses/"+dsName)
				.get().build();
		try (Response response = client.newCall(request).execute()) {
			if (response.isSuccessful()) {
				String r = new String(response.body().bytes());
				LOGGER.debug("status :" + r);
				RestSyndesisSourceStatus status = mapper.readValue(r,RestSyndesisSourceStatus.class);
				return status;
			}
			response.close();
		} catch (IOException e) {
			LOGGER.warn("Failed to submitted request to fetch metadata for connection " + dsName, e);
			throw handleError(e);
		}
		return null;
	}

	private void requestMetadataForDataSource(DefaultSyndesisDataSource sds, boolean generateSchema) {
		String query = "?redeploy=true&generate-schema=false";
		if (generateSchema) {
			query = "?redeploy=false&generate-schema=true";
		}
		Request request = SyndesisConnectionMonitor.buildRequest()
				.url(LOCAL_REST + "/metadata/refresh-schema/" + sds.getName() + query)
				.post(RequestBody.create(null, "")).build();
		try (Response response = this.client.newCall(request).execute()) {
			if (response.isSuccessful()) {
				LOGGER.info("submitted request to fetch metadata of connection " + sds.getName()
						+ " with schema-generation " + generateSchema);
			} else {
				LOGGER.warn("Failed to submitted request to fetch metadata for connection " + sds.getName());
			}
		} catch (IOException e) {
			LOGGER.warn("Failed to submitted request to fetch metadata for connection " + sds.getName(), e);
		}
	}

	private OkHttpClient buildHttpClient() {
		OkHttpClient client = new OkHttpClient.Builder().connectTimeout(10, TimeUnit.SECONDS)
				.writeTimeout(10, TimeUnit.SECONDS).readTimeout(30, TimeUnit.SECONDS).build();
		return client;
	}

	private boolean deleteConnection(String dsName) throws KException {
		try {
			RestSyndesisSourceStatus status = checkMetadataStatus(dsName);
			if (status != null && status.getSchemaModelName() != null) {
				deleteSchemaModel(status);
			}
	
			if (status != null && status.getVdbName() != null) {
				deleteSourceVDB(status);
			}
	
			this.openshiftClient.deleteDataSource(dsName);
			LOGGER.info("Connection deleted " + dsName);
			return true;
		} catch(AdminException e) {
			throw handleError(e);
		}
	}

	private void deleteSchemaModel(RestSyndesisSourceStatus status) throws KException {
        UnitOfWork uow = null;
        try {
            uow = kengine.createTransaction(SystemConstants.SYSTEM_USER, "delete schema", false, KomodoService.REPO_USER); //$NON-NLS-1$

            final WorkspaceManager mgr = kengine.getWorkspaceManager();
            
            boolean result = mgr.deleteSchema(status.getSchemaModelName());
            if (result) {
    			LOGGER.info("Workspace schema " + status.getSchemaModelName() + " deleted.");
    		} else {
    			LOGGER.info("Failed to delete schema " + status.getSchemaModelName());
    		}
            
            uow.commit();
        } catch (final Exception e) {
            if (uow != null) {
                uow.rollback();
            }
        }
	}

	private void deleteSourceVDB(RestSyndesisSourceStatus status) throws KException {
		Request request = SyndesisConnectionMonitor.buildRequest()
				.url(LOCAL_REST + "/metadata/vdbs/" + status.getVdbName()).delete().build();
		try (Response response = this.client.newCall(request).execute()) {
			if (response.isSuccessful()) {
				LOGGER.info("Source VDB " + status.getVdbName() + " deleted.");
			} else {
				LOGGER.info("Failed to delete Source VDB " + status.getVdbName());
			}
		} catch (IOException e) {
			throw handleError(e);
		}
	}
	
    protected static KException handleError(Throwable e) {
        assert (e != null);
        if (e instanceof KException) {
            return (KException)e;
        }
        return new KException(e);
    }

	private boolean synchronzePreviewVDB() {
		LOGGER.info("Preview VDB update Request being submitted.");
		Request request = SyndesisConnectionMonitor.buildRequest()
				.url(LOCAL_REST + "/metadata/refreshPreviewVdb/PreviewVdb").post(RequestBody.create(null, "")).build();
		try (Response response = this.client.newCall(request).execute()) {
			if (response.isSuccessful()) {
				LOGGER.info("Preview VDB Updated");
			} else {
				LOGGER.info("Failed to Update Preview VDB");
			}
			return true;
		} catch (IOException e) {
			LOGGER.error("Failed to Update Preview VDB", e);
		}
		return false;
	}	
}