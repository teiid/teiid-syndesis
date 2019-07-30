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

import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.komodo.KException;
import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.KomodoService;
import org.komodo.rest.connections.SyndesisConnectionMonitor.EventMsg;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.rest.service.KomodoMetadataService;
import org.komodo.rest.service.KomodoUtilService;
import org.teiid.adminapi.AdminException;

/**
 * This class provides the communication and hooks
 *
 */
public class SyndesisConnectionSynchronizer {
	private static final Log LOGGER = LogFactory.getLog(SyndesisConnectionSynchronizer.class);
	
	private TeiidOpenShiftClient openshiftClient;

	OAuthCredentials bogusCredentials = new OAuthCredentials("supersecret", "developer");
	private KomodoMetadataService metadataService;

	public SyndesisConnectionSynchronizer(TeiidOpenShiftClient toc, KomodoMetadataService metadataService) {
		this.openshiftClient = toc;
		this.metadataService = metadataService;
	}

	/*
	 * This method processes each connection event and delegates to appropriate
	 * connection operation
	 */
	public boolean handleConnectionEvent(final EventMsg event) {
		try {
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
		} catch (Exception e) {
			LOGGER.error("Error on connection event", e);
		}
		return false;
	}

	/*
	 * This method checks each applicable syndesis connection and updates all
	 * associated syndesisSource vdbs and schema
	 */
	public boolean synchronizeConnections() {
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
			LOGGER.error("Error syncronizing", e);
		}
		return false;
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
		if (status != null
				&& status.getVdbState() == RestSyndesisSourceStatus.EntityState.ACTIVE) {
			LOGGER.info("Schema already in repo for source " + sds.getName() +" skipping refresh");
			return;
		}

		// check for 5 mins
		boolean vdbRequestSubmitted = false;
		long start = System.currentTimeMillis();
		while (System.currentTimeMillis() - start < (5 * 60 * 1000)) {
			status = checkMetadataStatus(sds.getName());
			if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.ACTIVE) {
				// we are done.
				LOGGER.info("Schema Generation Success for source " + sds.getName());
				break;
			} else if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.FAILED) {
				LOGGER.warn("VDB deployment Failed for fetching metadata for source " + sds.getName());
				break;
			} else if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.MISSING && !vdbRequestSubmitted) {
				requestMetadataForDataSource(sds);
				vdbRequestSubmitted = true;
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
		try {
			return metadataService.getSyndesisSourceStatusByName(dsName, KomodoService.SYSTEM_USER);
		} catch (Exception e) {
			LOGGER.warn("Failed to get metadata status " + dsName, e);
			return null;
		}
	}

	private void requestMetadataForDataSource(DefaultSyndesisDataSource sds) throws KException {
		try {
			this.metadataService.refreshSchema(sds.getName(), true, KomodoService.SYSTEM_USER);
			LOGGER.info("submitted request to fetch metadata of connection " + sds.getName());
		} catch (Exception e) {
			LOGGER.warn("Failed to fetch metadata for connection " + sds.getName(), e);
		}
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
		try {
			this.metadataService.deleteSchema(status.getSchemaModelName(), KomodoService.SYSTEM_USER);
			LOGGER.info("Workspace schema " + status.getSchemaModelName() + " deleted.");
		} catch (Exception e) {
			LOGGER.info("Failed to delete schema " + status.getSchemaModelName(), e);
		}
	}

	private void deleteSourceVDB(RestSyndesisSourceStatus status) throws KException {
		try {
			this.metadataService.removeVdb(status.getVdbName());
			LOGGER.info("Source VDB " + status.getVdbName() + " deleted.");
		} catch (Exception e) {
			LOGGER.info("Failed to delete Source VDB " + status.getVdbName(), e);
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
		try {
			this.metadataService.refreshPreviewVdb(KomodoUtilService.PREVIEW_VDB, KomodoService.SYSTEM_USER);
			LOGGER.info("Preview VDB Updated");
			return true;
		} catch (Exception e) {
			LOGGER.info("Failed to Update Preview VDB", e);
		}
		
		return false;
	}	
}