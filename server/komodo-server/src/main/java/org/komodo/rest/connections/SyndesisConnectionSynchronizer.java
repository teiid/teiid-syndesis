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
import org.komodo.rest.datavirtualization.RestSyndesisSourceStatus;
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
        DefaultSyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceById(bogusCredentials,
                event.getId());
        if (sds != null) {
            addConnection(sds);
            LOGGER.info("Add connection completed for " + sds.getSyndesisName());
        } else {
            LOGGER.info("failed find data source with id " + event.getId());
        }
    }

    /*
     * Updating a connection requires deleting the existing connection and
     * associated objects and then re-adding the same connection.
     */
    private void handleUpdateConnection(EventMsg event) throws KException{
        DefaultSyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceById(bogusCredentials,
                event.getId());
        if (sds != null) {
            deleteConnection(sds);
            handleAddConnection(event);
        }
        LOGGER.info("UPDATE connection completed for " + sds.getSyndesisName());
    }

    private boolean handleDeleteConnection(EventMsg event) throws KException {
        try {
            // note here that the datasource is already deleted from the syndesis
            // so we would need to search by local cached event id
            DefaultSyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceById(bogusCredentials,
                    event.getId());
            if (sds != null) {
                return deleteConnection(sds);
            }

            return false;
        } catch (Exception e) {
            throw handleError(e);
        }
    }

    private void addConnection(DefaultSyndesisDataSource sds) throws KException {
        if (sds.getKomodoName() == null) {
            this.openshiftClient.bindToSyndesisSource(bogusCredentials, sds);
        }

        // check if the metadata is already available, then skip it.
        RestSyndesisSourceStatus status = checkMetadataStatus(sds);
        if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.ACTIVE) {
            LOGGER.info("Schema already in repo for source " + sds.getSyndesisName() +" skipping refresh");
            return;
        }

        // check for 5 mins
        boolean vdbRequestSubmitted = false;
        long start = System.currentTimeMillis();
        while (System.currentTimeMillis() - start < (5 * 60 * 1000)) {
            status = checkMetadataStatus(sds);
            if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.ACTIVE) {
                // we are done.
                LOGGER.info("Schema Generation Success for source " + sds.getSyndesisName());
                break;
            } else if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.FAILED) {
                LOGGER.warn("VDB deployment Failed for fetching metadata for source " + sds.getSyndesisName());
                break;
            } else if (status.getVdbState() == RestSyndesisSourceStatus.EntityState.MISSING && !vdbRequestSubmitted) {
                requestMetadataForDataSource(sds);
                vdbRequestSubmitted = true;
            }

            // sleep for 3 seconds
            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                Thread.interrupted();
                break;
            }
        }
    }

    private RestSyndesisSourceStatus checkMetadataStatus(DefaultSyndesisDataSource dsd) throws KException {
        try {
            return metadataService.getSyndesisSourceStatus(dsd, KomodoService.SYSTEM_USER_NAME);
        } catch (Exception e) {
            LOGGER.warn("Failed to get metadata status " + dsd.getSyndesisName(), e);
            return null;
        }
    }

    private void requestMetadataForDataSource(DefaultSyndesisDataSource sds) throws KException {
        try {
            this.metadataService.refreshSchema(sds.getKomodoName(), true, KomodoService.SYSTEM_USER_NAME);
            LOGGER.info("submitted request to fetch metadata of connection " + sds.getSyndesisName());
        } catch (Exception e) {
            LOGGER.warn("Failed to fetch metadata for connection " + sds.getSyndesisName(), e);
        }
    }

    private boolean deleteConnection(DefaultSyndesisDataSource dsd) throws KException {
        try {
            RestSyndesisSourceStatus status = checkMetadataStatus(dsd);
            if (status != null && status.getId() != null) {
                deleteSchemaModel(status);
            }

            if (status != null && status.getVdbName() != null) {
                deleteSourceVDB(status);
            }

            String komodoName = dsd.getKomodoName();
            if (komodoName != null) {
                this.openshiftClient.deleteDataSource(komodoName);
            }
            LOGGER.info("Connection deleted " + dsd.getSyndesisName());
            return true;
        } catch(AdminException e) {
            throw handleError(e);
        }
    }

    private void deleteSchemaModel(RestSyndesisSourceStatus status) throws KException {
        try {
            if (this.metadataService.deleteSchema(status.getId(), KomodoService.SYSTEM_USER_NAME)) {
                LOGGER.info("Workspace schema " + status.getSourceName() + " deleted.");
            } // else already deleted
        } catch (Exception e) {
            LOGGER.info("Failed to delete schema " + status.getSourceName(), e);
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
            this.metadataService.refreshPreviewVdb(KomodoUtilService.PREVIEW_VDB, KomodoService.SYSTEM_USER_NAME);
            LOGGER.info("Preview VDB Updated");
            return true;
        } catch (Exception e) {
            LOGGER.info("Failed to Update Preview VDB", e);
        }

        return false;
    }
}