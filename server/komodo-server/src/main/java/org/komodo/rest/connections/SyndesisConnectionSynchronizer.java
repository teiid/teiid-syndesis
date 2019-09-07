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
import org.komodo.rest.connections.SyndesisConnectionMonitor.EventMsg;
import org.komodo.rest.service.KomodoMetadataService;
import org.komodo.rest.service.KomodoMetadataService.SourceDeploymentMode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * This class provides the communication and hooks
 *
 */
@Component
public class SyndesisConnectionSynchronizer {
    private static final Log LOGGER = LogFactory.getLog(SyndesisConnectionSynchronizer.class);

    private TeiidOpenShiftClient openshiftClient;

    OAuthCredentials bogusCredentials = new OAuthCredentials("supersecret", "developer");
    private KomodoMetadataService metadataService;

    public SyndesisConnectionSynchronizer(@Autowired TeiidOpenShiftClient toc, @Autowired KomodoMetadataService metadataService) {
        this.openshiftClient = toc;
        this.metadataService = metadataService;
    }

    /*
     * This method processes each connection event and delegates to appropriate
     * connection operation
     */
    public void handleConnectionEvent(final EventMsg event) throws KException {
        switch (event.getAction()) {
        case created:
            LOGGER.info("Handling CREATE connection with Event ID = " + event.getId());
            handleAddConnection(event, false);
            break;
        case deleted:
            LOGGER.info("Handling DELETE connection with Event ID = " + event.getId());
            handleDeleteConnection(event);
            break;
        case updated:
            LOGGER.info("Handling UPDATE connection with Event ID = " + event.getId());
            handleAddConnection(event, true);
            break;
        }
    }

    /*
     * This method checks each applicable syndesis connection and updates all
     * associated syndesisSource vdbs and schema
     */
    public void synchronizeConnections() throws KException {
        // Get syndesis sources
        Collection<DefaultSyndesisDataSource> dataSources = openshiftClient
                .getSyndesisSources(bogusCredentials);
        for (DefaultSyndesisDataSource sds : dataSources) {
            addConnection(sds, false);
        }
    }

    private void handleAddConnection(EventMsg event, boolean update) throws KException {
        DefaultSyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceById(bogusCredentials,
                event.getId());
        if (sds != null) {
            addConnection(sds, update);
        }
    }

    private void handleDeleteConnection(EventMsg event) throws KException {
        // note here that the datasource is already deleted from the syndesis
        // so we would need to search by local cached event id
        DefaultSyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceById(bogusCredentials,
                event.getId());
        if (sds != null) {
            deleteConnection(sds);
        }
    }

    public void addConnection(DefaultSyndesisDataSource sds, boolean update) {
        if (update) {
            try {
                this.openshiftClient.deleteDataSource(sds);
            } catch (KException e) {
                LOGGER.warn("Error deleting data source for " + sds.getSyndesisName(), e);
            }
        }
        try {
            this.openshiftClient.createDataSource(sds);
        } catch (Exception e) {
            LOGGER.warn("Error creating data source for " + sds.getSyndesisName(), e);
            return;
        }

        try {
            this.metadataService.deploySourceVdb(sds.getKomodoName(), update?SourceDeploymentMode.REFRESH:SourceDeploymentMode.MAKE_LIVE);
            LOGGER.info("submitted request to fetch metadata of connection " + sds.getSyndesisName());
        } catch (Exception e) {
            LOGGER.warn("Failed to fetch metadata for connection " + sds.getSyndesisName(), e);
        }
    }

    public void deleteConnection(DefaultSyndesisDataSource dsd) throws KException {
        try {
            if (this.metadataService.deleteSchema(dsd)) {
                LOGGER.info("Workspace schema " + dsd.getKomodoName() + " deleted.");
            } // else already deleted
        } catch (Exception e) {
            LOGGER.info("Failed to delete schema " + dsd.getKomodoName(), e);
        }

        this.openshiftClient.deleteDataSource(dsd);
        LOGGER.info("Connection deleted " + dsd.getSyndesisName());
    }

}
