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
import org.komodo.core.KEngine;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.AbstractKomodoMetadataService;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.connections.SyndesisConnectionMonitor.EventMsg;
import org.komodo.spi.KException;
import org.komodo.spi.runtime.SyndesisDataSource;

/**
 * This class provides the communication and hooks
 *
 */
public class SyndesisConnectionSynchronizer extends AbstractKomodoMetadataService {
	private static final Log LOGGER = LogFactory.getLog(SyndesisConnectionSynchronizer.class);

	private KEngine kengine;
	private TeiidOpenShiftClient openshiftClient;
	
	OAuthCredentials bogusCredentials = new OAuthCredentials("supersecret", "developer");

	public SyndesisConnectionSynchronizer(KEngine kengine, TeiidOpenShiftClient client) {
		super(kengine);
		this.kengine = kengine;
		openshiftClient = client;
	}

	/*
	 * This method processes each connection event and delegates to appropriate
	 * connection operation
	 */
	public void handleConnectionEvent(EventMsg event) {
		switch (event.getAction()) {

		case created: {
			LOGGER.info("Handling CREATE connection with Event ID = " + event.getId());
			handleAddConnection(event);
		}
			break;

		case deleted: {
			LOGGER.info("Handling DELETE connection with Event ID = " + event.getId());
			handleDeleteConnection(event);
		}
			break;

		case updated: {
			LOGGER.info("Handling UPDATE connection with Event ID = " + event.getId());
			handleUpdateConnection(event);

		}
			break;
		}
	}

	/*
	 * This method checks each applicable syndesis connection and updates all
	 * associated syndesisSource vdbs and schema
	 */
	public void synchronizeConnections() {

		// get all source connections from syndesis

		try {
			// Get syndesis sources
			Collection<SyndesisDataSource> dataSources = this.openshiftClient.getSyndesisSources(bogusCredentials);
			
			for( SyndesisDataSource sds : dataSources ) {
				SyndesisSourceVdbProcessor processor = new SyndesisSourceVdbProcessor(this.kengine, this.openshiftClient, sds.getName());
				
				try {
					boolean success = processor.refreshSchema(false);
					
					if( success ) {
						LOGGER.info("synchronizeConnections()  Schema REFRESHED for source = " + sds.getName());
					} else {
						LOGGER.info("synchronizeConnections()  Schema NOT REFRESHED for source = " + sds.getName() 
						 + " Check log for details...");
					}
				} catch (Exception e) {
					LOGGER.error("PROCESSOR ERROR in synchronizeConnections() for data source:  " + sds.getName() 
					+ " \nREASON: " + e);
				}
			}
			
			
		} catch (Exception e) {
			LOGGER.error(e);
		}
	}

	private void handleAddConnection(EventMsg event) {
		try {
			this.openshiftClient.bindToSyndesisSourceById(bogusCredentials, event.getId());
			
			SyndesisDataSource sds = this.openshiftClient.getSyndesisDataSourceById(bogusCredentials, event.getId());

			SyndesisSourceVdbProcessor processor = new SyndesisSourceVdbProcessor(this.kengine, this.openshiftClient, sds.getName());
			
			try {
				processor.refreshSchema(true);

			} catch (Exception e) {
				LOGGER.error("PROCESSOR ERROR in synchronizeConnections() for data source:  " + sds.getName() 
				+ " \nREASON: " + e);
			}
		} catch (Exception e) {
			LOGGER.error(e);
		}
	}

	private boolean handleDeleteConnection(EventMsg event) {
		try {
			String sourceName = this.openshiftClient.findDataSourceName(event.getId());
			
			SyndesisSourceVdbProcessor processor = new SyndesisSourceVdbProcessor(this.kengine, this.openshiftClient, sourceName);
			processor.removeConnection(sourceName);
			
			LOGGER.info("DELETE connection completed for " + sourceName);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}

		return true;
	}

	/*
	 * Updating a connection requires deleting the existing connection and associated objects and 
	 * then re-adding the same connection.
	 */
	private void handleUpdateConnection(EventMsg event) {

		try {
			String sourceName = this.openshiftClient.findDataSourceName(event.getId());
			
			if( handleDeleteConnection(event) ) {
				handleAddConnection(event);
			} else {
				throw new KException("Delete connection failed. Check log for details");
			}
			
			LOGGER.info("UPDATE connection completed for " + sourceName);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		
	}
}