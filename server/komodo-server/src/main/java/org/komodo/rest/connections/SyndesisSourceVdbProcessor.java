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

import static org.komodo.rest.Messages.Error.COMMIT_TIMEOUT;

import java.util.concurrent.TimeUnit;

import org.komodo.core.KEngine;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.Messages;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;
import org.teiid.adminapi.AdminException;

/**
 * This class provides a means to fork a process for any source VDB that gets deployed to be
 * monitor for status and to complete the work of refreshing the schema to the repository metadata
 */

public class SyndesisSourceVdbProcessor extends AbstractSyndesisConnectionProcessor {
    private static final int TIMEOUT = 30;
    private static final TimeUnit UNIT = TimeUnit.SECONDS;

	private String syndesisSourceName;

	private TeiidOpenShiftClient tosClient;
	
	private OAuthCredentials bogusCredentials;
	
	public SyndesisSourceVdbProcessor(KEngine kengine, TeiidOpenShiftClient tosClient, String syndesisSourceName) {
		super(kengine);
		this.syndesisSourceName = syndesisSourceName;
		this.tosClient = tosClient;
		
		bogusCredentials = new OAuthCredentials("supersecret", "developer");
	}

	
	
	public boolean bind() {
		
		try {
			this.tosClient.bindToSyndesisSource(bogusCredentials, syndesisSourceName);
		} catch (KException e) {
			LOGGER.error("  ---->> bind(FAILED) FAILED for SOURCE = " + syndesisSourceName + "\n" + e);
			return false;
		}
		return true;
	}

	
	public boolean refreshSchema(boolean force) throws Exception {
		UnitOfWork uow = null;

		try {
			uow = KomodoTxnUtils.createTransaction(getKEngine(), bogusCredentials.getUser(), "refreshSchema", false); //$NON-NLS-1$
			
            // Find the bound teiid source corresponding to the syndesis source
            TeiidDataSource teiidSource = getKEngine().getMetadataInstance().getDataSource(syndesisSourceName);
            
            if (teiidSource == null) {
                if( !this.bind() ) {
                	return false;
                }
            }
            
            teiidSource = getKEngine().getMetadataInstance().getDataSource(syndesisSourceName);
            
            if (teiidSource == null) {
            	throw new KException("Error binding Syndesis source to teiid data source: " + syndesisSourceName);
            }

            TeiidVdb deployedVdb = findDeployedVdb( syndesisSourceName );

            // Initiate the VDB deployment
            
            if ( deployedVdb == null ) {

            	try {
            		doDeploySourceVdb(uow, teiidSource); // this will delete workspace VDB first
				} catch (Exception ex) {
					 LOGGER.error("Deploy source VDB failed, check connection properties and credentials");
					 return false;
				}
            } 

            final SynchronousCallback callback = ( SynchronousCallback )uow.getCallback();
            uow.commit();

            if ( ! callback.await( 30, TimeUnit.SECONDS ) ) {
                // callback timeout occurred
                String errorMessage = Messages.getString( COMMIT_TIMEOUT, uow.getName(), TIMEOUT, UNIT );
                throw new KException(errorMessage);
            }
            
            // check deployed status
            deployedVdb = findDeployedVdb( syndesisSourceName );
            if ( deployedVdb == null ) {
            	throw new KException("VDB did not deploy for Syndesis source : " + syndesisSourceName);
            } 
            
			uow = KomodoTxnUtils.createTransaction(getKEngine(), bogusCredentials.getUser(), "generateSchema", false); //$NON-NLS-1$
            
            Vdb schemaVdb = findWorkspaceSchemaVdb( uow, teiidSource );

            final String schemaModelName = getSchemaModelName( syndesisSourceName );
            Model schemaModel = null;

            // create if necessary
            if ( schemaVdb == null ) {
                final WorkspaceManager wkspMgr = getWorkspaceManager( uow );
                final String schemaVdbName = getSchemaVdbName( syndesisSourceName );
                schemaVdb = wkspMgr.createVdb( uow, null, schemaVdbName, schemaVdbName );

                // Add schema model to schema vdb
                schemaModel = addModelToSchemaVdb(uow, schemaVdb, teiidSource, schemaModelName);
            } else {
                final Model[] models = schemaVdb.getModels( uow, schemaModelName );

                if ( models.length != 0 ) {
                    schemaModel = models[ 0 ];
                } else {
                    // should never happen but just in case
                    schemaModel = addModelToSchemaVdb(uow, schemaVdb, teiidSource, schemaModelName);
                }
            }

            final String modelDdl = getMetadataInstance().getSchema( deployedVdb.getName(), "1", schemaModelName ); //$NON-NLS-1$
            schemaModel.setModelDefinition( uow, modelDdl );
            // after transaction is committed this will trigger the DDL sequencer which will create
            // the model objects.

            KomodoTxnUtils.commit(uow);

		} catch (final Exception e) {
			if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
				uow.rollback();
			}
            throw new KException(e);
		}
		return true;
	}
	
	/**
	 * Method needs to:
	 * 		1) Delete the model schema
	 * 		2) Delete the source VDB
	 * 		3) Delete the syndesis data source
	 * @param connectionName
	 */
	public void removeConnection(String sourceName) throws KException, AdminException {
		UnitOfWork uow = null;

		try {
			uow = KomodoTxnUtils.createTransaction(getKEngine(), bogusCredentials.getUser(), "removeVdb", false); //$NON-NLS-1$
			
			TeiidDataSource tds = findTeiidSource(sourceName);
			
			Vdb srcVdb = findWorkspaceSchemaVdb(uow, tds);
			
			// Undeploy the src vdb
			getMetadataInstance().undeployDynamicVdb(sourceName);

			// Await the undeployment to end
			Thread.sleep(DEPLOYMENT_WAIT_TIME);

			
			// delete the workspace src vdb

			if( srcVdb != null ) {
				getWorkspaceManager(uow).delete(uow, srcVdb);
			}
			
			// Delete Teiid Data Source
			
			this.tosClient.deleteDataSource(sourceName);
			
			// Await the undeployment to end
			Thread.sleep(DEPLOYMENT_WAIT_TIME);

			KomodoTxnUtils.commit(uow);

		} catch (final Exception e) {
			if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
				uow.rollback();
			}
			throw new KException(e);
		}

	}
}
