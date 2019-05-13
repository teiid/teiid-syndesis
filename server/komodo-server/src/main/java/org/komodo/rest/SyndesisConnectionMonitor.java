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
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.COMMIT_TIMEOUT;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.komodo.core.KEngine;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter.AuthToken;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;

import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.WebSocket;
import okhttp3.WebSocketListener;
import okio.ByteString;

/**
 * This class provides the communication and hooks
 *
 */
public class SyndesisConnectionMonitor extends AbstractKomodoMetadataService   {
	private KEngine kengine;
    private TeiidOpenShiftClient openshiftClient;
    
    private String username;

    private static final Log LOGGER = LogFactory.getLog(SyndesisConnectionMonitor.class);
    
    private static final int TIMEOUT = 30;
    private static final TimeUnit UNIT = TimeUnit.SECONDS;
    
    private WebSocket webSocket;
    
    private boolean connected;
    
    private AuthToken token;
    
    private enum EVENT_TYPE {
    	CREATE,
    	DELETE,
    	UPDATE
    }
	
	
	public SyndesisConnectionMonitor(KEngine kengine, AuthToken token) {
		super(kengine);
		this.kengine = kengine;
		this.token = token;
	}

	public boolean connect() {
	    
		/* 
		  1) connect to Syndesis
		  FROM UI's BaseITCase.ts
		*/
	   
	    if( token != null ) {
		    String RESERVATIONS_PATH = "ws://syndesis-server/api/v1/event/reservations";
			
			OkHttpClient client = new OkHttpClient();
	
//			String authHeaderName = HttpHeaders.AUTHORIZATION;
//			String authHeaderValue = token.getHttpAuthentication();
//			String synTokenName = "SYNDESIS-XSRF-TOKEN";
//			String synTokenValue = "awesome";
			String xForwardedUser = "X-Forwarded-User";
//			String user = "developer";
			String xForwardedAccessToken = "X-Forwarded-Access-Token";
			String xForwardedToken = "supersecret";
			String xForwardedOrigin = "X-Forwarded-Origin";
			String origin = "for=127.0.0.1;host=localhost:4200;proto=https";
			
			RequestBody dummyRequestBody = RequestBody.create(null, "Dummy Request Body");
			
//    		.addHeader(authHeaderName, authHeaderValue)
//    		.addHeader(synTokenName, synTokenValue)
		    Request request = new Request.Builder()
		    		.url(RESERVATIONS_PATH)
		    		.addHeader(xForwardedUser, "user")
		    		.addHeader(xForwardedAccessToken, xForwardedToken)
		    		.addHeader(xForwardedOrigin, origin)
		    		.post(dummyRequestBody)
		    		.build();
		    LOGGER.info("  *** SyndesisConnectionMonitor.connect() >> Created Request = " + request);
		    if( request.body() != null ) {
		    	LOGGER.info("  *** SyndesisConnectionMonitor.connect() >>          Body = " + request.body().toString());
		    }
		    if( request.headers() != null ) {
		    	LOGGER.info("  *** SyndesisConnectionMonitor.connect() >>          Headers = " + request.headers().toString());
		    }
		    webSocket = client.newWebSocket(request, new WebSocketListener() {

		        @Override
		        public void onOpen(WebSocket webSocket, Response response) {
		            LOGGER.info("  *** WebSocketListener.onOpen(): Socket has been opened successfully.");
		            // reset connections attempts counter
		        }

		        @Override
		        public void onMessage(WebSocket webSocket, String text) {
		            // print received message in log
		        	LOGGER.info( "  *** WebSocketListener.onMessage(String): New Text Message received "+text);

		    		
		   		 try {
		   			 handleConnectionEvent(null);
		   		 } catch (KException kex) {
		   			 
		   		 } catch (Exception ex) {
		   			 
		   		 } 
		        }

		        @Override
		        public void onMessage(WebSocket webSocket, ByteString message) {
		        	LOGGER.info( "  *** WebSocketListener.onMessage(ByteString): New ByteString Message received "+message);
		        }

		        @Override
		        public void onClosing(WebSocket webSocket, int code, String reason) {
		        	LOGGER.info("  *** WebSocketListener.onClosing(): Close request from server with reason '"+reason+"'");
//		            changeState(State.CLOSING);
		            webSocket.close(1000,reason);
		        }

		        @Override
		        public void onClosed(WebSocket webSocket, int code, String reason) {
		        	LOGGER.info("  *** WebSocketListener.onClosed(): Socket connection closed with reason '"+reason+"'");
		        }

		        @Override
		        public void onFailure(WebSocket webSocket, Throwable t, Response response) {
		        	if( response.headers() != null ) {
		        		LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Response Headers = " + response.headers());
		        	}
		        	if( response.body() != null ) {
		        		LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Response Body = " + response.body());
		        	}
		        	if( response.message() != null ) {
		        		LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Response Message = " + response.message());
		        	}
		            LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Error = " + t.getLocalizedMessage());
		        }
		    }
		);
		    
		    connected = true;
	    } else {
		    connected = false;
	    }

	    LOGGER.info("SyndesisConnectionMonitor.connect() == " + connected);
	    return connected;
	}
	
	public boolean isConnected() {
		return webSocket != null && connected;
	}
	
	public void close() {
		if( webSocket != null ) {
			webSocket.close(1000, "programmed standard close() call");
		}
		webSocket = null;
		connected = false;
	}
	
	/*
	 * This method processes each connection event and delegates to appropriate connection operation
	 */
	private void handleConnectionEvent(ConnectionEvent event) throws Exception {

		switch (event.type) {
			/*
			 * 1 - On create of a syndesis connection /metadata/syndesisSource (POST) (arg:
			 * name=’syndesis source name’) ‘Binds’ the syndesisSource - this creates source
			 * in teiid which corresponds to syndesis source.
			 */
			case CREATE: {
			    LOGGER.info("SyndesisConnectionMonitor.handleConnectionEvent(CREATE)");
//				bindSyndesisSource(event.name);
//				refreshSchema(event.name);
			}
				break;
			/*
			 * 2 - Refresh the connection schema (deploys VDB, builds schema)
			 * /metdata/refresh-schema/{connectionName} (POST) (args: redeploy /
			 * generate-schema) This service controls deployment of the ‘connection’ vdb and
			 * regeneration of a schema model in the repo. Ultimately, the schema model is
			 * used to provide the schema info to the UI A) redeploy=true -
			 * deploys/redeploys connection VDB to teiid - ‘[connName]btlconn’ B)
			 * generate-schema=true - pulls metadata from deployed connection VDB - create
			 * repo schema VDB - ‘[connName]schemavdb’.
			 */
			case DELETE: {
				LOGGER.info("SyndesisConnectionMonitor.handleConnectionEvent(DELETE)");
			}
				break;
				
			case UPDATE: {
				LOGGER.info("SyndesisConnectionMonitor.handleConnectionEvent(UPDATE)");
			}
				break;
		}
		
	}
	
	/*
	 * This method checks each applicable syndesis connection and updates all associated syndesisSource vdbs and schema
	 */
	private void synchronizeConnections() {
		
		// get all source connections from syndesis
		
	}
	
	private void addConnection() {

	}
	
	private void removeConnection() {
		
	}
	
	private void replaceConnection() {
		
	}
	
	private void bindSyndesisSource(
		String syndesisSourceName ) throws Exception {

		this.openshiftClient.bindToSyndesisSource(getAuthenticationToken(), syndesisSourceName);
    }
	
	private void refreshSchema(
			String syndesisSourceName) throws Exception {

		UnitOfWork uow = null;

		try {
			uow = createTransaction("refreshSchema", false); //$NON-NLS-1$
			
            // Find the bound teiid source corresponding to the syndesis source
            TeiidDataSource teiidSource = this.kengine.getMetadataInstance().getDataSource(syndesisSourceName);

            if (teiidSource == null)
                throw new KException("Teiid Source not found for Syndnesis source :" + syndesisSourceName);

            TeiidVdb deployedVdb = findDeployedVdb( syndesisSourceName );

            // Initiate the VDB deployment
            
            if ( deployedVdb == null ) {
            	doDeploySourceVdb(uow, teiidSource); // this will delete workspace VDB first
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
            
			uow = createTransaction("generateSchema", false); //$NON-NLS-1$
            
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

            commit(uow);

		} catch (final Exception e) {
			if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
				uow.rollback();
			}
            throw new KException(e);
		}
	}
	
    /**
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @param callback the callback to fire when the transaction is committed
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected UnitOfWork createTransaction(final String name,
                                            final boolean rollbackOnly, final UnitOfWorkListener callback) throws KException {
    	Repository repo = this.kengine.getDefaultRepository();
        final UnitOfWork result = repo.createTransaction( username,
                                                               (getClass().getSimpleName() + COLON + name + COLON + System.currentTimeMillis()),
                                                               rollbackOnly, callback );
         return result;
    }

    /**
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected UnitOfWork createTransaction(final String name,
                                            final boolean rollbackOnly ) throws KException {
    	Repository repo = this.kengine.getDefaultRepository();
        final SynchronousCallback callback = new SynchronousCallback();
        final UnitOfWork result = repo.createTransaction(username,
                                                               (getClass().getSimpleName() + COLON + name + COLON + System.currentTimeMillis()),
                                                               rollbackOnly, callback );
        return result;
    }
    
    protected void commit(UnitOfWork transaction) throws Exception {
        assert( transaction.getCallback() instanceof SynchronousCallback );
        final int timeout = TIMEOUT;
        final TimeUnit unit = UNIT;

        final SynchronousCallback callback = ( SynchronousCallback )transaction.getCallback();
        transaction.commit();

        if ( ! callback.await( timeout, unit ) ) {
            // callback timeout occurred
            String errorMessage = Messages.getString( COMMIT_TIMEOUT, transaction.getName(), timeout, unit );
            throw new KException(errorMessage);
        }

        Throwable error = transaction.getError();
        if ( error != null ) {
            // callback was called because of an error condition
        	throw new KException(error);
        }

        error = callback.error();
        if ( error != null ) {
         // callback was called because of an error condition
        	throw new KException(error);
        }
    }
	
    protected WorkspaceManager getWorkspaceManager(UnitOfWork transaction) throws KException {
    	Repository repo = this.kengine.getDefaultRepository();
        return WorkspaceManager.getInstance(repo, transaction);
    }
    
    private class ConnectionEvent {
    	public EVENT_TYPE type;
    	public String message;
    	public String name;
    	
    	public ConnectionEvent(String message) {
    		super();
    		this.message = message;
    	}
    	
    	private void parseJson() {
    		this.type = EVENT_TYPE.CREATE;
    		
    		this.name = "pgEmployees";
    	}
    }
    
    public static void main(String[] args) throws IOException {
	    String RESERVATIONS_PATH = "ws://syndesis-server/api/v1/event/reservations";
		
		OkHttpClient client = new OkHttpClient();

		String xForwardedUser = "X-Forwarded-User";
		String xForwardedAccessToken = "X-Forwarded-Access-Token";
		String xForwardedToken = "supersecret";
		String xForwardedOrigin = "X-Forwarded-Origin";
		String origin = "for=127.0.0.1;host=localhost:4200;proto=https";
		
		RequestBody dummyRequestBody = RequestBody.create(null, "Dummy Request Body");
		
	    Request request = new Request.Builder()
	    		.url(RESERVATIONS_PATH)
	    		.addHeader(xForwardedUser, "user")
	    		.addHeader(xForwardedAccessToken, xForwardedToken)
	    		.addHeader(xForwardedOrigin, origin)
	    		.post(dummyRequestBody)
	    		.build();
	    LOGGER.info("  *** SyndesisConnectionMonitor.connect() >> Created Request = " + request);
	    if( request.body() != null ) {
	    	LOGGER.info("  *** SyndesisConnectionMonitor.connect() >>          Body = " + request.body().toString());
	    }
	    if( request.headers() != null ) {
	    	LOGGER.info("  *** SyndesisConnectionMonitor.connect() >>          Headers = " + request.headers().toString());
	    }
	    WebSocket wSocket = client.newWebSocket(request, new WebSocketListener() {

		        @Override
		        public void onOpen(WebSocket webSocket, Response response) {
		            LOGGER.info("  *** WebSocketListener.onOpen(): Socket has been opened successfully.");
		            // reset connections attempts counter
		        }
	
		        @Override
		        public void onMessage(WebSocket webSocket, String text) {
		            // print received message in log
		        	LOGGER.info( "  *** WebSocketListener.onMessage(String): New Text Message received "+text);
		        }
	
		        @Override
		        public void onMessage(WebSocket webSocket, ByteString message) {
		        	LOGGER.info( "  *** WebSocketListener.onMessage(ByteString): New ByteString Message received "+message);
		        }
	
		        @Override
		        public void onClosing(WebSocket webSocket, int code, String reason) {
		        	LOGGER.info("  *** WebSocketListener.onClosing(): Close request from server with reason '"+reason+"'");
		            webSocket.close(1000,reason);
		        }
	
		        @Override
		        public void onClosed(WebSocket webSocket, int code, String reason) {
		        	LOGGER.info("  *** WebSocketListener.onClosed(): Socket connection closed with reason '"+reason+"'");
		        }
	
		        @Override
		        public void onFailure(WebSocket webSocket, Throwable t, Response response) {
		        	if( response.headers() != null ) {
		        		LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Response Headers = " + response.headers());
		        	}
		        	if( response.body() != null ) {
		        		LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Response Body = " + response.body());
		        	}
		        	if( response.message() != null ) {
		        		LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Response Message = " + response.message());
		        	}
		            LOGGER.info("  *** WebSocketListener.onFailure():  Socket connection fail.  Error = " + t.getLocalizedMessage());
		        }
		    }
	    );

    }
}
