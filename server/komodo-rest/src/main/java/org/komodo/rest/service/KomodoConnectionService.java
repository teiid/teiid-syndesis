/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.service;

import static org.komodo.rest.relational.RelationalMessages.Error.CONNECTION_SERVICE_NAME_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.CONNECTION_SERVICE_NAME_VALIDATION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.VDB_DATA_SOURCE_NAME_EXISTS;

import java.net.URI;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;

import org.komodo.core.KEngine;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.internal.OptionContainerUtils;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.connection.RestSchemaNode;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestConnectionSummary;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus.EntityState;
import org.komodo.servicecatalog.TeiidOpenShiftClient;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.ServiceCatalogDataSource;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.StringUtils;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining Connection information from the workspace.
 */
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH +
           V1Constants.CONNECTIONS_SEGMENT)
@Api(tags = {V1Constants.CONNECTIONS_SEGMENT})
public final class KomodoConnectionService extends KomodoService {

	private interface OptionalParam {

        /**
         * Indicates if connection schema should be generated if it doesn't exist. Defaults to <code>true</code>.
         */
        String GENERATE_SCHEMA = "generate-schema"; //$NON-NLS-1$

        /**
         * Indicates if schema statuses should be returned. Defaults to <code>false</code>.
         */
        String INCLUDE_SCHEMA_STATUS = "include-schema-status"; //$NON-NLS-1$

        /**
         * Indicates if workspace connection should be included. Defaults to <code>true</code>.
         */
        String INCLUDE_CONNECTION = "include-connection"; //$NON-NLS-1$

        /**
         * Indicates if the connection server VDB should be redeployed if it already exists. Defaults to <code>false</code>.
         */
        String REDEPLOY_CONNECTION = "redeploy"; //$NON-NLS-1$

    }

	private static final int ALL_AVAILABLE = -1;
    private TeiidOpenShiftClient openshiftClient;
    private static final String CONNECTION_VDB_PATTERN = "{0}btlconn"; //$NON-NLS-1$

    private static final String SCHEMA_MODEL_NAME_PATTERN = "{0}schemamodel"; //$NON-NLS-1$
    private static final String SCHEMA_VDB_NAME_PATTERN = "{0}schemavdb"; //$NON-NLS-1$

    /**
     * Time to wait after deploying/undeploying a connection VDB from the metadata instance
     */
    private final static int DEPLOYMENT_WAIT_TIME = 5000;
    
    /**
     * fqn table option key
     */
    private final static String TABLE_OPTION_FQN = "teiid_rel:fqn"; //$NON-NLS-1$

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @param openshiftClient OpenShift Client to access service catalog
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoConnectionService(final KEngine engine, TeiidOpenShiftClient openshiftClient)
            throws WebApplicationException {
        super( engine );
        this.openshiftClient = openshiftClient;
    }

    /**
     * Get connection summaries from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the summaries of the requested connections in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Connection JSON document
     */
    @GET
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Return the summaries of the requested connections",
                  response = RestConnectionSummary[].class)
    @ApiImplicitParams({
    	@ApiImplicitParam(
    			name = OptionalParam.INCLUDE_CONNECTION,
    			value = "Include connections in result.  If not present, connections are returned.",
    			required = false,
    			dataType = "boolean",
    			paramType = "query"),
    	@ApiImplicitParam(
    			name = OptionalParam.INCLUDE_SCHEMA_STATUS,
    			value = "Include statuses in result. If not present, status are not returned.",
    			required = false,
    			dataType = "boolean",
    			paramType = "query"),
    	@ApiImplicitParam(
    			name = QueryParamKeys.PATTERN,
    			value = "A regex expression used when searching. If not present, all objects are returned.",
    			required = false,
    			dataType = "string",
    			paramType = "query"),
    	@ApiImplicitParam(
    			name = QueryParamKeys.SIZE,
    			value = "The number of objects to return. If not present, all objects are returned",
    			required = false,
    			dataType = "integer",
    			paramType = "query"),
    	@ApiImplicitParam(
    			name = QueryParamKeys.START,
    			value = "Index of the first dataservice to return",
    			required = false,
    			dataType = "integer",
    			paramType = "query")
      })
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnections( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        boolean includeSchemaStatus = false;
        boolean includeConnection = true;
        final List< RestConnectionSummary > summaries = new ArrayList<>();

        try {
            { // include-schema-status query parameter
                final String param = uriInfo.getQueryParameters().getFirst( OptionalParam.INCLUDE_SCHEMA_STATUS );

                if ( param != null ) {
                    includeSchemaStatus = Boolean.parseBoolean( param );
                }
            }

            { // include-connection query parameter
                final String param = uriInfo.getQueryParameters().getFirst( OptionalParam.INCLUDE_CONNECTION );

                if ( param != null ) {
                	includeConnection = Boolean.parseBoolean( param );
                }
            }

            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );
            includeConnection = includeConnection || !StringUtils.isBlank( searchPattern ); // assume if search pattern exists that connections should be returned

            // find connections
            final String txId = "getConnections?includeSchemaStatus=" + includeSchemaStatus + "&includeConnection=" + includeConnection; //$NON-NLS-1$ //$NON-NLS-2$
            uow = createTransaction(principal, txId, true );

            final Collection< TeiidVdb > vdbs = includeSchemaStatus ? getMetadataInstance().getVdbs() : null;
            Connection[] connections = null;

            if ( includeConnection ) {	
	            if ( StringUtils.isBlank( searchPattern ) ) {
	                connections = getWorkspaceManager(uow).findConnections( uow );
	                LOGGER.debug( "getConnections:found '{0}' Connections", connections.length ); //$NON-NLS-1$
	            } else {
	                final String[] connectionPaths = getWorkspaceManager(uow).findByType( uow, DataVirtLexicon.Connection.NODE_TYPE, null, searchPattern, false );
	
	                if ( connectionPaths.length == 0 ) {
	                    connections = Connection.NO_CONNECTIONS;
	                } else {
	                    connections = new Connection[ connectionPaths.length ];
	                    int i = 0;
	
	                    for ( final String path : connectionPaths ) {
	                        connections[ i++ ] = getWorkspaceManager(uow).resolve( uow, new ObjectImpl( getWorkspaceManager(uow).getRepository(), path, 0 ), Connection.class );
	                    }
	
	                    LOGGER.debug( "getConnections:found '{0}' Connections using pattern '{1}'", connections.length, searchPattern ); //$NON-NLS-1$
	                }
	            }
	
	            int start = 0;
	
	            { // start query parameter
	                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParamKeys.START );
	
	                if ( qparam != null ) {
	
	                    try {
	                        start = Integer.parseInt( qparam );
	
	                        if ( start < 0 ) {
	                            start = 0;
	                        }
	                    } catch ( final Exception e ) {
	                        start = 0;
	                    }
	                }
	            }
	
	            int size = ALL_AVAILABLE;
	
	            { // size query parameter
	                final String qparam = uriInfo.getQueryParameters().getFirst( QueryParamKeys.SIZE );
	
	                if ( qparam != null ) {
	
	                    try {
	                        size = Integer.parseInt( qparam );
	
	                        if ( size <= 0 ) {
	                            size = ALL_AVAILABLE;
	                        }
	                    } catch ( final Exception e ) {
	                        size = ALL_AVAILABLE;
	                    }
	                }
	            }
	
	            int i = 0;
	
	            KomodoProperties properties = new KomodoProperties();
	            for ( final Connection connection : connections ) {
	                if ( ( start == 0 ) || ( i >= start ) ) {
	                	RestConnection restConnection = null;
	                	RestMetadataConnectionStatus restStatus = null;

	                	if ( ( size == ALL_AVAILABLE ) || ( summaries.size() < size ) ) {                        
	                        restConnection = entityFactory.create(connection, uriInfo.getBaseUri(), uow, properties);
	                        LOGGER.debug("getConnections:Connection '{0}' entity was constructed", connection.getName(uow)); //$NON-NLS-1$

	                        if ( includeSchemaStatus ) {
	                           	restStatus = createStatusRestEntity( uow, vdbs, connection );
	                        }

	                        summaries.add( new RestConnectionSummary( uriInfo.getBaseUri(), restConnection, restStatus ) );
	                	} else {
	                        break;
	                    }
	                }
	
	                ++i;
	            }
            } else if ( includeSchemaStatus ) { // include schema status and no connections
            	connections = getWorkspaceManager(uow).findConnections( uow );
                
                for ( final Connection connection: connections ) {
                    final RestMetadataConnectionStatus restStatus = createStatusRestEntity( uow, vdbs, connection );
                    summaries.add( new RestConnectionSummary( uriInfo.getBaseUri(), null, restStatus ) );
                }
            }

            return commit( uow, mediaTypes, summaries );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_GET_CONNECTIONS_ERROR);
        }
    }

    private TeiidVdb findDeployedVdb( final String connectionName ) throws KException {
        final String connectionVdbName = getConnectionWorkspaceVdbName( connectionName );
        return getMetadataInstance().getVdb( connectionVdbName );
    }

    private Vdb findSchemaVdb( final UnitOfWork uow,
                               final Connection connection ) throws KException {
        final WorkspaceManager wkspMgr = getWorkspaceManager( uow );
        final String connectionName = connection.getName( uow );

        final String schemaVdbName = getSchemaVdbName( connectionName );
        final KomodoObject[] vdbs = connection.getChildrenOfType( uow,
                                                                  VdbLexicon.Vdb.VIRTUAL_DATABASE,
                                                                  schemaVdbName );

        if ( vdbs.length == 0 ) {
            return null;
        }
        
        return wkspMgr.resolve( uow, vdbs[ 0 ], Vdb.class );
    }

    private Model findSchemaModel( final UnitOfWork uow,
                                   final Connection connection ) throws KException {
        final Vdb vdb = findSchemaVdb( uow, connection );

        if ( vdb != null ) {
            final String connectionName = connection.getName( uow );
            final String schemaModelName = getSchemaModelName( connectionName );
            final Model[] models = vdb.getModels( uow, schemaModelName );

            if ( models.length != 0 ) {
                return models[ 0 ];
            }
        }

        return null;
    }

    private Vdb findConnectionWorkspaceVdb( final UnitOfWork uow,
    		                                final Connection connection ) throws KException {
    	final WorkspaceManager wkspMgr = getWorkspaceManager( uow );
    	final String connectionName = connection.getName( uow );

    	final String wsConnectionVdbName = this.getConnectionWorkspaceVdbName(connectionName);
    	final KomodoObject vdb = wkspMgr.getChild(uow, 
    			                                  wsConnectionVdbName, 
    			                                  VdbLexicon.Vdb.VIRTUAL_DATABASE);

    	if(vdb == null) return null;

    	return wkspMgr.resolve( uow, vdb, Vdb.class );
    }
    
    private ModelSource findConnectionWorkspaceVdbModelSource( final UnitOfWork uow,
    		                                                   final Connection connection ) throws KException {
        ModelSource modelSource = null;
        
    	final Vdb vdb = findConnectionWorkspaceVdb( uow, connection );

    	if ( vdb != null ) {
    		final String connectionName = connection.getName( uow );
    		final String schemaModelName = getSchemaModelName( connectionName );
    		final Model[] models = vdb.getModels(uow, schemaModelName);

    		Model model = null;
    		if ( models.length != 0 ) {
    			model = models[ 0 ];
    		}
    		
    		if( model != null ) {
        		final String schemaModelSourceName = getSchemaModelSourceName( uow, connection );
    			final ModelSource[] modelSources = model.getSources(uow, schemaModelSourceName);
    			if ( modelSources.length != 0 ) {
    				modelSource = modelSources[ 0 ];
    			}
    		}
    	}

    	return modelSource;
    }

    private String getConnectionWorkspaceVdbName( final String connectionName ) {
        return MessageFormat.format( CONNECTION_VDB_PATTERN, connectionName.toLowerCase() );
    }

    private String getSchemaModelName( final String connectionName ) {
        return MessageFormat.format( SCHEMA_MODEL_NAME_PATTERN, connectionName.toLowerCase() );
    }

    private String getSchemaModelSourceName( final UnitOfWork uow, final Connection connection ) throws KException {
    	String svcCatalogSourceName = null;
    	if(connection.hasProperty(uow, "serviceCatalogSource")) {
            svcCatalogSourceName = connection.getProperty(uow, "serviceCatalogSource").getStringValue(uow);
    	}
    	String schemaModelSourceName = svcCatalogSourceName != null ? svcCatalogSourceName : connection.getId(uow).toLowerCase();
    	return schemaModelSourceName;
    }

    private String getSchemaVdbName( final String connectionName ) {
        return MessageFormat.format( SCHEMA_VDB_NAME_PATTERN, connectionName.toLowerCase() );
    }

    private RestMetadataConnectionStatus createStatusRestEntity( final UnitOfWork uow,
                                                                 final Collection< TeiidVdb > vdbs,
                                                                 final Connection connection ) throws Exception {
        final String connectionName = connection.getName( uow );
        final String connVdbName = getConnectionWorkspaceVdbName( connectionName );
        
        // find status of server connection VDB
        TeiidVdb connVdb = null;

        for ( final TeiidVdb vdb : vdbs ) {
            if ( vdb.getName().equals( connVdbName ) ) {
                connVdb = vdb;
                break;
            }
        }

        if ( connVdb == null ) {
            return new RestMetadataConnectionStatus( connectionName );
        }

        // now find status of workspace schema
        final RestMetadataConnectionStatus restStatus = new RestMetadataConnectionStatus( connectionName, connVdb );
        final WorkspaceManager wkspMgr = getWorkspaceManager( uow );

        // the schema VDB is a child of the connection
        final String schemaVdbName = getSchemaVdbName( connectionName );
        final KomodoObject[] workspaceVdbs = connection.getChildrenOfType( uow,
                                                                           VdbLexicon.Vdb.VIRTUAL_DATABASE,
                                                                           schemaVdbName );

        if ( workspaceVdbs.length != 0 ) {
            final Vdb vdb = wkspMgr.resolve( uow, workspaceVdbs[ 0 ], Vdb.class );
            restStatus.setSchemaVdbName( schemaVdbName );

            // there should be one model
            final String schemaModelName = getSchemaModelName( connectionName );
            final Model[] models = vdb.getModels( uow, schemaModelName );

            if ( models.length > 0 ) {
                final Model schemaModel = models[ 0 ];
                restStatus.setSchemaModelName( schemaModelName );

                // if model has children the DDL has been sequenced
                if ( schemaModel.hasChildren( uow ) ) {
                    // assume sequencer ran successfully
                    restStatus.setSchemaState( EntityState.ACTIVE );
                } else if ( schemaModel.hasProperty( uow, VdbLexicon.Model.MODEL_DEFINITION ) ) {
                    // assume sequencer is running but could have failed
                    restStatus.setSchemaState( EntityState.LOADING );
                }
            } else {
                // Since VDB and model are created in the same transaction this should never happen.
                // Would be nice to be able to get here if we can detect the DDL sequencing failed.
                restStatus.setSchemaState( EntityState.FAILED );
            }
        }

        return restStatus;
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the name of the connection whose tables are being requested (cannot be empty)
     * @return the JSON representation of the tables collection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace connection or constructing the JSON representation
     */
    @GET
    @Path( "{connectionName}/schema" )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation( value = "Get the native schema for the connection",
                   response = RestSchemaNode[].class )
    @ApiResponses( value = {
        @ApiResponse( code = 403, message = "An error has occurred." ),
        @ApiResponse( code = 404, message = "No connection could be found with the specified name" ),
        @ApiResponse( code = 406, message = "Only JSON is returned by this operation" )
    } )
    public Response getSchema( @Context final HttpHeaders headers,
                               final @Context UriInfo uriInfo,
                               @ApiParam( value = "Name of the connection",
                                          required = true )
                               @PathParam( "connectionName" )
                               final String connectionName ) throws KomodoRestException {
        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final List< MediaType > mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "getSchema?connectionName=" + connectionName, true ); //$NON-NLS-1$
            final Connection connection = findConnection( uow, connectionName );

            // connection not found so return 404 error response
            if ( connection == null ) {
                return commitNoConnectionFound( uow, mediaTypes, connectionName );
            }

            final Model schemaModel = findSchemaModel( uow, connection );

            List<RestSchemaNode> schemaNodes = Collections.emptyList();
            if ( schemaModel != null ) {
                final Table[] tables = schemaModel.getTables( uow );
                
                schemaNodes = this.generateConnectionSchema(uow, connectionName, tables);
            }

            return commit( uow, mediaTypes, schemaNodes ); 
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden( mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_GET_TABLES_ERROR );
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the id of the connection whose summary is being retrieved (cannot be empty)
     * @return a JSON document representing the summary of the requested connection in the Komodo workspace (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace Connection or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find connection by name", response = RestConnectionSummary.class)
    @ApiImplicitParams({
    	@ApiImplicitParam(
    			name = OptionalParam.INCLUDE_CONNECTION,
    			value = "Include connections in result.  If not present, connections are returned.",
    			required = false,
    			dataType = "boolean",
    			paramType = "query"),
    	@ApiImplicitParam(
    			name = OptionalParam.INCLUDE_SCHEMA_STATUS,
    			value = "Include statuses in result. If not present, status are not returned.",
    			required = false,
    			dataType = "boolean",
    			paramType = "query"),
      })
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnection( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "Name of the connection",
                                             required = true
                                   )
                                   final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        boolean includeSchemaStatus = false;
        boolean includeConnection = true;

        try {
            { // include-schema-status query parameter
                final String param = uriInfo.getQueryParameters().getFirst( OptionalParam.INCLUDE_SCHEMA_STATUS );

                if ( param != null ) {
                    includeSchemaStatus = Boolean.parseBoolean( param );
                }
            }

            { // include-connection query parameter
                final String param = uriInfo.getQueryParameters().getFirst( OptionalParam.INCLUDE_CONNECTION );

                if ( param != null ) {
                	includeConnection = Boolean.parseBoolean( param );
                }
            }

            final String txId = "getConnection?includeSchemaStatus=" + includeSchemaStatus + "&includeConnection=" + includeConnection; //$NON-NLS-1$ //$NON-NLS-2$
            uow = createTransaction(principal, txId, true ); //$NON-NLS-1$

            Connection connection = findConnection(uow, connectionName);
            if (connection == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

        	RestConnection restConnection = null;
        	RestMetadataConnectionStatus restStatus = null;

        	if ( includeConnection ) {
	        	KomodoProperties properties = new KomodoProperties();
	            restConnection = entityFactory.create(connection, uriInfo.getBaseUri(), uow, properties);
	            LOGGER.debug("getConnection:Connection '{0}' entity was constructed", connection.getName(uow)); //$NON-NLS-1$
        	}

        	if ( includeSchemaStatus ) {
        		restStatus = createStatusRestEntity( uow, getMetadataInstance().getVdbs(), connection );
	            LOGGER.debug("getConnection:Connection '{0}' status entity was constructed", connection.getName(uow)); //$NON-NLS-1$
        	}

        	final RestConnectionSummary summary = new RestConnectionSummary( uriInfo.getBaseUri(), restConnection, restStatus );
        	return commit( uow, mediaTypes, summary );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_GET_CONNECTION_ERROR, connectionName);
        }
    }
    
    /**
     * Create a new Connection in the komodo repository, using a service catalogSource
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the connection name (cannot be empty)
     * @param connectionJson
     *        the connection JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the Connection
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Create a connection in the workspace, using ServiceCatalogSource")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response createConnection( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo,
                                      @ApiParam(
                                              value = "Name of the connection",
                                              required = true
                                      )
                                      final @PathParam( "connectionName" ) String connectionName,
                                      @ApiParam(
                                              value = "" + 
                                                      "Properties for the new connection:<br>" +
                                                      OPEN_PRE_TAG +
                                                      OPEN_BRACE + BR +
                                                      NBSP + "description: \"description for the connection\"" + COMMA + BR +
                                                      NBSP + "serviceCatalogSource: \"serviceCatalog source for the connection\"" + BR +
                                                      CLOSE_BRACE +
                                                      CLOSE_PRE_TAG,
                                              required = true
                                      )
                                      final String connectionJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the connection name is missing
        if (StringUtils.isBlank( connectionName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_MISSING_CONNECTION_NAME);
        }

        // Get the attributes - ensure valid attributes provided
        KomodoConnectionAttributes rcAttr;
        try {
        	rcAttr = KomodoJsonMarshaller.unmarshall(connectionJson, KomodoConnectionAttributes.class);
            
            Response response = checkConnectionAttributes(rcAttr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.CONNECTION_SERVICE_REQUEST_PARSING_ERROR);
        }
        
        ServiceCatalogDataSource serviceCatalogSource = null;
        
        RestConnection restConnection = new RestConnection();
        restConnection.setId(connectionName);

        try {
            // Add properties for the description and serviceCatalogSource
            restConnection.addProperty("description", rcAttr.getDescription());
            restConnection.addProperty(DataVirtLexicon.Connection.SERVICE_CATALOG_SOURCE, rcAttr.getServiceCatalogSource());
            restConnection.setJdbc(true);
            
            // Get the specified ServiceCatalogDataSource from the metadata instance
            Collection<ServiceCatalogDataSource> dataSources = openshiftClient.getServiceCatalogSources(getAuthenticationToken());
			for(ServiceCatalogDataSource ds: dataSources) {
				if(ds.getName().equals(rcAttr.getServiceCatalogSource())) {
					serviceCatalogSource = ds;
					break;
				}
			}
			// If catalogSource is not found, exit with error
			if (serviceCatalogSource == null) {
				return createErrorResponseWithForbidden(mediaTypes,
						RelationalMessages.Error.CONNECTION_SERVICE_CATALOG_SOURCE_DNE_ERROR);
			}
        } catch (Exception ex) {
            throw new KomodoRestException(ex);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "createConnection", false ); //$NON-NLS-1$

            // Error if the repo already contains a connection with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, connectionName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CREATE_ALREADY_EXISTS);
            }

			// Ensures service catalog is bound, and creates the corresponding datasource in wildfly
			openshiftClient.bindToServiceCatalogSource(getAuthenticationToken(), serviceCatalogSource.getName());
			
			// Get the connection from the wildfly instance (should be available after binding)
            TeiidDataSource dataSource = getMetadataInstance().getDataSource(serviceCatalogSource.getName());
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);
			
            // Add the jndi and driver to the komodo connection to be created
            restConnection.setJndiName(dataSource.getJndiName());
            restConnection.setDriverName(dataSource.getType());

            // create new Connection
            return doAddConnection( uow, uriInfo.getBaseUri(), mediaTypes, restConnection );

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_CREATE_CONNECTION_ERROR, connectionName);
        }
    }

    /**
     * Clone a Connection in the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the connection name (cannot be empty)
     * @param newConnectionName
     *        the new connection name (cannot be empty)
     * @return a JSON representation of the new connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error creating the Connection
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CLONE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Clone a connection in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response cloneConnection( final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam(
                                               value = "Name of the connection",
                                               required = true
                                     )
                                     final @PathParam( "connectionName" ) String connectionName,
                                     @ApiParam(
                                               value = "The new name of the connection",
                                               required = true
                                     )
                                     final String newConnectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the connection name is missing
        if (StringUtils.isBlank( connectionName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_MISSING_CONNECTION_NAME);
        }

        // Error if the new connection name is missing
        if ( StringUtils.isBlank( newConnectionName ) ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_MISSING_NEW_NAME);
        }

        // Error if the name parameter and new name are the same
        final boolean namesMatch = connectionName.equals( newConnectionName );
        if ( namesMatch ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_SAME_NAME_ERROR, newConnectionName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "cloneConnection", false ); //$NON-NLS-1$

            // Error if the repo already contains a connection with the supplied name.
            if ( getWorkspaceManager(uow).hasChild( uow, newConnectionName ) ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_ALREADY_EXISTS);
            }

            // create new Connection
            // must be an update
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
            final Connection oldConnection = getWorkspaceManager(uow).resolve( uow, kobject, Connection.class );
            final RestConnection oldEntity = entityFactory.create(oldConnection, uriInfo.getBaseUri(), uow );
            
            final Connection connection = getWorkspaceManager(uow).createConnection( uow, null, newConnectionName);

            setProperties( uow, connection, oldEntity );

            final RestConnection entity = entityFactory.create(connection, uriInfo.getBaseUri(), uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_CLONE_CONNECTION_ERROR, connectionName);
        }
    }

    /**
     * Update a Connection in the komodo repository, using service catalog source
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the connection name (cannot be empty)
     * @param connectionJson
     *        the connection JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the updated connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error updating the VDB
     */
    @PUT
    @Path( StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Update a connection in the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response updateConnection( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo,
                                      @ApiParam(
                                                value = "Name of the connection",
                                                required = true
                                      )
                                      final @PathParam( "connectionName" ) String connectionName,
                                      @ApiParam(
                                                value = "" + 
                                                        "Properties for the connection update:<br>" +
                                                        OPEN_PRE_TAG +
                                                        OPEN_BRACE + BR +
                                                        NBSP + "description: \"description for the connection\"" + COMMA + BR +
                                                        NBSP + "serviceCatalogSource: \"serviceCatalog source for the connection\"" + BR +
                                                        CLOSE_BRACE +
                                                        CLOSE_PRE_TAG,
                                                required = true
                                      )
                                      final String connectionJson) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the connection name is missing
        if (StringUtils.isBlank( connectionName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_MISSING_CONNECTION_NAME);
        }
        
        // Get the attributes - ensure valid attributes provided
        KomodoConnectionAttributes rcAttr;
        try {
        	rcAttr = KomodoJsonMarshaller.unmarshall(connectionJson, KomodoConnectionAttributes.class);
            
            Response response = checkConnectionAttributes(rcAttr, mediaTypes);
            if (response.getStatus() != Status.OK.getStatusCode())
                return response;

        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.CONNECTION_SERVICE_REQUEST_PARSING_ERROR);
        }

        ServiceCatalogDataSource serviceCatalogSource = null;

        RestConnection restConnection = new RestConnection();
        restConnection.setId(connectionName);

        try {
            // Add properties for the description and serviceCatalogSource
            restConnection.addProperty("description", rcAttr.getDescription());
            restConnection.addProperty(DataVirtLexicon.Connection.SERVICE_CATALOG_SOURCE, rcAttr.getServiceCatalogSource());
            restConnection.setJdbc(true);
            
            // Get the specified ServiceCatalogDataSource from the metadata instance
            Collection<ServiceCatalogDataSource> dataSources = openshiftClient.getServiceCatalogSources(getAuthenticationToken());
			for(ServiceCatalogDataSource ds: dataSources) {
				if(ds.getName().equals(rcAttr.getServiceCatalogSource())) {
					serviceCatalogSource = ds;
					break;
				}
			}
			// If catalogSource is not found, exit with error
			if(serviceCatalogSource == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_CATALOG_SOURCE_DNE_ERROR);
			}
        } catch (Exception ex) {
            throw new KomodoRestException(ex);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "updateConnection", false ); //$NON-NLS-1$

            final boolean exists = getWorkspaceManager(uow).hasChild( uow, connectionName );
            // Error if the specified connection does not exist
            if ( !exists ) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_UPDATE_CONNECTION_DNE);
            }

            // Update deletes the existing connection and recreates it.
            final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
            getWorkspaceManager(uow).delete(uow, kobject);

			// Ensures service catalog is bound, and creates the corresponding datasource in wildfly
			openshiftClient.bindToServiceCatalogSource(getAuthenticationToken(), serviceCatalogSource.getName());
			
			// Get the connection from the wildfly instance (should be available after binding)
            TeiidDataSource dataSource = getMetadataInstance().getDataSource(serviceCatalogSource.getName());
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);
			
            // Add the jndi and driver to the komodo connection to be created
            restConnection.setJndiName(dataSource.getJndiName());
            restConnection.setDriverName(dataSource.getType());

            // Create the connection
            Response response = doAddConnection( uow, uriInfo.getBaseUri(), mediaTypes, restConnection );

            LOGGER.debug("updateConnection: connection '{0}' entity was updated", connectionName); //$NON-NLS-1$

            return response;
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_UPDATE_CONNECTION_ERROR, connectionName);
        }
    }

    private Response doAddConnection( final UnitOfWork uow,
                                      final URI baseUri,
                                      final List<MediaType> mediaTypes,
                                      final RestConnection restConnection ) throws KomodoRestException {
        assert( !uow.isRollbackOnly() );
        assert( uow.getState() == State.NOT_STARTED );
        assert( restConnection != null );

        final String connectionName = restConnection.getId();
        try {
            final Connection connection = getWorkspaceManager(uow).createConnection( uow, null, connectionName);

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, connection, restConnection);

            final RestConnection entity = entityFactory.create(connection, baseUri, uow );
            final Response response = commit( uow, mediaTypes, entity );
            return response;
        } catch ( final Exception e ) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            throw new KomodoRestException( RelationalMessages.getString( RelationalMessages.Error.CONNECTION_SERVICE_CREATE_CONNECTION_ERROR, connectionName ), e );
        }
    }

    /**
     * Delete the specified Connection from the komodo repository
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the name of the connection to remove (cannot be <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *         if there is a problem performing the delete
     */
    @DELETE
    @Path("{connectionName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete a connection from the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteConnection( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo,
                                      @ApiParam(
                                                value = "Name of the connection",
                                                required = true
                                      )
                                      final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeConnectionFromWorkspace", false); //$NON-NLS-1$
            Repository repo = this.kengine.getDefaultRepository();
            final WorkspaceManager mgr = WorkspaceManager.getInstance( repo, uow );
            Connection connection = findConnection(uow, connectionName);

            if (connection == null)
                return Response.noContent().build();

            // get associated workspace vdb - remove if it exists
            final String connectionVdbName = getConnectionWorkspaceVdbName( connectionName );
            final Vdb connectionWorkspaceVdb = findVdb( uow, connectionVdbName );
            if (connectionWorkspaceVdb != null) {
                mgr.delete(uow, connectionWorkspaceVdb);
            }
            
            // get associated deployed vdb - undeploy if it exists
            final TeiidVdb deployedVdb = findDeployedVdb( connectionName );
            if (deployedVdb != null) {
                getMetadataInstance().undeployDynamicVdb(connectionVdbName);
            }
            
            // Delete the workspace connection
            mgr.delete(uow, connection);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            if (mgr.hasChild(uow, connectionName))
                kso.addAttribute(connectionName, "Deletion failure"); //$NON-NLS-1$
            else
                kso.addAttribute(connectionName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_DELETE_CONNECTION_ERROR, connectionName);
        }
    }

    /**
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param connectionName
     *            the Connection name being validated (cannot be empty)
     * @return the response (never <code>null</code>) with an entity that is
     *         either an empty string, when the name is valid, or an error
     *         message
     * @throws KomodoRestException
     *             if there is a problem validating the name or constructing
     *             the response
     */
    @GET
    @Path( V1Constants.NAME_VALIDATION_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( { MediaType.TEXT_PLAIN } )
    @ApiOperation( value = "Returns an error message if the Connection name is invalid" )
    @ApiResponses( value = {
            @ApiResponse( code = 400, message = "The URI cannot contain encoded slashes or backslashes." ),
            @ApiResponse( code = 403, message = "An unexpected error has occurred." ),
            @ApiResponse( code = 500, message = "The Connection name cannot be empty." )
    } )
    public Response validateConnectionName( final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam( value = "The Connection name being checked", required = true )
                                     final @PathParam( "connectionName" ) String connectionName ) throws KomodoRestException {

        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final String errorMsg = VALIDATOR.checkValidName( connectionName );
        
        // a name validation error occurred
        if ( errorMsg != null ) {
            return Response.ok().entity( errorMsg ).build();
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "validateConnectionName", true ); //$NON-NLS-1$

            // make sure an existing Connection does not have that name
            final Connection connection = findConnection( uow, connectionName );

            if ( connection == null ) {
                // make sure an existing vdb does not have the same name
                final Vdb ds = findVdb( uow, connectionName );

                if ( ds == null ) {
                    // name is valid
                    return Response.ok().build();
                }

                // name is the same as an existing connection
                return Response.ok()
                               .entity( RelationalMessages.getString( VDB_DATA_SOURCE_NAME_EXISTS ) )
                               .build();
            }

            // name is the same as an existing connection
            return Response.ok()
                           .entity( RelationalMessages.getString( CONNECTION_SERVICE_NAME_EXISTS ) )
                           .build();
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden( headers.getAcceptableMediaTypes(), 
                                                     e, 
                                                     CONNECTION_SERVICE_NAME_VALIDATION_ERROR );
        }
    }
    
    /**
     * Initiate schema refresh for a Connection
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the connection name (cannot be empty)
     * @return a JSON representation of the refresh status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error initiating a connection schema refresh
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.REFRESH_SCHEMA_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Initiate schema refresh for a workspace connection")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response refreshConnection( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam( value = "Name of the connection",
                                                  required = true )
                                       final @PathParam( "connectionName" ) String connectionName,
                                       @ApiParam( value = "Indicates the connection VDB should be redeployed if it already exists",
                                                  required = false )
                                       @DefaultValue( "false" )
                                       @QueryParam( OptionalParam.REDEPLOY_CONNECTION )
                                       final boolean redeployServerVdb,
                                       @ApiParam( value = "Indicates the workspace schema model should be generated if it doesn't exist",
                                                  required = false )
                                       @DefaultValue( "true" )
                                       @QueryParam( OptionalParam.GENERATE_SCHEMA )
                                       final boolean generateSchema ) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the connection name is missing
        if (StringUtils.isBlank( connectionName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_MISSING_CONNECTION_NAME);
        }

        UnitOfWork uow = null;

        try {
            final String txId = "refreshConnection?redeploy=" + redeployServerVdb + "&generate-schema=" + generateSchema; 
            uow = createTransaction(principal, txId, false );

            // Find the requested connection
            Connection connection = findConnection(uow, connectionName);
            if (connection == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            final KomodoStatusObject kso = new KomodoStatusObject( "Refresh connection" );
            final TeiidVdb deployedVdb = findDeployedVdb( connectionName );
            boolean doDeploy = false;

            if ( deployedVdb == null ) {
                doDeploy = true;
            } else {
                doDeploy = redeployServerVdb;
            }

            // Initiate the VDB deployment
            if ( doDeploy ) {
                doDeployConnectionVdb(uow, connection); // this will delete workspace VDB first
                kso.addAttribute(connectionName, "Delete workspace VDB, recreate, and redeploy");
            } else if ( generateSchema ) {
                Vdb schemaVdb = findSchemaVdb( uow, connection );
                final String schemaModelName = getSchemaModelName( connectionName );
                Model schemaModel = null;

                // create if necessary
                if ( schemaVdb == null ) {
                    final WorkspaceManager wkspMgr = getWorkspaceManager( uow );
                    final String schemaVdbName = getSchemaVdbName( connectionName );
                    schemaVdb = wkspMgr.createVdb( uow, connection, schemaVdbName, schemaVdbName );

                    // Add schema model to schema vdb
                    schemaModel = addModelToSchemaVdb(uow, schemaVdb, connection, schemaModelName);
                } else {
                    final Model[] models = schemaVdb.getModels( uow, schemaModelName );

                    if ( models.length != 0 ) {
                        schemaModel = models[ 0 ];
                    } else {
                        // should never happen but just in case
                        schemaModel = addModelToSchemaVdb(uow, schemaVdb, connection, schemaModelName);
                    }
                }

                final String modelDdl = getMetadataInstance().getSchema( deployedVdb.getName(), "1", schemaModelName );
                schemaModel.setModelDefinition( uow, modelDdl );
                kso.addAttribute(connectionName, "Generate schema");
                // after transaction is committed this will trigger the DDL sequencer which will create
                // the model objects.
            } else {
                kso.addAttribute( connectionName, "Neither redeploy or generate schema requested" );
            }

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_SERVICE_REFRESH_SCHEMA_ERROR);
        }
    }

    /**
     * Deploy / re-deploy a VDB to the metadata instance for the provided workspace connection.
     * @param uow the transaction
     * @param connection the connection
     * @return the DeployStatus from deploying the VDB
     * @throws KException
     * @throws InterruptedException
     */
    private DeployStatus doDeployConnectionVdb( final UnitOfWork uow,
    		                                    Connection connection ) throws KException, InterruptedException {
    	assert( uow.getState() == State.NOT_STARTED );
    	assert( connection != null );

    	// Get necessary info from the connection
    	String connectionName = connection.getName(uow);
        String jndiName = connection.getJndiName(uow);
        String driverName = connection.getDriverName(uow);
        
        // Name of VDB to be created is based on the connection name
        String vdbName = getConnectionWorkspaceVdbName( connectionName );
        
        // VDB is created in the repository.  If it already exists, delete it
        Repository repo = this.kengine.getDefaultRepository();
        final WorkspaceManager mgr = WorkspaceManager.getInstance( repo, uow );
        String repoPath = repo.komodoWorkspace( uow ).getAbsolutePath();
        
        final Vdb existingVdb = findVdb( uow, vdbName );

        if ( existingVdb != null ) {
            mgr.delete(uow, existingVdb);
        }
        
        // delete schema VDB if it exists
        final Vdb schemaVdb = findSchemaVdb( uow, connection );

        if ( schemaVdb != null ) {
            mgr.delete( uow, schemaVdb );
        }

        // Create new VDB
        String vdbPath = repoPath + "/" + vdbName;
        final Vdb vdb = mgr.createVdb( uow, null, vdbName, vdbPath );
        vdb.setDescription(uow, "Vdb for connection "+connectionName);
                    
        // Add model to the VDB
        Model model = vdb.addModel(uow, getSchemaModelName(connectionName));
        model.setModelType(uow, Model.Type.PHYSICAL);
        model.setProperty(uow, "importer.TableTypes", "TABLE,VIEW");
        model.setProperty(uow, "importer.UseQualifiedName", "true");
        model.setProperty(uow, "importer.UseCatalogName", "false");
        model.setProperty(uow, "importer.UseFullSchemaName", "false");
        
        // Add model source to the model
        final String modelSourceName = getSchemaModelSourceName(uow, connection);
        ModelSource modelSource = model.addSource(uow, modelSourceName);
        modelSource.setJndiName(uow, jndiName);
        modelSource.setTranslatorName(uow, driverName);
        modelSource.setAssociatedConnection(uow, connection);
        
        // Deploy the VDB
        DeployStatus deployStatus = vdb.deploy(uow);
        
        // Wait for deployment to complete
        Thread.sleep(DEPLOYMENT_WAIT_TIME);
        
        return deployStatus;
    }

    /**
     * Add model to the schema vdb
     * @param uow the transaction
     * @param schemaVdb the schema VDB
     * @param connection the connection
     * @param schemaModelName the name for the schema model being created
     * @return the created schema model
     * @throws KException
     */
    private Model addModelToSchemaVdb(final UnitOfWork uow, final Vdb schemaVdb, final Connection connection, final String schemaModelName) throws KException {
        // create schema model
        Model schemaModel = schemaVdb.addModel( uow, schemaModelName );
        
        // Make a copy of the workspace connection vdb model source under the connection schema vdb model
        final ModelSource workspaceVdbModelSource = findConnectionWorkspaceVdbModelSource( uow, connection );
        if( workspaceVdbModelSource != null ) {
        	ModelSource mdlSource = schemaModel.addSource(uow, workspaceVdbModelSource.getName(uow));
        	mdlSource.setJndiName(uow, workspaceVdbModelSource.getJndiName(uow));
        	mdlSource.setTranslatorName(uow, workspaceVdbModelSource.getTranslatorName(uow));
        	mdlSource.setAssociatedConnection(uow, workspaceVdbModelSource.getOriginConnection(uow));
        }
        
        return schemaModel;
    }
    
    private synchronized MetadataInstance getMetadataInstance() throws KException {
        return this.kengine.getMetadataInstance();
    }
    
    /*
     * Checks the supplied attributes for create and update of connections
     *  - serviceCatalogSource is required
     *  - description is optional
     */
    private Response checkConnectionAttributes(KomodoConnectionAttributes attr,
                                               List<MediaType> mediaTypes) throws Exception {

        if ( attr == null || attr.getServiceCatalogSource() == null ) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_MISSING_PARAMETER_ERROR);
        }

        return Response.ok().build();
    }

    /**
     * Generate the Connection Schema structure using the supplied table fqn information.
     * @param uow the transaction
     * @param connectionName the name of the connection
     * @param tables the supplied array of tables
     * @return the list of schema nodes
     * @throws KException exception if problem occurs
     */
    private List<RestSchemaNode> generateConnectionSchema(final UnitOfWork uow, final String connectionName, final Table[] tables) throws KException {
    	List<RestSchemaNode> schemaNodes = new ArrayList<RestSchemaNode>();

    	for(final Table table : tables) {
    		// Use the fqb table option do determine native structure
            final String option = OptionContainerUtils.getOption( uow, table, TABLE_OPTION_FQN );
            if( option != null ) {
            	// Break fqn into segments (segment starts at root, eg "schema=public/table=customer")
                String[] segments = option.split(FORWARD_SLASH);
                // Get the parent node of the final segment in the 'path'.  New nodes are created if needed.
                RestSchemaNode parentNode = getLeafNodeParent(connectionName, schemaNodes, segments);

                // Use last segment to create the leaf node child in the parent.  If parent is null, was root (and leaf already created).
                if( parentNode != null ) {
                	String type = getSegmentType(segments[segments.length-1]);
                	String name = getSegmentName(segments[segments.length-1]);
                	RestSchemaNode node = new RestSchemaNode(connectionName, name, type);
                	node.setQueryable(true);
                	parentNode.addChild(node);
                }
            }
    	}
    	
    	return schemaNodes;
    }

    /**
     * Get the RestSchemaNode immediately above the last path segment (leaf parent).  If the parent nodes do not already exist,
     * they are created and added to the currentNodes.  The returned List is a list of the root nodes.  The root node children,
     * children's children, etc, are built out according to the path segments.
     * @param connectionName the connection name
     * @param currentNodes the current node list
     * @param segments the full path of segments, starting at the root
     * @return the final segments parent node.  (null if final segment is at the root)
     */
    private RestSchemaNode getLeafNodeParent(String connectionName, List<RestSchemaNode> currentNodes, String[] segments) {
    	RestSchemaNode parentNode = null;
    	// Determine number of levels to process.
    	// - process one level if one segment
    	// - if more than one level, process nSegment - 1 levels
    	int nLevels = (segments.length > 1) ? segments.length-1 : 1;

    	// Start at beginning of segment path, creating nodes if necessary
    	for( int i=0; i < nLevels; i++ ) {
        	String type = getSegmentType(segments[i]);
        	String name = getSegmentName(segments[i]);
        	// Root Level - look for matching root node in the list 
    		if( i == 0 ) {
    			RestSchemaNode matchNode = getMatchingNode(connectionName, name, type, currentNodes.toArray( new RestSchemaNode[ currentNodes.size() ] ));
    			// No match - create a new node
    			if(matchNode == null) {
    				matchNode = new RestSchemaNode(connectionName, name, type);
    				currentNodes.add(matchNode);
    			}
    		    // Set parent for next iteration
    			if( segments.length == 1 ) {       // Only one segment - parent is null (root)
    				matchNode.setQueryable(true);
    				parentNode = null;
    			} else {
    				// Set next parent if not last level
    				if( i != segments.length-1 ) { 
    					parentNode = matchNode;
    				}
    			}
    		// Not at root - look for matching node in parents children
    		} else {
    			RestSchemaNode matchNode = getMatchingNode(connectionName, name, type, parentNode.getChildren());
    			// No match - create a new node
    			if(matchNode == null) {
    				matchNode = new RestSchemaNode(connectionName, name, type);
    				parentNode.addChild(matchNode);
    			}
    			// Set next parent if not last level
    			if( i != segments.length-1 ) {
    				parentNode = matchNode;
    			}
    		}
    	}
    	return parentNode;
    }

    /**
     * Searches the supplied list for node with matching name and type.  Does NOT search children or parents of supplied nodes.
     * @param connectionName the connection name
     * @param name the node name
     * @param type the node type
     * @param nodeList the list of nodes to search
     * @return the matching node, if found
     */
    private RestSchemaNode getMatchingNode(String connectionName, String name, String type, RestSchemaNode[] nodeArray) {
		RestSchemaNode matchedNode = null;
    	for(RestSchemaNode node : nodeArray) {
			if( node.getConnectionName().equals(connectionName) && node.getName().equals(name) && node.getType().equals(type) ) {
				matchedNode = node;
				break;
			}
		}
    	return matchedNode;
    }

    /**
     * Split the segment apart and return the name
     * @param segment the segment (eg "table=customer")
     * @return the name
     */
    private String getSegmentName(String segment) {
    	String[] parts = segment.split(EQUALS);
    	return parts[1].trim();
    }
    
    /**
     * Split the segment apart and return the type
     * @param segment the segment (eg "table=customer")
     * @return the type
     */
    private String getSegmentType(String segment) {
    	String[] parts = segment.split(EQUALS);
    	return parts[0].trim();
    }
}