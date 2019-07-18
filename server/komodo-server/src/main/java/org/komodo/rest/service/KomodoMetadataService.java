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
package org.komodo.rest.service;

import java.net.URI;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
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

import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.query.QSResult;
import org.komodo.metadata.runtime.TeiidDataSource;
import org.komodo.metadata.runtime.TeiidVdb;
import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.PublishConfiguration;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.WorkspaceManager;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.CallbackTimeoutException;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.connection.RestSchemaNode;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.request.PublishRequestPayload;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.rest.relational.response.virtualization.RestVirtualizationStatus;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
/**
 * A Komodo REST service for obtaining information from a metadata instance.
 */
@Component
@Path( V1Constants.METADATA_SEGMENT )
@Api( tags = {V1Constants.METADATA_SEGMENT} )
public class KomodoMetadataService extends KomodoService {

    private interface OptionalParam {

        /**
         * Indicates if connection schema should be generated if it doesn't exist. Defaults to <code>true</code>.
         */
        String GENERATE_SCHEMA = "generate-schema"; //$NON-NLS-1$

        /**
         * Indicates if the connection server VDB should be redeployed if it already exists. Defaults to <code>false</code>.
         */
        String REDEPLOY_CONNECTION = "redeploy"; //$NON-NLS-1$

    }

    private static final String CONNECTION_VDB_PATTERN = "{0}btlconn"; //$NON-NLS-1$

    private static final String SCHEMA_MODEL_NAME_PATTERN = "{0}schemamodel"; //$NON-NLS-1$
    private static final String SCHEMA_VDB_NAME_PATTERN = "{0}schemavdb"; //$NON-NLS-1$

    /**
     * fqn table option key
     */
    private final static String TABLE_OPTION_FQN = "teiid_rel:fqn"; //$NON-NLS-1$

    /**
     * Time to wait after deploying/undeploying an artifact from the metadata instance
     */
    private final static int DEPLOYMENT_WAIT_TIME = 10000;

    @Autowired
    private TeiidOpenShiftClient openshiftClient;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @param openshiftClient OpenShift client to access service catalog
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoMetadataService() throws WebApplicationException {
    }

    private synchronized MetadataInstance getMetadataInstance() throws KException {
        return this.kengine.getMetadataInstance();
    }

    private Response createTimeoutResponse(List<MediaType> mediaTypes) {
        Object responseEntity = createErrorResponseEntity(mediaTypes,
                                                                  RelationalMessages.getString(
                                                                                               RelationalMessages.Error.VDB_SAMPLE_IMPORT_TIMEOUT));
        return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
    }

    private boolean hasDynamicVdb(String vdbName) throws Exception {
        boolean hasVdb = false;

        Collection<TeiidVdb> vdbs = getMetadataInstance().getVdbs();
        for (TeiidVdb vdb : vdbs) {
            if (vdb.getName().startsWith(vdbName)) {
                hasVdb = true;
                break;
            }
        }

        return hasVdb;
    }    
    
    /**
     * Remove a VDB from the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the dynamic VDB name (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error removing the VDB
     */
    @DELETE
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Removes a Vdb from the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeVdb(final @Context HttpHeaders headers,
                              final @Context UriInfo uriInfo,
                              @ApiParam(value = "Name of the VDB to be removed", required = true)
                              final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "unDeployTeiidDriver", false); //$NON-NLS-1$

            getMetadataInstance().undeployDynamicVdb(vdbName);

            // Await the undeployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            if (! hasDynamicVdb(vdbName)) {
                status.addAttribute(vdbName,
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_SUCCESSFULLY_UNDEPLOYED));
            } else
                status.addAttribute(vdbName,
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_UNDEPLOYMENT_REQUEST_SENT));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_UNDEPLOY_VDB_ERROR, vdbName);
        }
    }

//
//    TODO
//
//    Cannot actually use this at the moment since the removal and addition
//    of a datasource during the same runtime fails with the error:
//
//    TEIID70006 {"WFLYCTL0062: Composite operation failed and was rolled back
//    Steps that failed:" => {"Operation step-1" => "WFLYCTL0158: Operation handler
//    failed: org.jboss.msc.service.DuplicateServiceException: Service
//    org.wildfly.data-source.{DatasourceName} is already registered"}}
//
//    Cannot refresh teiid either since its not part of the API
//    see https://issues.jboss.org/browse/TEIID-4592
//
//    /**
//     * Updates a Connection on the server (deletes then adds)
//     * @param headers
//     *        the request headers (never <code>null</code>)
//     * @param uriInfo
//     *        the request URI information (never <code>null</code>)
//     * @param pathAttribute
//     *        the path (never <code>null</code>)
//     * @return a JSON representation of the status (never <code>null</code>)
//     * @throws KomodoRestException
//     *         if there is an error adding the Connection
//     */
//    @SuppressWarnings( "nls" )
//    @PUT
//    @Path(V1Constants.CONNECTION_SEGMENT)
//    @Produces( MediaType.APPLICATION_JSON )
//    @Consumes ( { MediaType.APPLICATION_JSON } )
//    @ApiOperation(value = "Updates the connection on the teiid server")
//    @ApiResponses(value = {
//        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
//        @ApiResponse(code = 403, message = "An error has occurred.")
//    })
//    public Response updateConnection( final @Context HttpHeaders headers,
//                                   final @Context UriInfo uriInfo,
//                                   @ApiParam(
//                                             value = "" +
//                                                     "JSON of the properties of the connection:<br>" +
//                                                     OPEN_PRE_TAG +
//                                                     OPEN_BRACE + BR +
//                                                     NBSP + "path: \"location of the connection in the workspace\"" + BR +
//                                                     CLOSE_BRACE +
//                                                     CLOSE_PRE_TAG,
//                                             required = true
//                                   )
//                                   final String pathAttribute)
//                                   throws KomodoRestException {
//
//        SecurityPrincipal principal = checkSecurityContext(headers);
//        if (principal.hasErrorResponse())
//            return principal.getErrorResponse();
//
//        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
//        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
//            return notAcceptableMediaTypesBuilder().build();
//
//        //
//        // Error if there is no path attribute defined
//        //
//        KomodoPathAttribute kpa;
//        try {
//            kpa = KomodoJsonMarshaller.unmarshall(pathAttribute, KomodoPathAttribute.class);
//            if (kpa.getPath() == null) {
//                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_DATA_SOURCE_MISSING_PATH);
//            }
//        } catch (Exception ex) {
//            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.METADATA_SERVICE_REQUEST_PARSING_ERROR);
//        }
//
//        UnitOfWork uow = null;
//        try {
//            Teiid teiidNode = getDefaultTeiid();
//
//            uow = createTransaction(principal, "updateTeiidConnection", false); //$NON-NLS-1$
//
//            TeiidInstance getMetadataInstance() = teiidNode.getTeiidInstance(uow);
//
//            List<KomodoObject> dataSources = this.repo.searchByPath(uow, kpa.getPath());
//            if (dataSources.size() == 0) {
//                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_DATA_SOURCE_FOUND);
//            }
//
//            Connection dataSource = getWorkspaceManager(uow).resolve(uow, dataSources.get(0), Connection.class);
//            if (dataSource == null) {
//                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_DATA_SOURCE_FOUND);
//            }
//
//            //
//            // If connection exists then remove it first
//            //
//            String connectionName = dataSource.getName(uow);
//            if (getMetadataInstance().dataSourceExists(connectionName)) {
//                getMetadataInstance().deleteDataSource(connectionName);
//                Thread.sleep(DEPLOYMENT_WAIT_TIME);
//            }
//
//            //
//            // Deploy the data source
//            //
//            DeployStatus deployStatus = dataSource.deploy(uow, teiidNode);
//
//            // Await the deployment to end
//            Thread.sleep(DEPLOYMENT_WAIT_TIME);
//
//            // Make sure Datasource is current in the CachedTeiid
//            refreshCachedDataSources(teiidNode);
//
//            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_DEPLOYMENT_STATUS_TITLE);
//            KomodoStatusObject status = new KomodoStatusObject(title);
//
//            List<String> progressMessages = deployStatus.getProgressMessages();
//            for (int i = 0; i < progressMessages.size(); ++i) {
//                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
//            }
//
//            if (deployStatus.ok()) {
//                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
//                status.addAttribute(dataSource.getName(uow),
//                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_SUCCESSFULLY_DEPLOYED));
//            } else {
//                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
//                List<String> errorMessages = deployStatus.getErrorMessages();
//                for (int i = 0; i < errorMessages.size(); ++i) {
//                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
//                }
//
//                status.addAttribute(dataSource.getName(uow),
//                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SOURCE_DEPLOYED_WITH_ERRORS));
//            }
//
//           return commit(uow, mediaTypes, status);
//
//        } catch (final Exception e) {
//            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
//                uow.rollback();
//            }
//
//            if (e instanceof KomodoRestException) {
//                throw (KomodoRestException)e;
//            }
//
//            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_DEPLOY_DATA_SOURCE_ERROR);
//        }
//    }

    /**
     * Refresh the preview Vdb with the supplied name
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the vdb name (cannot be empty)
     * @return a JSON representation of the refresh status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error refreshing the preview vdb
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.REFRESH_PREVIEW_VDB_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Refresh the deployed preview vdb")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response refreshPreviewVdb( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(
                                          value = "Name of the Vdb to be refreshed",
                                          required = true
                                       )
                                       final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the vdb name is missing
        if (StringUtils.isBlank( vdbName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_MISSING_VDB_NAME);
        }


        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "refreshPreviewVdb", false ); //$NON-NLS-1$

            WorkspaceManager wMgr = getWorkspaceManager(uow);

            Vdb previewVdb = wMgr.findVdb(uow, vdbName);
            
            // if workspace does not have preview vdb, then create it.
            if (previewVdb == null ) {
            	previewVdb = wMgr.createVdb( uow, vdbName );
            }

            // Get the list of current preview VDB import names
            List<String> currentVdbImportNames = new ArrayList<String>();
            VdbImport[] currentVdbImports = previewVdb.getImports(uow);
            for( VdbImport vdbImport: currentVdbImports ) {
            	currentVdbImportNames.add(vdbImport.getName(uow));
            }

            // Get the current workspace connection VDB names
            List<String> connectionVdbNames = new ArrayList<String>();
            Vdb[] connVdbObjs = wMgr.findVdbs(uow, "*btlconn"); //$NON-NLS-1$
            for( Vdb kObj: connVdbObjs) {
           		connectionVdbNames.add(kObj.getName(uow));
            }

            // Add import for connectionVdb if it is missing
            boolean importAdded = false;
            for(String connVdbName: connectionVdbNames) {
            	if(!currentVdbImportNames.contains(connVdbName)) {
            		previewVdb.addImport(uow, connVdbName);
            		importAdded = true;
            	}
            }

            // Remove extra imports
            boolean importRemoved = false;
            for(String currentVdbImportName: currentVdbImportNames) {
            	if(!connectionVdbNames.contains(currentVdbImportName)) {
            		previewVdb.removeImport(uow, currentVdbImportName);
            		importRemoved = true;
            	}
            }

            // check if there is a VDB already deployed in the instance
            TeiidVdb vdb = getMetadataInstance().getVdb(previewVdb.getName(uow));
             
            // The updated VDB is deployed if imports were added or removed
            if(vdb == null || importAdded || importRemoved) {
                //
                // Deploy the VDB
                //
                DeployStatus deployStatus = getMetadataInstance().deploy(uow, previewVdb);

                // Await the deployment to end
                Thread.sleep(DEPLOYMENT_WAIT_TIME);

                String title = RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYMENT_STATUS_TITLE);
                KomodoStatusObject status = new KomodoStatusObject(title);

                List<String> progressMessages = deployStatus.getProgressMessages();
                for (int i = 0; i < progressMessages.size(); ++i) {
                    status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i)); //$NON-NLS-1$
                }

                if (deployStatus.ok()) {
                    status.addAttribute("deploymentSuccess", Boolean.TRUE.toString()); //$NON-NLS-1$
                    status.addAttribute(previewVdb.getName(uow),
                                        RelationalMessages.getString(RelationalMessages.Info.VDB_SUCCESSFULLY_DEPLOYED));
                } else {
                    status.addAttribute("deploymentSuccess", Boolean.FALSE.toString()); //$NON-NLS-1$
                    List<String> errorMessages = deployStatus.getErrorMessages();
                    for (int i = 0; i < errorMessages.size(); ++i) {
                        status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i)); //$NON-NLS-1$
                    }

                    status.addAttribute(previewVdb.getName(uow),
                                        RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYED_WITH_ERRORS));
                }

               return commit(uow, mediaTypes, status);
            } else {
            	KomodoStatusObject kso = new KomodoStatusObject("Preview VDB Status"); //$NON-NLS-1$
            	kso.addAttribute(vdbName, "No refresh required"); //$NON-NLS-1$

            	return commit(uow, mediaTypes, kso);
            }
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_REFRESH_PREVIEW_VDB_ERROR);
        }
    }

    /**
     * Query the teiid server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param queryAttribute
     *        the query attribute (never <code>null</code>)
     * @return a JSON representation of the Query results (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error getting results
     */
    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.QUERY_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Pass a query to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 400, message = "An error has occurred.")
    })
    public Response query(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "" +
                                                     "JSON of the properties of the query:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "query: \"SQL formatted query to interrogate the target\"" + COMMA + BR +
                                                     NBSP + "target: \"The name of the target to be queried\"" + BR +
                                                     NBSP + OPEN_PRE_CMT + "(The target can be a vdb or data service. If the latter " +
                                                     NBSP + "then the name of the service vdb is extracted and " +
                                                     NBSP + "replaces the data service)" + CLOSE_PRE_CMT + COMMA + BR +
                                                     NBSP + "limit: Add a limit on number of results to be returned" + COMMA + BR +
                                                     NBSP + "offset: The index of the result to begin the results with" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
                                   final String queryAttribute)
                                   throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        //
        // Error if there is no query attribute defined
        //
        KomodoQueryAttribute kqa;
        try {
            kqa = KomodoJsonMarshaller.unmarshall(queryAttribute, KomodoQueryAttribute.class);
            if (kqa.getQuery() == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_QUERY_MISSING_QUERY);
            }

            if (kqa.getTarget() == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_QUERY_MISSING_TARGET);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.METADATA_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "queryTeiidservice", true); //$NON-NLS-1$
            String target = kqa.getTarget();
            String query = kqa.getQuery();

            String vdbName = target;

            TeiidVdb vdb = getMetadataInstance().getVdb(vdbName);
            if (vdb == null) {
				return createErrorResponse(Status.BAD_REQUEST, mediaTypes,
						RelationalMessages.Error.METADATA_SERVICE_QUERY_TARGET_NOT_DEPLOYED);
            }

            LOGGER.debug("Establishing query service for query {0} on vdb {1}", query, vdbName);
            QSResult result = getMetadataInstance().query(vdbName, query, kqa.getOffset(), kqa.getLimit());
            RestQueryResult restResult = new RestQueryResult(result);

           return commit(uow, mediaTypes, restResult);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, RelationalMessages.Error.METADATA_SERVICE_QUERY_ERROR, e.getLocalizedMessage());
        }
    }

    /**
     * Initiate schema refresh for a syndesis source.  This will either deploy a vdb for the source, or refresh an existing source vdb schema
     * - no params supplied : (redeploy=false, generate-schema=true) - If source vdb not found, it is deployed. If source vdb found, regen schema.
     * - params supplied (redeploy=true, generate-schema=any) - The source vdb is redeployed
     * - params supplied (redeploy=false, generate-schema=false) - If source vdb not found, it is deployed.  If source vdb found, no op
     * - params supplied (redeploy=false, generate-schema=true) - If source vdb not found, it is deployed.  If source vdb found, regen schema
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param syndesisSourceName
     *        the syndesis source name (cannot be empty)
     * @return a JSON representation of the refresh status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error initiating a schema refresh
     */
    @POST
    @Path( StringConstants.FORWARD_SLASH + V1Constants.REFRESH_SCHEMA_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.SYNDESIS_SOURCE_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Initiate schema refresh for a syndesis source")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response refreshSchema( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam( value = "Name of the syndesis source",
                                              required = true )
                                   final @PathParam( "syndesisSourceName" ) String syndesisSourceName,
                                   @ApiParam( value = "Indicates the source VDB should be redeployed if it already exists",
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

        // Error if the syndesisSource is missing
        if (StringUtils.isBlank( syndesisSourceName )) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.CONNECTION_SERVICE_MISSING_CONNECTION_NAME);
        }

        UnitOfWork uow = null;

        try {
            final String txId = "refreshSchema?redeploy=" + redeployServerVdb + "&generate-schema=" + generateSchema;   //$NON-NLS-1$//$NON-NLS-2$
            uow = createTransaction(principal, txId, false );

            // Find the bound teiid source corresponding to the syndesis source
            TeiidDataSource teiidSource = this.findTeiidSource(syndesisSourceName);

            if (teiidSource == null)
                return commitNoConnectionFound(uow, mediaTypes, syndesisSourceName);

            final KomodoStatusObject kso = new KomodoStatusObject( "Refresh schema" ); //$NON-NLS-1$
            final TeiidVdb deployedVdb = findDeployedVdb( syndesisSourceName );
            boolean doDeploy = false;

            // If no deployed VDB is found for the source, it is deployed regardless of other settings
            if ( deployedVdb == null ) {
                doDeploy = true;
            } else {
                doDeploy = redeployServerVdb;
            }

            // Initiate the VDB deployment
            if ( doDeploy ) {
                doDeploySourceVdb(uow, teiidSource); // this will delete workspace VDB first
                kso.addAttribute(syndesisSourceName, "Delete workspace VDB, recreate, and redeploy"); //$NON-NLS-1$
            } else if ( generateSchema ) {
                Vdb schemaVdb = findWorkspaceSchemaVdb( uow, teiidSource );
                final String schemaModelName = getSchemaModelName( syndesisSourceName );
                Model schemaModel = null;

                // create if necessary
                if ( schemaVdb == null ) {
                    final WorkspaceManager wkspMgr = getWorkspaceManager( uow );
                    final String schemaVdbName = getSchemaVdbName( syndesisSourceName );
                    schemaVdb = wkspMgr.createVdb( uow, schemaVdbName );

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
                kso.addAttribute(syndesisSourceName, "Generate schema"); //$NON-NLS-1$
                // after transaction is committed this will trigger the DDL sequencer which will create
                // the model objects.
            } else {
                kso.addAttribute( syndesisSourceName, "Neither redeploy or generate schema requested" ); //$NON-NLS-1$
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
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param syndesisSourceName
     *        the name of the syndesisSource whose tables are being requested (cannot be empty)
     * @return the JSON representation of the tables collection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified syndesis source or constructing the JSON representation
     */
    @GET
    @Path( "{syndesisSourceName}/schema" )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation( value = "Get the native schema for the syndesis source",
                   response = RestSchemaNode[].class )
    @ApiResponses( value = {
        @ApiResponse( code = 403, message = "An error has occurred." ),
        @ApiResponse( code = 404, message = "No syndesis source could be found with the specified name" ),
        @ApiResponse( code = 406, message = "Only JSON is returned by this operation" )
    } )
    public Response getSchema( @Context final HttpHeaders headers,
                               final @Context UriInfo uriInfo,
                               @ApiParam( value = "Name of the syndesis source",
                                          required = true )
                               @PathParam( "syndesisSourceName" )
                               final String syndesisSourceName ) throws KomodoRestException {
        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final List< MediaType > mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "getSchema?syndesisSourceName=" + syndesisSourceName, true ); //$NON-NLS-1$

            // Find the bound teiid source corresponding to the syndesis source
            TeiidDataSource teiidSource = this.findTeiidSource(syndesisSourceName);

            if (teiidSource == null)
                return commitNoConnectionFound(uow, mediaTypes, syndesisSourceName);

            final Model schemaModel = findSchemaModel( uow, teiidSource );

            List<RestSchemaNode> schemaNodes = Collections.emptyList();
            if ( schemaModel != null ) {
                final Table[] tables = schemaModel.getTables( uow );
                
                schemaNodes = this.generateSourceSchema(uow, syndesisSourceName, tables);
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
     * @return the JSON representation of the schema collection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the schema collection or constructing the JSON representation
     */
    @GET
    @Path( "connection-schema" )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation( value = "Get the native schema for all syndesis sources",
                   response = RestSchemaNode[].class )
    @ApiResponses( value = {
        @ApiResponse( code = 403, message = "An error has occurred." ),
        @ApiResponse( code = 404, message = "No results found" ),
        @ApiResponse( code = 406, message = "Only JSON is returned by this operation" )
    } )
    public Response getAllConnectionSchema( @Context final HttpHeaders headers,
                                            final @Context UriInfo uriInfo ) throws KomodoRestException {
        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final List< MediaType > mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction( principal, "getAllConnectionSchema", true ); //$NON-NLS-1$

            List<RestSchemaNode> rootNodes = new ArrayList<RestSchemaNode>();
            
            // Get syndesis sources
            Collection<DefaultSyndesisDataSource> dataSources = this.openshiftClient.getSyndesisSources(getAuthenticationToken());

            // Get teiid datasources
            Collection<TeiidDataSource> allTeiidSources = getMetadataInstance().getDataSources();

            // Add status summary for each of the syndesis sources.  Determine if there is a matching teiid source
            for (DefaultSyndesisDataSource dataSource : dataSources) {
                for (TeiidDataSource teiidSource : allTeiidSources) {
                    // Syndesis source has a corresponding VDB.  Use VDB for status
                    if (teiidSource.getName().equals(dataSource.getName())) {
                        final Model schemaModel = findSchemaModel( uow, teiidSource );

                        List<RestSchemaNode> schemaNodes = null;
                        if ( schemaModel != null ) {
                            final Table[] tables = schemaModel.getTables( uow );
                            
                            schemaNodes = this.generateSourceSchema(uow, dataSource.getName(), tables);
                            if(schemaNodes != null && !schemaNodes.isEmpty()) {
                            	RestSchemaNode rootNode = new RestSchemaNode();
                            	rootNode.setName(dataSource.getName());
                            	rootNode.setType("root");
                            	for(RestSchemaNode sNode: schemaNodes) {
                            		rootNode.addChild(sNode);
                            	}
                            	rootNodes.add(rootNode);
                            }
                        }
                    }
                }
            }

            return commit( uow, mediaTypes, rootNodes ); 
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
     * Get status for the available syndesis sources.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the statuses of the sources (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.SYNDESIS_SOURCE_STATUSES)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Return the syndesis source statuses",
                  response = RestSyndesisSourceStatus[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getSyndesisSourceStatuses( final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        final List< RestSyndesisSourceStatus > statuses = new ArrayList<>();

        try {

            // find sources
            uow = createTransaction(principal, "getSyndesisSourceStatuses", true ); //$NON-NLS-1$

            // Get syndesis sources
            Collection<DefaultSyndesisDataSource> dataSources = this.openshiftClient.getSyndesisSources(getAuthenticationToken());

            // Get teiid datasources
            Collection<TeiidDataSource> allTeiidSources = getMetadataInstance().getDataSources();

            // Add status summary for each of the syndesis sources.  Determine if there is a matching teiid source
            for (DefaultSyndesisDataSource dataSource : dataSources) {
                RestSyndesisSourceStatus status = new RestSyndesisSourceStatus(dataSource.getName());
                for (TeiidDataSource teiidSource : allTeiidSources) {
                    // Syndesis source has a corresponding VDB.  Use VDB for status
                    if (teiidSource.getName().equals(dataSource.getName())) {
                        status.setHasTeiidSource(true);
                    }
                }
                statuses.add(status);
            }
            
            // For each syndesis source, determine if there is a matching teiid VDB for the source
            final Collection< TeiidVdb > vdbs = getMetadataInstance().getVdbs();
            for( RestSyndesisSourceStatus status : statuses ) {
                // Name of vdb based on source name
                String vdbName = getWorkspaceSourceVdbName( status.getSourceName() );
                for (TeiidVdb vdb: vdbs) {
                    if ( vdb.getName().equals(vdbName) ) {
                        status.setTeiidVdbDetails(vdb);
                        break;
                    }
                }
            }

            // For each syndesis source, set the schema availability status
            for( RestSyndesisSourceStatus status : statuses ) {
                this.setSchemaStatus(uow, status);
            }

            LOGGER.debug( "getSyndesisSourceStatuses '{0}' statuses", statuses.size() ); //$NON-NLS-1$

            return commit( uow, mediaTypes, statuses );
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
    
    @GET
    @Path(V1Constants.SYNDESIS_SOURCE_STATUSES+"/{syndesisSourceName}")
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Return the syndesis source statuses by name",
                  response = RestSyndesisSourceStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
	public Response getSyndesisSourceStatusesByName(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
			@PathParam("syndesisSourceName") final String syndesisSourceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getSyndesisSourceStatus", true ); //$NON-NLS-1$

            TeiidDataSource teiidSource = getMetadataInstance().getDataSource(syndesisSourceName);
            RestSyndesisSourceStatus status = new RestSyndesisSourceStatus(syndesisSourceName);
            if (teiidSource != null) {
            	status.setHasTeiidSource(true);
            }

            // Name of vdb based on source name
            String vdbName = getWorkspaceSourceVdbName(syndesisSourceName);
            TeiidVdb teiidVdb = getMetadataInstance().getVdb(vdbName);
            if (teiidVdb != null) {
            	status.setTeiidVdbDetails(teiidVdb);
            }

            // For each syndesis source, set the schema availability status
            setSchemaStatus(uow, status);
            return commit( uow, mediaTypes, status );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }
            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }
			return createErrorResponseWithForbidden(mediaTypes, e,
					RelationalMessages.Error.CONNECTION_SERVICE_GET_CONNECTIONS_ERROR);
        }
    }    

    @GET
    @Path(V1Constants.PUBLISH)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Gets the published virtualization services", response = RestVirtualizationStatus[].class)
    @ApiResponses(value = { @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response getVirtualizations(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "true to include in progress services", required = true, defaultValue="true")
            @QueryParam("includeInProgress") boolean includeInProgressServices) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        //
        // Ensure include in-progress services is included by default
        //
        if (! uriInfo.getQueryParameters().containsKey("includeInProgressServices")) //$NON-NLS-1$
            includeInProgressServices = true;

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            final List<RestVirtualizationStatus> entityList = new ArrayList<>();
            Collection<BuildStatus> statuses = this.openshiftClient.getVirtualizations(includeInProgressServices);
            for (BuildStatus status : statuses) {
                entityList.add(createBuildStatus(status, uriInfo.getBaseUri()));
            }
            return commit(uow, mediaTypes, entityList);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }
            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }
            return createErrorResponseWithForbidden(mediaTypes, e,
                    RelationalMessages.Error.PUBLISH_ERROR);
        }
    }

    @GET
    @Path(V1Constants.PUBLISH + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find Build Status of Virtualization by VDB name", response = RestVirtualizationStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No VDB could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVirtualizationStatus(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the VDB", required = true) final @PathParam("vdbName") String vdbName)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            BuildStatus status = this.openshiftClient.getVirtualizationStatus(vdbName);

            return commit(uow, mediaTypes, createBuildStatus(status, uriInfo.getBaseUri()));
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }
            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }
            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PUBLISH_ERROR);
        }
    }

    @GET
    @Path(V1Constants.PUBLISH_LOGS + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find Publish Logs of Virtualization by VDB name", response = KomodoStatusObject.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No VDB could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVirtualizationLogs(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the VDB", required = true) final @PathParam("vdbName") String vdbName)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        KomodoStatusObject status = new KomodoStatusObject("Logs for " + vdbName); //$NON-NLS-1$

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            String log = this.openshiftClient.getVirtualizationLog(vdbName);
            status.addAttribute("log", log); //$NON-NLS-1$

            return commit(uow, mediaTypes, status);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }
            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }
            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PUBLISH_ERROR);
        }
    }
    
    /**
     * Create RestVirtualizationStatus
     * @param status the build status
     * @param baseUri the base uri
     * @return RestVirtualizationStatus
     * @throws Exception if error occurs
     */
    public RestVirtualizationStatus createBuildStatus(BuildStatus status, URI baseUri) throws Exception {
        return new RestVirtualizationStatus(baseUri, status);
    }

    @DELETE
    @Path(V1Constants.PUBLISH + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Delete Virtualization Service by VDB name",response = RestVirtualizationStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No VDB could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response deleteVirtualization(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the VDB", required = true) final @PathParam("vdbName") String vdbName)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            BuildStatus status = this.openshiftClient.deleteVirtualization(vdbName);
            return commit(uow, mediaTypes, createBuildStatus(status, uriInfo.getBaseUri()));
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }
            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }
            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PUBLISH_ERROR);
        }
    }

    @POST
    @Path(V1Constants.PUBLISH)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Publish Virtualization Service based on VDB or Dataservice",
                                response = KomodoStatusObject.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No VDB or Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response publishVirtualization(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "JSON properties:<br>" + OPEN_PRE_TAG + OPEN_BRACE + BR + NBSP
                    + "\"name\":      \"Name of the VDB or Dataservice\"" + BR
                    + "\"cpu-units\": \"(optional) Number of CPU units to allocate. 100 is 0.1 CPU (default 500)\"" + BR
                    + "\"memory\":    \"(optional) Amount memory to allocate in MB (default 1024)\"" + BR
                    + "\"disk-size\": \"(optional) Amount disk allocated in GB (default 20)\"" + BR
                    + "\"enable-odata\": \"(optional) Enable OData interface. true|false (default true)\"" + BR
                    + CLOSE_BRACE
                    + CLOSE_PRE_TAG, required = true) final PublishRequestPayload payload)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse()) {
            return principal.getErrorResponse();
        }

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (!isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();
        //
        // Error if there is no name attribute defined
        //
        if (payload.getName() == null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VDB_NAME_NOT_PROVIDED);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "publish-init", true); //$NON-NLS-1$
            Vdb vdb = findVdbByServiceOrName(payload, mediaTypes, uow);

            if (vdb == null) {
		        return createErrorResponse(Status.NOT_FOUND, mediaTypes, RelationalMessages.Error.VDB_NOT_FOUND);
		    }
            
            KomodoStatusObject status = new KomodoStatusObject();
            status.addAttribute("Publishing", "Operation initiated");  //$NON-NLS-1$//$NON-NLS-2$

            final OAuthCredentials creds = getAuthenticationToken();

            UnitOfWork publishUow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            
            //look the vdb backup with a new transaction
            Vdb theVdb = findVdbByServiceOrName(payload, mediaTypes, publishUow);
            
            if (theVdb == null) {
		        return createErrorResponse(Status.NOT_FOUND, mediaTypes, RelationalMessages.Error.VDB_NOT_FOUND);
		    }
            
            // the properties in this class can be exposed for user input
            PublishConfiguration config = new PublishConfiguration();
            config.setVDB(theVdb);
            config.setOAuthCredentials(creds);
            config.setTransaction(publishUow);
            config.setEnableOData(payload.getEnableOdata());
            config.setContainerDiskSize(payload.getDiskSize());
            config.setContainerMemorySize(payload.getMemory());
            config.setCpuUnits(payload.getCpuUnits());
            BuildStatus buildStatus = openshiftClient.publishVirtualization(config);

            //
            // If the thread concludes within the time of the parent thread sleeping
            // then add some build status messages.
            //
            status.addAttribute("Vdb Name", buildStatus.vdbName()); //$NON-NLS-1$
            status.addAttribute("Build Status", buildStatus.status().name()); //$NON-NLS-1$
            status.addAttribute("Build Status Message", buildStatus.statusMessage()); //$NON-NLS-1$

            //
            // Return the status from this request. Otherwise, monitor using #getVirtualizations()
            //
            return commit(uow, mediaTypes, status);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }
            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }
            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PUBLISH_ERROR, e.getMessage());
        }
    }

	private Vdb findVdbByServiceOrName(final PublishRequestPayload payload, List<MediaType> mediaTypes, UnitOfWork uow)
			throws KException {
		Vdb vdb = findVdb(uow, payload.getName());
		if (vdb == null) {
		    //
		    // We don't have a vdb so maybe we have a dataservice instead
		    // Find the dataservice's vdb to publish.
		    //
		    Dataservice dataservice = findDataservice(uow, payload.getName());
		    if (dataservice == null) {
		    	return null;
		    }

		    String name = dataservice.getServiceVdbName(uow);
		    if (name != null) {
		    	return findVdb(uow, name);
		    }
		}
		
		return vdb;
	}

    /**
     * Deploy / re-deploy a VDB to the metadata instance for the provided teiid data source.
     * @param uow the transaction
     * @param teiidSource the teiidSource
     * @return the DeployStatus from deploying the VDB
     * @throws KException
     * @throws InterruptedException
     */
    private DeployStatus doDeploySourceVdb( final UnitOfWork uow,
                                            TeiidDataSource teiidSource ) throws KException, InterruptedException {
        assert( uow.getState() == State.NOT_STARTED );
        assert( teiidSource != null );

        // Get necessary info from the source
        String sourceName = teiidSource.getName();
        String jndiName = teiidSource.getJndiName();
        String driverName = teiidSource.getType();
        
        // Name of VDB to be created is based on the source name
        String vdbName = getWorkspaceSourceVdbName( sourceName );
        
        // VDB is created in the repository.  If it already exists, delete it
        final WorkspaceManager mgr = this.kengine.getWorkspaceManager(uow);
        
        final Vdb existingVdb = findVdb( uow, vdbName );

        if ( existingVdb != null ) {
            mgr.deleteVdb(uow, existingVdb);
        }
        
        // delete schema VDB if it exists
        final Vdb schemaVdb = findWorkspaceSchemaVdb( uow, teiidSource );

        if ( schemaVdb != null ) {
            mgr.deleteVdb( uow, schemaVdb );
        }

        // Create new VDB
        final Vdb vdb = mgr.createVdb( uow, vdbName );
        vdb.setDescription(uow, "Vdb for source "+teiidSource); //$NON-NLS-1$
                    
        // Add model to the VDB
        Model model = vdb.addModel(uow, getSchemaModelName(sourceName));
        model.setModelType(uow, Model.Type.PHYSICAL);
        model.setPropertyValue(uow, "importer.TableTypes", "TABLE,VIEW"); //$NON-NLS-1$ //$NON-NLS-2$
        model.setPropertyValue(uow, "importer.UseQualifiedName", "true");  //$NON-NLS-1$//$NON-NLS-2$
        model.setPropertyValue(uow, "importer.UseCatalogName", "false");  //$NON-NLS-1$//$NON-NLS-2$
        model.setPropertyValue(uow, "importer.UseFullSchemaName", "false");  //$NON-NLS-1$//$NON-NLS-2$
        if (teiidSource.getPropertyValue("schema") != null) {
        	model.setPropertyValue(uow, "importer.schemaPattern", teiidSource.getPropertyValue("schema"));  //$NON-NLS-1$//$NON-NLS-2$
        }
        
        // Add model source to the model
        final String modelSourceName = teiidSource.getName();
        ModelSource modelSource = model.addSource(uow, modelSourceName);
        modelSource.setJndiName(uow, jndiName);
        modelSource.setTranslatorName(uow, driverName);
        // TODO: re-implement, needed for publishing
        // modelSource.setAssociatedConnection(uow, connection);
        
        // Deploy the VDB
        DeployStatus deployStatus = getMetadataInstance().deploy(uow, vdb);
        
        // Wait for deployment to complete
        Thread.sleep(DEPLOYMENT_WAIT_TIME);
        
        return deployStatus;
    }
    
    /**
     * Add model to the schema vdb
     * @param uow the transaction
     * @param schemaVdb the schema VDB
     * @param dataSource the teiid dataSource
     * @param schemaModelName the name for the schema model being created
     * @return the created schema model
     * @throws KException
     */
    private Model addModelToSchemaVdb(final UnitOfWork uow, final Vdb schemaVdb, final TeiidDataSource dataSource, final String schemaModelName) throws KException {
        // create schema model
        Model schemaModel = schemaVdb.addModel( uow, schemaModelName );
        
        // Make a copy of the workspace syndesis source vdb model source under the syndesis source schema vdb model
        final ModelSource workspaceVdbModelSource = findWorkspaceSyndesisSourceVdbModelSource( uow, dataSource );
        if( workspaceVdbModelSource != null ) {
            ModelSource mdlSource = schemaModel.addSource(uow, workspaceVdbModelSource.getName(uow));
            mdlSource.setJndiName(uow, workspaceVdbModelSource.getJndiName(uow));
            mdlSource.setTranslatorName(uow, workspaceVdbModelSource.getTranslatorName(uow));
            // TODO: re-implement, needed for publishing
            // mdlSource.setAssociatedConnection(uow, workspaceVdbModelSource.getOriginConnection(uow));
        }
        
        return schemaModel;
    }

    /**
     * Find the VDB in the workspace for the specified teiid dataSource
     * @param uow the transaction
     * @param dataSource the teiid data source
     * @return the workspace VDB
     * @throws KException
     */
    private Vdb findWorkspaceSyndesisSourceVdb( final UnitOfWork uow,
                                                final TeiidDataSource dataSource ) throws KException {
        final String dataSourceName = dataSource.getName( );

        final String wsSourceVdbName = this.getWorkspaceSourceVdbName(dataSourceName);

        Vdb vdb = findVdb(uow, wsSourceVdbName);
        return vdb;
    }
    
    /**
     * Find the Vdb ModelSource for the workspace syndesis source VDB
     * @param uow the transaction
     * @param dataSource the teiid data source
     * @return the workspace VDB
     * @throws KException
     */
    private ModelSource findWorkspaceSyndesisSourceVdbModelSource( final UnitOfWork uow,
                                                                   final TeiidDataSource dataSource ) throws KException {
        ModelSource modelSource = null;
        
        final Vdb vdb = findWorkspaceSyndesisSourceVdb( uow, dataSource );

        if ( vdb != null ) {
            final String dataSourceName = dataSource.getName( );
            final String schemaModelName = getSchemaModelName( dataSourceName );
            final Model[] models = vdb.getModels(uow, schemaModelName);

            Model model = null;
            if ( models.length != 0 ) {
                model = models[ 0 ];
            }
            
            if( model != null ) {
                final String schemaModelSourceName = dataSource.getName();
                final ModelSource[] modelSources = model.getSources(uow, schemaModelSourceName);
                if ( modelSources.length != 0 ) {
                    modelSource = modelSources[ 0 ];
                }
            }
        }

        return modelSource;
    }

    /**
     * Find the schema VDB in the workspace for the specified teiid source
     * @param uow the transaction
     * @param dataSource the teiid datasource
     * @return the VDB
     * @throws KException
     */
    private Vdb findWorkspaceSchemaVdb( final UnitOfWork uow,
                                        final TeiidDataSource dataSource ) throws KException {
        final String dataSourceName = dataSource.getName( );
        final String schemaVdbName = getSchemaVdbName( dataSourceName );

        Vdb vdb = findVdb(uow, schemaVdbName);
        return vdb;
    }
    
    /**
     * Find the schema VDB model in the workspace for the specified teiid source
     * @param uow the transaction
     * @param dataSource the teiid datasource
     * @return the Model
     * @throws KException
     */
    private Model findSchemaModel( final UnitOfWork uow,
                                   final TeiidDataSource dataSource ) throws KException {
        final Vdb vdb = findWorkspaceSchemaVdb( uow, dataSource );

        if ( vdb != null ) {
            final String dataSourceName = dataSource.getName( );
            final String schemaModelName = getSchemaModelName( dataSourceName );
            final Model[] models = vdb.getModels( uow, schemaModelName );

            if ( models.length != 0 ) {
                return models[ 0 ];
            }
        }

        return null;
    }

    /**
     * Find the teiid datasource with the specified name
     * @param sourceName the source name
     * @return the teiid datasource
     * @throws KException
     */
    private TeiidDataSource findTeiidSource( final String sourceName ) throws KException {
        return getMetadataInstance().getDataSource(sourceName);
    }

    /**
     * Find the deployed Vdb for the specified source
     * @param sourceName the source name
     * @return the TeiidVdb
     * @throws KException
     */
    private TeiidVdb findDeployedVdb( final String sourceName ) throws KException {
        final String sourceVdbName = getWorkspaceSourceVdbName( sourceName );
        return getMetadataInstance().getVdb( sourceVdbName );
    }


    /**
     * Generate schema VDB name, given the name of the source
     * @param sourceName the source name
     * @return the schema VDB name
     */
    static String getSchemaVdbName( final String sourceName ) {
        return MessageFormat.format( SCHEMA_VDB_NAME_PATTERN, sourceName.toLowerCase() );
    }

    /**
     * Generate a workspace source vdb name, given the name of the source
     * @param sourceName the source name
     * @return the source vdb name
     */
    private String getWorkspaceSourceVdbName( final String sourceName ) {
        return MessageFormat.format( CONNECTION_VDB_PATTERN, sourceName.toLowerCase() );
    }

    /**
     * Generate schema VDB model name, given the name of the source
     * @param sourceName the source name
     * @return the schema VDB model name
     */
    static String getSchemaModelName( final String sourceName ) {
        return MessageFormat.format( SCHEMA_MODEL_NAME_PATTERN, sourceName.toLowerCase() );
    }
    
    /**
     * Generate the syndesis source schema structure using the supplied table fqn information.
     * @param uow the transaction
     * @param sourceName the name of the source
     * @param tables the supplied array of tables
     * @return the list of schema nodes
     * @throws KException exception if problem occurs
     */
    private List<RestSchemaNode> generateSourceSchema(final UnitOfWork uow, final String sourceName, final Table[] tables) throws KException {
        List<RestSchemaNode> schemaNodes = new ArrayList<RestSchemaNode>();

        for(final Table table : tables) {
            // Use the fqn table option do determine native structure
            String option = table.getPropertyValue(uow, TABLE_OPTION_FQN );
            if( option != null ) {
                // Break fqn into segments (segment starts at root, eg "schema=public/table=customer")
                String[] segments = option.split(FORWARD_SLASH);
                // Get the parent node of the final segment in the 'path'.  New nodes are created if needed.
                RestSchemaNode parentNode = getLeafNodeParent(sourceName, schemaNodes, segments);

                // Use last segment to create the leaf node child in the parent.  If parent is null, was root (and leaf already created).
                if( parentNode != null ) {
                    String type = getSegmentType(segments[segments.length-1]);
                    String name = getSegmentName(segments[segments.length-1]);
                    RestSchemaNode node = new RestSchemaNode(sourceName, name, type);
                    node.setQueryable(true);
                    String path = createSchemaNodePath(segments.length-1, segments);
                    node.setPath(path);
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
     * @param sourceName the name of the source
     * @param currentNodes the current node list
     * @param segments the full path of segments, starting at the root
     * @return the final segments parent node.  (null if final segment is at the root)
     */
    private RestSchemaNode getLeafNodeParent(String sourceName, List<RestSchemaNode> currentNodes, String[] segments) {
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
                RestSchemaNode matchNode = getMatchingNode(sourceName, name, type, currentNodes.toArray( new RestSchemaNode[ currentNodes.size() ] ));
                // No match - create a new node
                if(matchNode == null) {
                    matchNode = new RestSchemaNode(sourceName, name, type);
                    String path = createSchemaNodePath(i,segments);
                    matchNode.setPath(path);
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
                RestSchemaNode matchNode = getMatchingNode(sourceName, name, type, parentNode.getChildren());
                // No match - create a new node
                if(matchNode == null) {
                    matchNode = new RestSchemaNode(sourceName, name, type);
                    String path = createSchemaNodePath(i,segments);
                    matchNode.setPath(path);
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
     * Generate the path for the node, given the segments and the position within the segments
     * @param iPosn the index position within the segments
     * @param segments the array of segments
     * @return the node path (segment0/segment1/etc)
     */
    private String createSchemaNodePath(int iPosn, String[] segments) {
        StringBuilder sb = new StringBuilder();
        if(segments!=null && segments.length > 0) {
            for (int i = 0; i < segments.length; i++) {
                if(i < iPosn) {
                    sb.append(segments[i]+"/"); //$NON-NLS-1$
                } else {
                    sb.append(segments[i]);
                    break;
                }
            }
        }
        return sb.toString();
    }
    
    /**
     * Searches the supplied list for node with matching name and type.  Does NOT search children or parents of supplied nodes.
     * @param sourceName the source name
     * @param name the node name
     * @param type the node type
     * @param nodeList the list of nodes to search
     * @return the matching node, if found
     */
    private RestSchemaNode getMatchingNode(String sourceName, String name, String type, RestSchemaNode[] nodeArray) {
        RestSchemaNode matchedNode = null;
        for(RestSchemaNode node : nodeArray) {
            if( node.getConnectionName().equals(sourceName) && node.getName().equals(name) && node.getType().equals(type) ) {
                matchedNode = node;
                break;
            }
        }
        return matchedNode;
    }

    /**
     * Split the segment apart and return the name
     * @param segment the segment (eg "table=customer")
     * @return the name (eg "customer")
     */
    private String getSegmentName(String segment) {
        String[] parts = segment.split(EQUALS);
        return parts[1].trim();
    }
    
    /**
     * Split the segment apart and return the type
     * @param segment the segment (eg "table=customer")
     * @return the type (eg "table")
     */
    private String getSegmentType(String segment) {
        String[] parts = segment.split(EQUALS);
        return parts[0].trim();
    }
    
    /**
     * Set the schema availability for the provided RestSyndesisSourceStatus 
     * @param uow the transaction
     * @param status the RestSyndesisSourceStatus
     * @throws Exception if error occurs
     */
    private void setSchemaStatus( final UnitOfWork uow,
                                  final RestSyndesisSourceStatus status ) throws Exception {
        // Name of schema vdb based on source name
        final String srcName = status.getSourceName();
        final String schemaVdbName = getSchemaVdbName( srcName );

        // Get the workspace schema VDB
        Vdb vdb = findVdb(uow, schemaVdbName);

        // If no vdb found, then status is not set
        if ( vdb != null ) {
            status.setSchemaVdbName( vdb.getName(uow) );

            // there should be one model
            final String schemaModelName = getSchemaModelName( srcName );
            final Model[] models = vdb.getModels( uow, schemaModelName );

            if ( models.length > 0 ) {
                final Model schemaModel = models[ 0 ];
                status.setSchemaModelName( schemaModelName );
                
                if (getWorkspaceManager(uow).isSchemaActive(uow, schemaModel)) {
                	status.setSchemaState( RestSyndesisSourceStatus.EntityState.ACTIVE );
                } else if ( schemaModel.getModelDefinition(uow) != null) {
                	status.setSchemaState( RestSyndesisSourceStatus.EntityState.LOADING );
                }
            } else {
                // Since VDB and model are created in the same transaction this should never happen.
                // Would be nice to be able to get here if we can detect the DDL sequencing failed.
                status.setSchemaState( RestSyndesisSourceStatus.EntityState.FAILED );
            }
        }
    }

}
