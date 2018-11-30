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

import java.io.File;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
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

import org.komodo.core.KEngine;
import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.PublishConfiguration;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.CallbackTimeoutException;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoFileAttributes;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.request.KomodoSyndesisDataSourceAttributes;
import org.komodo.rest.relational.request.PublishRequestPayload;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestSyndesisDataSource;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.rest.relational.response.metadata.RestMetadataConnection;
import org.komodo.rest.relational.response.metadata.RestMetadataStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplate;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplateEntry;
import org.komodo.rest.relational.response.metadata.RestMetadataVdb;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbTranslator;
import org.komodo.rest.relational.response.virtualization.RestVirtualizationStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.SyndesisDataSource;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining information from a metadata instance.
 */
@Path( V1Constants.METADATA_SEGMENT )
@Api( tags = {V1Constants.METADATA_SEGMENT} )
public class KomodoMetadataService extends KomodoService {

    /**
     * Default translator mappings for different drivers
     */
    private final static String DRIVER_TRANSLATOR_MAPPING_FILE = "driverTranslatorMappings.xml"; //$NON-NLS-1$

    /**
     * Default translator mappings for connection URL content
     */
    private final static String URLCONTENT_TRANSLATOR_MAPPING_FILE = "urlContentTranslatorMappings.xml"; //$NON-NLS-1$

    /**
     * Translator mapping file elements and attributes
     */
    private final static String ELEM_TRANSLATOR = "translator"; //$NON-NLS-1$
    private final static String ATTR_DRIVER = "driver"; //$NON-NLS-1$
    private final static String ATTR_URLCONTENT = "urlcontent"; //$NON-NLS-1$

    /**
     * Unknown translator
     */
    private final static String UNKNOWN_TRANSLATOR = "unknown"; //$NON-NLS-1$

    /**
     * Time to wait after deploying/undeploying an artifact from the metadata instance
     */
    private final static int DEPLOYMENT_WAIT_TIME = 10000;

    private static final String[] PRIORITY_TEMPLATE_NAMES = {"connection-url", "user-name", "password", "port"};

    private static class TeiidPropertyDefinitionComparator implements Comparator<TeiidPropertyDefinition> {

        @Override
        public int compare(TeiidPropertyDefinition entry1, TeiidPropertyDefinition entry2) {
            String entry1Name = null;
            boolean entry1Advanced = false;
            String entry2Name = null;
            boolean entry2Advanced = false;

            try {
                entry1Name = entry1.getName();
                entry1Advanced = entry1.isAdvanced();

                entry2Name = entry2.getName();
                entry2Advanced = entry2.isAdvanced();
            } catch (Exception e) {
                // Ignore exception. Will be dealt with below
            }

            if (entry1Name == null && entry2Name == null)
                return 0; // Not a lot
            else if (entry1Name == null)
                return -1;
            else if (entry2Name == null)
                return 1;

            if (entry1Name.equals(entry2Name))
                return 0;

            for (String name : PRIORITY_TEMPLATE_NAMES) {
                if (name.equals(entry1Name))
                    return -1;

                if (name.equals(entry2Name))
                    return 1;
            }

            if (entry1Advanced && !entry2Advanced)
                return 1; // De-prioritise advanced
            else if (! entry1Advanced && entry2Advanced)
                return -1; // De-prioritise advanced

            return entry1Name.compareTo(entry2Name);
        }
    }

    /**
     * Mapping of driverName to default translator
     */
    private Map<String, String> driverTranslatorMap = new HashMap<String,String>();

    /**
     * Mapping of urlContent to default translator
     */
    private Map<String, String> urlContentTranslatorMap = new HashMap<String,String>();

    private TeiidOpenShiftClient openshiftClient;

    /**
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     * @param openshiftClient OpenShift client to access service catalog
     * @throws WebApplicationException
     *         if there is a problem obtaining the {@link WorkspaceManager workspace manager}
     */
    public KomodoMetadataService(final KEngine engine, TeiidOpenShiftClient openshiftClient) throws WebApplicationException {
        super(engine);
        // Loads default translator mappings
        loadDriverTranslatorMap();
        loadUrlContentTranslatorMap();

        this.openshiftClient = openshiftClient;
    }

    private synchronized MetadataInstance getMetadataInstance() throws KException {
        return this.kengine.getMetadataInstance();
    }

    private String getSchema(UnitOfWork uow, String vdbName, String modelName) throws Exception {
        MetadataInstance mServer = getMetadataInstance();
        return mServer.getSchema(vdbName, "1", modelName);
    }

    private Response createTimeoutResponse(List<MediaType> mediaTypes) {
        Object responseEntity = createErrorResponseEntity(mediaTypes,
                                                                  RelationalMessages.getString(
                                                                                               RelationalMessages.Error.VDB_SAMPLE_IMPORT_TIMEOUT));
        return Response.status(Status.FORBIDDEN).entity(responseEntity).build();
    }

    private Response checkFileAttributes(KomodoFileAttributes kfa, List<MediaType> mediaTypes) throws Exception {
        if (kfa == null || (kfa.getName() == null && kfa.getContent() == null))
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_FILE_ATTRIB_NO_PARAMETERS);

        if (kfa.getName() == null)
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_FILE_ATTRIB_NO_NAME);

        if (kfa.getContent() == null)
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_FILE_ATTRIB_NO_CONTENT);

        return Response.ok().build();
    }

    private boolean hasDriver(String driverName) throws Exception {
        boolean hasDriver = false;

        try {
            Collection<ConnectionDriver> drivers = getMetadataInstance().getDataSourceDrivers();
            for (ConnectionDriver driver : drivers) {
                if (driver.getName().startsWith(driverName)) {
                    hasDriver = true;
                    break;
                }
            }

            return hasDriver;

        } catch (KException ex) {
            this.kengine.getErrorHandler().error(ex);

            throw ex;
        }
    }

    private boolean hasDynamicVdb(String vdbName) throws Exception {
        boolean hasVdb = false;

        try {
            Collection<TeiidVdb> vdbs = getMetadataInstance().getVdbs();
            for (TeiidVdb vdb : vdbs) {
                if (vdb.getName().startsWith(vdbName)) {
                    hasVdb = true;
                    break;
                }
            }

            return hasVdb;

        } catch (KException ex) {
            this.kengine.getErrorHandler().error(ex);
            throw ex;
        }
    }

    private boolean hasDataSource(String dataSourceName) throws Exception {
        boolean hasDataSource = false;

        try {
            Collection<TeiidDataSource> datasources = getMetadataInstance().getDataSources();
            for (TeiidDataSource datasource : datasources) {
                if (datasource.getName().startsWith(dataSourceName)) {
                    hasDataSource = true;
                    break;
                }
            }

            return hasDataSource;

        } catch (KException ex) {
            this.kengine.getErrorHandler().error(ex);
            throw ex;
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the status of the local metadata instance (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.STATUS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the status of the metadata instance",
                            response = RestMetadataStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response status(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        try {
            MetadataInstance mServer = getMetadataInstance();
            RestMetadataStatus status = new RestMetadataStatus(uriInfo.getBaseUri(), mServer);
            return commit(mediaTypes, status);
        } catch (Throwable e) {
            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_STATUS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document the status of the VDBs in the local teiid server (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.STATUS_SEGMENT + StringConstants.FORWARD_SLASH +
                  V1Constants.VDBS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the status of the vdbs of the metadata instance",
                            response = RestMetadataVdbStatus.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response vdbs(final @Context HttpHeaders headers,
                                             final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        try {
            RestMetadataVdbStatus status = new RestMetadataVdbStatus(uriInfo.getBaseUri(), getMetadataInstance());

            // create response
            return commit(mediaTypes, status);

        } catch (Throwable e) {

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_VDBS_STATUS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the VDBs deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.VDBS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of vdbs",
                            response = RestVdb[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdbs(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "convertVdbs", true);

            // find VDBs
            Collection<TeiidVdb> vdbs = getMetadataInstance().getVdbs();
            LOGGER.debug("getVdbs:found '{0}' VDBs", vdbs.size()); //$NON-NLS-1$

            final List<RestMetadataVdb> entities = new ArrayList<>();
            Repository repo = this.kengine.getDefaultRepository();
            for (final TeiidVdb vdb : vdbs) {
                RestMetadataVdb entity = entityFactory.createMetadataVdb(uow, repo, vdb, uriInfo.getBaseUri());
                entities.add(entity);
                LOGGER.debug("getVdbs:VDB '{0}' entity was constructed", vdb.getName()); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
                return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_VDBS_ERROR);
        }
    }

    /**
     * Get the specified VDB
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB being retrieved (cannot be empty)
     * @return the JSON representation of the VDB (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified workspace VDB or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH +
                  V1Constants.VDB_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find vdb by name", response = RestMetadataVdb.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdb( final @Context HttpHeaders headers,
                            final @Context UriInfo uriInfo,
                            @ApiParam(value = "Id of the vdb to be fetched", required = true)
                            final @PathParam( "vdbName" ) String vdbName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            // find VDB
            uow = createTransaction(principal, "getVdb-" + vdbName, true); //$NON-NLS-1$
            TeiidVdb vdb = getMetadataInstance().getVdb(vdbName);
            if (vdb == null)
                return commitNoVdbFound(uow, mediaTypes, vdbName);
            Repository repo = this.kengine.getDefaultRepository();
            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(VDB_EXPORT_XML_PROPERTY, mediaTypes.contains(MediaType.APPLICATION_XML_TYPE));
            RestMetadataVdb entity = entityFactory.createMetadataVdb(uow, repo, vdb, uriInfo.getBaseUri());
            LOGGER.debug("getVdb:VDB '{0}' entity was constructed", vdb.getName()); //$NON-NLS-1$
            return commit(uow, mediaTypes, entity);

        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch ( final Throwable e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_GET_VDB_ERROR, vdbName);
        }
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

    /**
     * Remove a Connection from the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the Connection name (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error removing the Connection
     */
    @DELETE
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Removes a Connection from the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeConnection(final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam(value = "Name of the connection to be removed", required = true)
                                     final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        String title = RelationalMessages.getString(RelationalMessages.Info.CONNECTION_DEPLOYMENT_STATUS_TITLE);
        KomodoStatusObject status = new KomodoStatusObject(title);

        try {
            uow = createTransaction(principal, "removeConnection", false); //$NON-NLS-1$

            if (! getMetadataInstance().dataSourceExists(connectionName)) {
                status.addAttribute(connectionName,
                                    RelationalMessages.getString(RelationalMessages.Error.METADATA_SERVICE_NO_CONNECTION_FOUND, connectionName));
                return commit(uow, mediaTypes, status);
            }

            getMetadataInstance().deleteDataSource(connectionName);

            // Await the undeployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            if (! hasDataSource(connectionName)) {
                status.addAttribute(connectionName,
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_SUCCESSFULLY_UNDEPLOYED, connectionName));
            } else
                status.addAttribute(connectionName,
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_UNDEPLOYMENT_REQUEST_SENT, connectionName));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_UNDEPLOY_CONNECTION_ERROR, connectionName);
        }
    }

    /**
     * Get the schema for a model in a deployed VDB
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param vdbName
     *        the id of the VDB  (cannot be empty)
     * @param modelName
     *        the id of the Model (cannot be empty)
     * @return the VDB model ddl (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem retrieving the schema
     */
    @GET
    @Path( V1Constants.VDBS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VDB_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.MODELS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.MODEL_PLACEHOLDER + StringConstants.FORWARD_SLASH +
           V1Constants.SCHEMA_SEGMENT )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get schema for a VDB Model")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No vdb could be found with name"),
        @ApiResponse(code = 404, message = "No model could be found with name"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getVdbModelSchema( final @Context HttpHeaders headers,
                                       final @Context UriInfo uriInfo,
                                       @ApiParam(value = "Name of the vdb", required = true)
                                       final @PathParam( "vdbName" ) String vdbName,
                                       @ApiParam(value = "Name of the model", required = true)
                                       final @PathParam( "modelName" ) String modelName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            // Get the model schema
            uow = createTransaction(principal, "getModelSchema", true); //$NON-NLS-1$

            String schema = getSchema(uow, vdbName, modelName);

            KomodoStatusObject kso = new KomodoStatusObject("VdbModelSchema"); //$NON-NLS-1$
            kso.addAttribute("schema", schema); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);

        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch ( final Throwable e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.VDB_SERVICE_GET_VDB_ERROR, vdbName);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the translators deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.TRANSLATORS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of translators",
                            response = RestVdbTranslator[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getTranslators(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            // find translators
            uow = createTransaction(principal, "getTranslators", true); //$NON-NLS-1$

            Collection<TeiidTranslator> translators = getMetadataInstance().getTranslators();
            LOGGER.debug("getTranslators:found '{0}' Translators", translators.size()); //$NON-NLS-1$

            final List<RestMetadataVdbTranslator> entities = new ArrayList<>();

            for (TeiidTranslator translator : translators) {
                RestMetadataVdbTranslator entity = entityFactory.createMetadataTranslator(uow, repo, translator, uriInfo.getBaseUri());
                entities.add(entity);
                LOGGER.debug("getTranslators:Translator '{0}' entity was constructed", translator.getName()); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_TRANSLATORS_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the connections deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.CONNECTIONS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of connections",
                            response = RestConnection[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnections(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            uow = createTransaction(principal, "getConnections", true); //$NON-NLS-1$

            // Get teiid datasources
            Collection<TeiidDataSource> dataSources = getMetadataInstance().getDataSources();
            LOGGER.debug("getConnections:found '{0}' DataSources", dataSources.size()); //$NON-NLS-1$

            final List<RestMetadataConnection> entities = new ArrayList<>();

            for (TeiidDataSource dataSource : dataSources) {
                RestMetadataConnection entity = entityFactory.createMetadataDataSource(uow, repo, dataSource, uriInfo.getBaseUri());
                entities.add(entity);
                LOGGER.debug("getConnections:Data Source '{0}' entity was constructed", dataSource.getName()); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_DATA_SOURCES_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the id of the Connection being retrieved (cannot be empty)
     * @return the JSON representation of the Connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified connection or constructing the JSON representation
     */
    @GET
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER )
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Find connection by name", response = RestConnection.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnection( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(value = "Name of the connection", required = true)
                                   final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
        	Repository repo = this.kengine.getDefaultRepository();
            // find DataSource
            uow = createTransaction(principal, "getConnection-" + connectionName, true); //$NON-NLS-1$
            TeiidDataSource dataSource = getMetadataInstance().getDataSource(connectionName);
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            final RestMetadataConnection restDataSource = entityFactory.createMetadataDataSource(uow, repo, dataSource, uriInfo.getBaseUri());
            LOGGER.debug("getConnection:Datasource '{0}' entity was constructed", dataSource.getName()); //$NON-NLS-1$
            return commit( uow, mediaTypes, restDataSource );

        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch ( final Throwable e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_DATA_SOURCE_ERROR, connectionName);
        }
    }

    /**
     * Return the default translator to be used for a Connection
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param connectionName
     *        the id of the Connection being retrieved (cannot be empty)
     * @return the translator for the connection (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem finding the specified connection
     */
    @GET
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTION_PLACEHOLDER
           + StringConstants.FORWARD_SLASH + V1Constants.TRANSLATOR_DEFAULT_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get the default translator recommended for a connection")
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Connection could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionDefaultTranslator( final @Context HttpHeaders headers,
                                                    final @Context UriInfo uriInfo,
                                                    @ApiParam(value = "Id of the connection", required = true)
                                                    final @PathParam( "connectionName" ) String connectionName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            // find Connection
            uow = createTransaction(principal, "getConnectionDefaultTranslator-" + connectionName, true); //$NON-NLS-1$
            TeiidDataSource dataSource = getMetadataInstance().getDataSource(connectionName);
            if (dataSource == null)
                return commitNoConnectionFound(uow, mediaTypes, connectionName);

            // Get the driver name for the source
            String driverName = dataSource.getType();

            // Get the translator name from the default driver - translator mappings
            String translatorName = driverTranslatorMap.get(driverName);

            // If translator not found using driver mappings, use the connection url if available.
            // The urlContentTranslatorMap keys are unique strings within the connection url which would identify the required translator
            if(translatorName==null) {
                String connectionUrl = dataSource.getPropertyValue("connection-url"); //$NON-NLS-1$
                // No connection url property - unknown translator
                if(connectionUrl == null || connectionUrl.isEmpty()) {
                    translatorName = UNKNOWN_TRANSLATOR;
                // Connection url property found - use mappings to get translator, if possible
                } else {
                    for(String contentKey : urlContentTranslatorMap.keySet()) {
                        if(connectionUrl.contains(contentKey)) {
                            translatorName = urlContentTranslatorMap.get(contentKey);
                            break;
                        }
                    }
                    if(translatorName==null) {
                        translatorName = UNKNOWN_TRANSLATOR;
                    }
                }
            }

            // Return a status object with the translator
            KomodoStatusObject kso = new KomodoStatusObject();
            kso.addAttribute("Translator", translatorName); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_DATA_SOURCE_TRANSLATOR_ERROR, connectionName);
        }
    }

    /**
     * Copy  connections from the server into the workspace that are not present in the workspace
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON representation of the status of the copying (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error copying the connections
     */
    @POST
    @Path( V1Constants.CONNECTIONS_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.CONNECTIONS_FROM_TEIID )
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Copy Connections from the server into the workspace")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response copyConnectionsIntoRepo( final @Context HttpHeaders headers,
                                      final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            // find Connections
            uow = createTransaction(principal, "connectionsFromTeiid", false); //$NON-NLS-1$
            Collection<TeiidDataSource> teiidConns = getMetadataInstance().getDataSources();

            // Get current list of workspace Connections
            final WorkspaceManager mgr = getWorkspaceManager(uow);
            Connection[] workspaceConns = mgr.findConnections( uow );
            List<String> workspaceConnNames = new ArrayList<String>(workspaceConns.length);

            String title = RelationalMessages.getString(RelationalMessages.Info.CONNECTION_TO_REPO_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            // Remove any connections that dont belong to user.  Compile list of remaining workspace connections
            for(Connection workspaceConn : workspaceConns) {
                // Connections not belonging to user are removed
                if(workspaceConn.hasProperty(uow, DSB_PROP_SERVICE_SOURCE)) {
                    String owner = workspaceConn.getProperty(uow, DSB_PROP_SERVICE_SOURCE).getStringValue(uow);
                    if(! uow.getUserName().equals(owner)) {
                        mgr.delete(uow, workspaceConn);
                        continue;
                    }
                }

                workspaceConnNames.add(workspaceConn.getName(uow));
            }

            // Copy the teiid connection into the workspace, if no workspace connection with the same name
            for(TeiidDataSource teiidConn : teiidConns) {
                String name = teiidConn.getName();

                if(workspaceConnNames.contains(name))
                    continue;

                final Connection connection = getWorkspaceManager(uow).createConnection( uow, null, name);
                final RestMetadataConnection teiidConnEntity = entityFactory.createMetadataDataSource(uow, repo, teiidConn, uriInfo.getBaseUri());

                setProperties(uow, connection, teiidConnEntity);
            }

            status.addAttribute("copyConnsToRepo", RelationalMessages.getString(RelationalMessages.Info.CONNECTION_TO_REPO_SUCCESS)); //$NON-NLS-1$
           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.CONNECTION_TO_REPO_IMPORT_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the drivers deployed to teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the JSON document
     */
    @GET
    @Path(V1Constants.DRIVERS_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of drivers available in teiid",
                            response = RestConnectionDriver[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDrivers(final @Context HttpHeaders headers,
                               final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            // find drivers
            uow = createTransaction(principal, "getDrivers", true); //$NON-NLS-1$

            Collection<ConnectionDriver> drivers = getMetadataInstance().getDataSourceDrivers();
            LOGGER.debug("getDrivers:found '{0}' Drivers", drivers.size()); //$NON-NLS-1$

            final List<RestConnectionDriver> entities = new ArrayList<>();

            for (ConnectionDriver driver : drivers) {
                RestConnectionDriver entity = new RestConnectionDriver();
                entity.setName(driver.getName());
                entities.add(entity);
                LOGGER.debug("getDrivers:Driver '{0}' entity was constructed", driver.getName()); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
            return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_DRIVERS_ERROR);
        }
    }

    /**
     * Adds (deploys) a Driver to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param driverAttributes
     *        the file attributes (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the Driver
     */
    @POST
    @Path(V1Constants.METADATA_DRIVER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Add a driver to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addDriver(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "" +
                                                     "JSON of the properties of the driver to add:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "name: \"name of the driver\"" + COMMA + BR +
                                                     NBSP + "content: \"Base64-encoded byte data of the" + COMMA + BR +
                                                     NBSP + "driver file\"" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
                                   final String driverAttributes) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;
        String driverName = null;
        byte[] driverContent = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            uow = createTransaction(principal, "deployTeiidDriver", false); //$NON-NLS-1$

            if (driverAttributes.contains(KomodoPathAttribute.PATH_LABEL)) {
                // Is a workspace path to a driver
                try {
                    KomodoPathAttribute kpa = KomodoJsonMarshaller.unmarshall(driverAttributes, KomodoPathAttribute.class);
                    if (kpa.getPath() == null) {
                        return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_DRIVER_MISSING_PATH);
                    }

                    List<KomodoObject> results = repo.searchByPath(uow, kpa.getPath());
                    if (results.size() == 0) {
                        return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_DRIVER_FOUND_IN_WKSP, kpa.getPath());
                    }

                    Driver driver = getWorkspaceManager(uow).resolve(uow, results.get(0), Driver.class);
                    if (driver == null) {
                        return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_DRIVER_FOUND_IN_WKSP, kpa.getPath());
                    }

                    driverName = driver.getName(uow);
                    driverContent = FileUtils.streamToByteArray(driver.getContent(uow));

                } catch (Exception ex) {
                    return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.METADATA_SERVICE_REQUEST_PARSING_ERROR);
                }

            } else {
                // Is a set of file attributes for file-based with content encoded
                try {
                    KomodoFileAttributes kfa = KomodoJsonMarshaller.unmarshall(driverAttributes, KomodoFileAttributes.class);
                    Response response = checkFileAttributes(kfa, mediaTypes);
                    if (response.getStatus() != Status.OK.getStatusCode())
                        return response;

                    driverName = kfa.getName();
                    driverContent = decode(kfa.getContent());

                } catch (Exception ex) {
                    return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.METADATA_SERVICE_REQUEST_PARSING_ERROR);
                }
            }

            if (driverName == null || driverContent == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_DRIVER_ATTRIBUTES_MISSING);
            }

            String tempDir = FileUtils.tempDirectory();
            String fileName = driverContent.hashCode() + DOT + driverName;
            File driverFile = new File(tempDir, fileName);
            FileUtils.write(driverContent, driverFile);

            getMetadataInstance().deployDataSourceDriver(driverName, driverFile);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());

            if (hasDriver(driverName)) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_DEPLOYED));
            } else
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UPLOADED));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_DEPLOY_DRIVER_ERROR, driverName);
        }
    }

    /**
     * Remove a Driver from the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param driverName
     *        the driver name (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error removing the Driver
     */
    @DELETE
    @Path(V1Constants.METADATA_DRIVER + StringConstants.FORWARD_SLASH +
                  V1Constants.METADATA_DRIVER_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Removes a driver from the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeDriver(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(value = "Name of the driver to be removed", required = true)
                                    final @PathParam( "driverName" ) String driverName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "unDeployTeiidDriver", false); //$NON-NLS-1$

            getMetadataInstance().undeployDataSourceDriver(driverName);

            // Await the undeployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            if (! hasDriver(driverName)) {
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UNDEPLOYED));
            } else
                status.addAttribute(driverName,
                                    RelationalMessages.getString(RelationalMessages.Info.DRIVER_UNDEPLOYMENT_REQUEST_SENT));

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_UNDEPLOY_DRIVER_ERROR, driverName);
        }
    }

    /**
     * Return Dataservice deployability status.
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param dataserviceName
     *        the id of the Dataservice being requested for deployment (cannot be empty)
     * @return the deployable status for the Dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem determining the status
     */
    @GET
    @Path( V1Constants.DATA_SERVICE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER
           + StringConstants.FORWARD_SLASH + V1Constants.DEPLOYABLE_STATUS_SEGMENT)
    @Produces( { MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML } )
    @ApiOperation(value = "Get deployable status for a dataservice", response = KomodoStatusObject.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON or XML is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getDataserviceDeployableStatus( final @Context HttpHeaders headers,
                                                    final @Context UriInfo uriInfo,
                                                    @ApiParam(value = "Id of the data service", required = true)
                                                    final @PathParam( "dataserviceName" ) String dataserviceName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        java.sql.Connection connection = null;

        try {
            uow = createTransaction(principal, "getDeployableStatus", true); //$NON-NLS-1$

            // Get the dataservice from the users repo
            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);


            // Get the serviceVDB name associated with the dataservice
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            String serviceVdbName = serviceVdb.getName(uow);

            // Get the already-deployed VDB on the server if available
            TeiidVdb serverVdb = getMetadataInstance().getVdb(serviceVdbName);

            String deployableStatusMessage = "OK"; //$NON-NLS-1$
            // If found, determine if there is a conflict with the VDB
            if( serverVdb != null ) {
                String serverVdbOwner = serverVdb.getPropertyValue(DSB_PROP_OWNER);
                if(serverVdbOwner != null) {
                    // server vdb owner is different than the current user.
                    if(!serverVdbOwner.equals(uow.getUserName())) {
                        deployableStatusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_ALREADY_DEPLOYED_OWNER, serviceVdbName, serverVdbOwner);
                    }
                } else {
                    deployableStatusMessage = RelationalMessages.getString(RelationalMessages.Info.VDB_ALREADY_DEPLOYED, serviceVdbName);
                }
            }

            // The KomodoStatusObject returns 'OK' if OK to deploy, otherwise a message describing the issue
            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_DEPLOYABLE_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);
            status.addAttribute("deployableStatus", deployableStatusMessage); //$NON-NLS-1$

           return commit(uow, mediaTypes, status);
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_DATA_SERVICE_DEPLOYABLE_ERROR, dataserviceName);
        } finally {
            try {
                if(connection!=null) {
                    connection.close();
                }
            } catch (SQLException ex) {
            }
        }
   }

    /**
     * Adds (deploys) a Dataservice to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pathAttribute
     *        the path (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the Dataservice
     */
    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.DATA_SERVICE_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Deploy the data service to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addDataservice(final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "" +
                                                     "JSON of the properties of the data service:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "path: \"location of the data service in the workspace\"" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
                                   final String pathAttribute)
                                   throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        //
        // Error if there is no path attribute defined
        //
        KomodoPathAttribute kpa;
        try {
            kpa = KomodoJsonMarshaller.unmarshall(pathAttribute, KomodoPathAttribute.class);
            if (kpa.getPath() == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_DATA_SERVICE_MISSING_PATH);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.METADATA_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            uow = createTransaction(principal, "deployTeiidDataservice", false); //$NON-NLS-1$

            List<KomodoObject> dataServices = repo.searchByPath(uow, kpa.getPath());
            if (dataServices.size() == 0) {
                return createErrorResponseWithForbidden(mediaTypes,
                                                        RelationalMessages.Error.METADATA_SERVICE_NO_DATA_SERVICE_FOUND,
                                                        StringUtils.getLastToken(kpa.getPath(), FORWARD_SLASH));
            }

            Dataservice dataService = getWorkspaceManager(uow).resolve(uow, dataServices.get(0), Dataservice.class);
            if (dataService == null) {
                return createErrorResponseWithForbidden(mediaTypes,
                                                        RelationalMessages.Error.METADATA_SERVICE_NO_DATA_SERVICE_FOUND,
                                                        StringUtils.getLastToken(kpa.getPath(), FORWARD_SLASH));
            }

            //
            // Deploy the data service
            //
            DeployStatus deployStatus = dataService.deploy(uow);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            List<String> progressMessages = deployStatus.getProgressMessages();
            for (int i = 0; i < progressMessages.size(); ++i) {
                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
            }

            if (deployStatus.ok()) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(dataService.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_SUCCESSFULLY_DEPLOYED));
            } else {
                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                List<String> errorMessages = deployStatus.getErrorMessages();
                for (int i = 0; i < errorMessages.size(); ++i) {
                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
                }

                status.addAttribute(dataService.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.DATA_SERVICE_DEPLOYED_WITH_ERRORS));
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_DEPLOY_DATA_SERVICE_ERROR);
        }
    }

    /**
     * Adds (deploys) a Connection to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pathAttribute
     *        the path (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the Connection
     */
    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.CONNECTION_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Deploy the connection to the teiid server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addConnection( final @Context HttpHeaders headers,
                                   final @Context UriInfo uriInfo,
                                   @ApiParam(
                                             value = "" +
                                                     "JSON of the properties of the connection:<br>" +
                                                     OPEN_PRE_TAG +
                                                     OPEN_BRACE + BR +
                                                     NBSP + "path: \"location of the connection in the workspace\"" + BR +
                                                     CLOSE_BRACE +
                                                     CLOSE_PRE_TAG,
                                             required = true
                                   )
                                   final String pathAttribute)
                                   throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        //
        // Error if there is no path attribute defined
        //
        KomodoPathAttribute kpa;
        try {
            kpa = KomodoJsonMarshaller.unmarshall(pathAttribute, KomodoPathAttribute.class);
            if (kpa.getPath() == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_CONNECTION_MISSING_PATH);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.METADATA_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            uow = createTransaction(principal, "deployTeiidConnection", false); //$NON-NLS-1$

            List<KomodoObject> connections = repo.searchByPath(uow, kpa.getPath());
            if (connections.size() == 0) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_CONNECTION_FOUND);
            }

            Connection connection = getWorkspaceManager(uow).resolve(uow, connections.get(0), Connection.class);
            if (connection == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_CONNECTION_FOUND);
            }

            //
            // Deploy the connection
            //
            DeployStatus deployStatus = connection.deploy(uow);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.CONNECTION_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            List<String> progressMessages = deployStatus.getProgressMessages();
            for (int i = 0; i < progressMessages.size(); ++i) {
                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
            }

            if (deployStatus.ok()) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(connection.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_SUCCESSFULLY_DEPLOYED));
            } else {
                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                List<String> errorMessages = deployStatus.getErrorMessages();
                for (int i = 0; i < errorMessages.size(); ++i) {
                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
                }

                status.addAttribute(connection.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.CONNECTION_DEPLOYED_WITH_ERRORS));
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_DEPLOY_CONNECTION_ERROR, kpa.getPath());
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
     * Adds (deploys) a VDB to the server
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param pathAttribute
     *        the path attribute (never <code>null</code>)
     * @return a JSON representation of the status (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is an error adding the VDB
     */
    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.VDB_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @Consumes ( { MediaType.APPLICATION_JSON } )
    @ApiOperation(value = "Deploy the Vdb to the metadata server")
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response addVdb(final @Context HttpHeaders headers,
                           final @Context UriInfo uriInfo,
                           @ApiParam(
                                     value = "" +
                                             "JSON of the properties of the vdb:<br>" +
                                             OPEN_PRE_TAG +
                                             OPEN_BRACE + BR +
                                             NBSP + "path: \"location of the data service in the workspace\"" + BR +
                                             CLOSE_BRACE +
                                             CLOSE_PRE_TAG,
                                     required = true
                           )
                           final String pathAttribute)
                           throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        //
        // Error if there is no path attribute defined
        //
        KomodoPathAttribute kpa;
        try {
            kpa = KomodoJsonMarshaller.unmarshall(pathAttribute, KomodoPathAttribute.class);
            if (kpa.getPath() == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_VDB_MISSING_PATH);
            }
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.METADATA_SERVICE_REQUEST_PARSING_ERROR);
        }

        UnitOfWork uow = null;
        try {
        	Repository repo = this.kengine.getDefaultRepository();
            uow = createTransaction(principal, "deployVdb", false); //$NON-NLS-1$

            List<KomodoObject> vdbs = repo.searchByPath(uow, kpa.getPath());
            if (vdbs.size() == 0) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_VDB_FOUND);
            }

            Vdb vdb = getWorkspaceManager(uow).resolve(uow, vdbs.get(0), Vdb.class);
            if (vdb == null) {
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.METADATA_SERVICE_NO_VDB_FOUND);
            }

            //
            // Deploy the VDB
            //
            DeployStatus deployStatus = vdb.deploy(uow);

            // Await the deployment to end
            Thread.sleep(DEPLOYMENT_WAIT_TIME);

            String title = RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYMENT_STATUS_TITLE);
            KomodoStatusObject status = new KomodoStatusObject(title);

            List<String> progressMessages = deployStatus.getProgressMessages();
            for (int i = 0; i < progressMessages.size(); ++i) {
                status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
            }

            if (deployStatus.ok()) {
                status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                status.addAttribute(vdb.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_SUCCESSFULLY_DEPLOYED));
            } else {
                status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                List<String> errorMessages = deployStatus.getErrorMessages();
                for (int i = 0; i < errorMessages.size(); ++i) {
                    status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
                }

                status.addAttribute(vdb.getName(uow),
                                    RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYED_WITH_ERRORS));
            }

           return commit(uow, mediaTypes, status);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponse(Status.FORBIDDEN, mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_DEPLOY_VDB_ERROR);
        }
    }

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

            // if workspace does not have preview vdb, then create it.
            if ( !wMgr.hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
            	wMgr.createVdb( uow, null, vdbName, vdbName );
            }

            // Get the preview VDB.
            final KomodoObject kobject = wMgr.getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
            Vdb previewVdb = wMgr.resolve( uow, kobject, Vdb.class );

            // Get the list of current preview VDB import names
            List<String> currentVdbImportNames = new ArrayList<String>();
            VdbImport[] currentVdbImports = previewVdb.getImports(uow);
            for( VdbImport vdbImport: currentVdbImports ) {
            	currentVdbImportNames.add(vdbImport.getName(uow));
            }

            // Get the current workspace connection VDB names
            List<String> connectionVdbNames = new ArrayList<String>();
            KomodoObject[] connVdbObjs = wMgr.getChildrenOfType(uow, VdbLexicon.Vdb.VIRTUAL_DATABASE, "*btlconn");
            for( KomodoObject kObj: connVdbObjs) {
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

            // The updated VDB is deployed if imports were added or removed
            if(importAdded || importRemoved) {
                //
                // Deploy the VDB
                //
                DeployStatus deployStatus = previewVdb.deploy(uow);

                // Await the deployment to end
                Thread.sleep(DEPLOYMENT_WAIT_TIME);

                String title = RelationalMessages.getString(RelationalMessages.Info.VDB_DEPLOYMENT_STATUS_TITLE);
                KomodoStatusObject status = new KomodoStatusObject(title);

                List<String> progressMessages = deployStatus.getProgressMessages();
                for (int i = 0; i < progressMessages.size(); ++i) {
                    status.addAttribute("ProgressMessage" + (i + 1), progressMessages.get(i));
                }

                if (deployStatus.ok()) {
                    status.addAttribute("deploymentSuccess", Boolean.TRUE.toString());
                    status.addAttribute(previewVdb.getName(uow),
                                        RelationalMessages.getString(RelationalMessages.Info.VDB_SUCCESSFULLY_DEPLOYED));
                } else {
                    status.addAttribute("deploymentSuccess", Boolean.FALSE.toString());
                    List<String> errorMessages = deployStatus.getErrorMessages();
                    for (int i = 0; i < errorMessages.size(); ++i) {
                        status.addAttribute("ErrorMessage" + (i + 1), errorMessages.get(i));
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

    private String extractServiceVdbName(UnitOfWork uow, WorkspaceManager mgr, String dsPath) throws KException {
    	Repository repo = this.kengine.getDefaultRepository();
        KomodoObject dsObject = repo.getFromWorkspace(uow, dsPath);
        if (dsObject == null)
            return null; // Not a path in the workspace

        Dataservice dService = mgr.resolve(uow, dsObject, Dataservice.class);
        if (dService == null)
            return null; // Not a data service

        Vdb vdb = dService.getServiceVdb(uow);
        if (vdb == null)
            return null;

        return vdb.getVdbName(uow);
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
        @ApiResponse(code = 403, message = "An error has occurred.")
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
            WorkspaceManager mgr = getWorkspaceManager(uow);
            String target = kqa.getTarget();
            String query = kqa.getQuery();

            //
            // Is target a deployed vdb or a dataservice in the workspace that has had its vdbs deployed?
            //
            String vdbName = extractServiceVdbName(uow, mgr, target);
            if (vdbName == null) {
                //
                // The target does not reference a data service in the workspace
                // or the data service has no service vdb. Either way target should
                // be applied directly to the query.
                //
                vdbName = target;
            }

            TeiidVdb vdb = getMetadataInstance().getVdb(vdbName);
            if (vdb == null) {
                return createErrorResponse(Status.FORBIDDEN, mediaTypes, RelationalMessages.Error.METADATA_SERVICE_QUERY_TARGET_NOT_DEPLOYED);
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

    /*
     * Loads driver name - translator mappings from resource file
     */
    private void loadDriverTranslatorMap() {
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream( DRIVER_TRANSLATOR_MAPPING_FILE );

        if(inputStream==null) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.METADATA_SERVICE_DEFAULT_TRANSLATOR_MAPPINGS_NOT_FOUND_ERROR));
            return;
        }

        driverTranslatorMap.clear();

        // Load the mappings file
        Document doc;
        try {
            String mappingXml = FileUtils.streamToString(inputStream);
            doc = FileUtils.createDocument(mappingXml);
        } catch (Exception ex) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.METADATA_SERVICE_LOAD_DEFAULT_TRANSLATOR_MAPPINGS_ERROR, ex.getLocalizedMessage()));
            return;
        }

        // Single child node contains the mappings
        final Node mappingsNode = doc.getChildNodes().item(0);
        if ( mappingsNode.getNodeType() != Node.ELEMENT_NODE ) {
            return;
        }

        // Iterate the doc nodes and populate the default translator map
        final NodeList translatorNodes = ((Element)mappingsNode).getElementsByTagName( ELEM_TRANSLATOR );
        for(int i=0; i<translatorNodes.getLength(); i++) {
            final Node translatorNode = translatorNodes.item(i);
            String driver = translatorNode.getAttributes().getNamedItem( ATTR_DRIVER ).getTextContent();
            String translator = translatorNode.getTextContent();
            driverTranslatorMap.put(driver, translator);
        }
    }

    /*
     * Loads URL content - translator mappings from resource file
     */
    private void loadUrlContentTranslatorMap() {
        InputStream inputStream = getClass().getClassLoader().getResourceAsStream( URLCONTENT_TRANSLATOR_MAPPING_FILE );

        if(inputStream==null) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.METADATA_SERVICE_DEFAULT_TRANSLATOR_MAPPINGS_NOT_FOUND_ERROR));
            return;
        }

        urlContentTranslatorMap.clear();

        // Load the mappings file
        Document doc;
        try {
            String mappingXml = FileUtils.streamToString(inputStream);
            doc = FileUtils.createDocument(mappingXml);
        } catch (Exception ex) {
            LOGGER.error(RelationalMessages.getString(RelationalMessages.Error.METADATA_SERVICE_LOAD_DEFAULT_TRANSLATOR_MAPPINGS_ERROR, ex.getLocalizedMessage()));
            return;
        }

        // Single child node contains the mappings
        final Node mappingsNode = doc.getChildNodes().item(0);
        if ( mappingsNode.getNodeType() != Node.ELEMENT_NODE ) {
            return;
        }

        // Iterate the doc nodes and populate the default translator map
        final NodeList translatorNodes = ((Element)mappingsNode).getElementsByTagName( ELEM_TRANSLATOR );
        for(int i=0; i<translatorNodes.getLength(); i++) {
            final Node translatorNode = translatorNodes.item(i);
            String urlContent = translatorNode.getAttributes().getNamedItem( ATTR_URLCONTENT ).getTextContent();
            String translator = translatorNode.getTextContent();
            urlContentTranslatorMap.put(urlContent, translator);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the connection templates available in teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Templates JSON document
     */
    @GET
    @Path(V1Constants.TEMPLATES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the collection of templates",
                            response = RestMetadataTemplate[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionTemplates(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            // find templates
            uow = createTransaction(principal, "getTemplates", true); //$NON-NLS-1$

            Set<String> templateNames = getMetadataInstance().getDataSourceTemplateNames();
            LOGGER.debug("getTemplates:found '{0}' Templates", templateNames.size()); //$NON-NLS-1$

            final List<RestMetadataTemplate> entities = new ArrayList<RestMetadataTemplate>();

            for (String template : templateNames) {
                Collection<TeiidPropertyDefinition> propertyDefns = getMetadataInstance().getTemplatePropertyDefns(template);
                RestMetadataTemplate entity = entityFactory.createMetadataTemplate(uow, repo, template, propertyDefns, uriInfo.getBaseUri());
                entities.add(entity);
                LOGGER.debug("getTemplates:Template '{0}' entity was constructed", template); //$NON-NLS-1$
            }

            // create response
            return commit(uow, mediaTypes, entities);
        } catch (CallbackTimeoutException ex) {
                return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_TEMPLATES_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the connection templates available in teiid (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Templates JSON document
     */
    @GET
    @Path(V1Constants.TEMPLATES_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.TEMPLATE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find connection template by name",
                            response = RestMetadataTemplate.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No template could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionTemplate(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo,
                                                   @ApiParam(value = "Name of the template", required = true)
                                                    final @PathParam( "templateName" ) String templateName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            // find template
            uow = createTransaction(principal, "getTemplates", true); //$NON-NLS-1$

            Set<String> templateNames = getMetadataInstance().getDataSourceTemplateNames();
            if (templateNames == null || templateNames.isEmpty())
                return commitNoTemplateFound(uow, mediaTypes, templateName);

            if (! templateNames.contains(templateName))
                return commitNoTemplateFound(uow, mediaTypes, templateName);

            Collection<TeiidPropertyDefinition> propertyDefns = getMetadataInstance().getTemplatePropertyDefns(templateName);
            RestMetadataTemplate restTemplate = entityFactory.createMetadataTemplate(uow, repo, templateName, propertyDefns, uriInfo.getBaseUri());
            LOGGER.debug("getConnectionTemplate:Template '{0}' entity was constructed", templateName); //$NON-NLS-1$
            return commit( uow, mediaTypes, restTemplate );

        } catch (CallbackTimeoutException ex) {
                return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_TEMPLATE_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing all the entry properties available in a teiid template (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the TemplateEntry JSON document
     */
    @GET
    @Path(V1Constants.TEMPLATES_SEGMENT + StringConstants.FORWARD_SLASH +
                  V1Constants.TEMPLATE_PLACEHOLDER + StringConstants.FORWARD_SLASH +
                  V1Constants.TEMPLATE_ENTRIES_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Find the template entries of the named template",
                            response = RestMetadataTemplateEntry[].class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "No template could be found with name"),
        @ApiResponse(code = 406, message = "Only JSON returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getConnectionTemplateEntries(final @Context HttpHeaders headers,
                                                   final @Context UriInfo uriInfo,
                                                   @ApiParam(value = "Name of the template", required = true)
                                                    final @PathParam( "templateName" ) String templateName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
        	Repository repo = this.kengine.getDefaultRepository();
            // find template
            uow = createTransaction(principal, "getTemplateEntries", true); //$NON-NLS-1$

            Set<String> templateNames = getMetadataInstance().getDataSourceTemplateNames();
            if (templateNames == null || templateNames.isEmpty())
                return commitNoTemplateFound(uow, mediaTypes, templateName);

            if (! templateNames.contains(templateName))
                return commitNoTemplateFound(uow, mediaTypes, templateName);

            List<TeiidPropertyDefinition> propertyDefns = new ArrayList<>();
            propertyDefns.addAll(getMetadataInstance().getTemplatePropertyDefns(templateName));

            Collections.sort(propertyDefns, new TeiidPropertyDefinitionComparator());
            List<String> priorityNames = Arrays.asList(PRIORITY_TEMPLATE_NAMES);

            List<RestMetadataTemplateEntry> entities = entityFactory.createMetadataTemplateEntry(uow, repo, propertyDefns, uriInfo.getBaseUri());

            for (RestMetadataTemplateEntry entity : entities) {
                if (priorityNames.contains(entity.getId())) {
                    //
                    // Appears some properties are being flagged as not required when they really should be,
                    // eg. derbyclient.jar -> connection-url
                    //
                    entity.setRequired(true);
                }
            }

            return commit(uow, mediaTypes, entities);

        } catch (CallbackTimeoutException ex) {
                return createTimeoutResponse(mediaTypes);
        } catch (Throwable e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.METADATA_SERVICE_GET_TEMPLATE_ENTRIES_ERROR);
        }
    }

	@GET
	@Path(V1Constants.SERVICE_CATALOG_SOURCES)
	@Produces(MediaType.APPLICATION_JSON)
	@ApiOperation(value = "Display the collection of a", response = RestSyndesisDataSource[].class)
	@ApiResponses(value = { @ApiResponse(code = 403, message = "An error has occurred.") })
	public Response getServiceCatalogSources(final @Context HttpHeaders headers, final @Context UriInfo uriInfo)
			throws KomodoRestException {

		SecurityPrincipal principal = checkSecurityContext(headers);
		if (principal.hasErrorResponse())
			return principal.getErrorResponse();

		List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
		UnitOfWork uow = null;

		try {
			Repository repo = this.kengine.getDefaultRepository();
			uow = createTransaction(principal, "availableSources", true); //$NON-NLS-1$

			// Get OpenShift based available data services
			Collection<SyndesisDataSource> dataSources = this.openshiftClient.getSyndesisSources(getAuthenticationToken());
			LOGGER.info("serviceCatalogSources '{0}' DataSources", dataSources.size()); //$NON-NLS-1$

			final List<RestSyndesisDataSource> entities = new ArrayList<>();

			for (SyndesisDataSource dataSource : dataSources) {
				RestSyndesisDataSource entity = entityFactory.createServiceCatalogDataSource(uow, repo,
						dataSource, uriInfo.getBaseUri());
				entities.add(entity);
				LOGGER.info("serviceCatalogSources:Data Source '{0}' entity was constructed", dataSource.getName()); //$NON-NLS-1$
			}
			// create response
			return commit(uow, mediaTypes, entities);
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
					RelationalMessages.Error.METADATA_GET_DATA_SOURCES_ERROR);
		}
	}

	/**
	 * Binds a Syndesis Data Service
	 *
	 * @param headers
	 *            the request headers (never <code>null</code>)
	 * @param uriInfo
	 *            the request URI information (never <code>null</code>)
	 * @param payload
	 *            the payload that contains the name of the service (never
	 *            <code>null</code>)
	 * @return a JSON representation of the status (never <code>null</code>)
	 * @throws KomodoRestException
	 *             if there is an error adding the Connection
	 */
	@SuppressWarnings("nls")
	@POST
	@Path(V1Constants.SERVICE_CATALOG_SOURCES)
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes({ MediaType.APPLICATION_JSON })
	@ApiOperation(value = "Create connection Teiid Engine based on Syndesis connection configuration")
	@ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
			@ApiResponse(code = 403, message = "An error has occurred.") })
	public Response bindSyndesisSource(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
			@ApiParam(value = "JSON of the properties of the connection:<br>" + OPEN_PRE_TAG + OPEN_BRACE + BR + NBSP
					+ "name: \"Name of the Syndesis Data Service\"" + BR + CLOSE_BRACE
					+ CLOSE_PRE_TAG, required = true) final String payload)
			throws KomodoRestException {

		SecurityPrincipal principal = checkSecurityContext(headers);
		if (principal.hasErrorResponse())
			return principal.getErrorResponse();

		List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
		if (!isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
			return notAcceptableMediaTypesBuilder().build();

		//
		// Error if there is no name attribute defined
		//
		KomodoSyndesisDataSourceAttributes attributes;
		try {
			attributes = KomodoJsonMarshaller.unmarshall(payload, KomodoSyndesisDataSourceAttributes.class);
			if (attributes.getName() == null) {
				return createErrorResponseWithForbidden(mediaTypes,
						RelationalMessages.Error.METADATA_DATA_SERVICE_BIND_MISSING_NAME);
			}
		} catch (Exception ex) {
			return createErrorResponseWithForbidden(mediaTypes, ex,
					RelationalMessages.Error.METADATA_DATA_SERVICE_BIND_PARSE_ERROR);
		}

		UnitOfWork uow = null;

		try {
			uow = createTransaction(principal, "bindServiceCatalogService", false); //$NON-NLS-1$
			this.openshiftClient.bindToSyndesisSource(getAuthenticationToken(), attributes.getName());
			String title = RelationalMessages.getString(
					RelationalMessages.Info.METADATA_SERVICE_CATALOG_DATA_SERVIVE_BIND_TITLE, attributes.getName());
			KomodoStatusObject status = new KomodoStatusObject(title);
			return commit(uow, mediaTypes, status);
		} catch (final Exception e) {
			if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
				uow.rollback();
			}
			if (e instanceof KomodoRestException) {
				throw (KomodoRestException) e;
			}
			return createErrorResponse(Status.FORBIDDEN, mediaTypes, e,
					RelationalMessages.Error.METADATA_DATA_SERVIVE_BIND_ERROR, e, attributes.getName());
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
        if (! uriInfo.getQueryParameters().containsKey("includeInProgressServices"))
            includeInProgressServices = true;

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            final List<RestVirtualizationStatus> entityList = new ArrayList<>();
            Collection<BuildStatus> statuses = this.openshiftClient.getVirtualizations(includeInProgressServices);
            for (BuildStatus status : statuses) {
                entityList.add(entityFactory.createBuildStatus(status, uriInfo.getBaseUri()));
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

            return commit(uow, mediaTypes, entityFactory.createBuildStatus(status, uriInfo.getBaseUri()));
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

        KomodoStatusObject status = new KomodoStatusObject("Logs for " + vdbName);

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            String log = this.openshiftClient.getVirtualizationLog(vdbName);
            status.addAttribute("log", log);

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
            return commit(uow, mediaTypes, entityFactory.createBuildStatus(status, uriInfo.getBaseUri()));
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
                    + "\"memor\"y:    \"(optional) Amount memory to allocate in MB (default 1024)\"" + BR
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
            Vdb vdb = findVdb(uow, payload.getName());
            if (vdb == null) {
                //
                // We don't have a vdb so maybe we have a dataservice instead
                // Find the dataservice's vdb to publish.
                //
                Dataservice dataservice = findDataservice(uow, payload.getName());
                if (dataservice == null) {
                    return createErrorResponse(Status.NOT_FOUND, mediaTypes, RelationalMessages.Error.VDB_NOT_FOUND);
                }

                vdb = dataservice.getServiceVdb(uow);
            }

            if (vdb == null) {
                return createErrorResponse(Status.NOT_FOUND, mediaTypes, RelationalMessages.Error.VDB_NOT_FOUND);
            }

            KomodoStatusObject status = new KomodoStatusObject();
            status.addAttribute("Publishing", "Operation initiated");

            final String vdbPath = vdb.getAbsolutePath();
            final OAuthCredentials creds = getAuthenticationToken();

            UnitOfWork publishUow = createTransaction(principal, "publish", true); //$NON-NLS-1$
            Vdb theVdb = new VdbImpl(publishUow, kengine.getDefaultRepository(), vdbPath);

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
            status.addAttribute("Vdb Name", buildStatus.vdbName());
            status.addAttribute("Build Status", buildStatus.status().name());
            status.addAttribute("Build Status Message", buildStatus.statusMessage());

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
}
