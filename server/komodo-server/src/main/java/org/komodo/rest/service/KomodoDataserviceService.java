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

import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_DELETE_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DATASERVICES_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_GET_DATASERVICE_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_NAME_EXISTS;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_NAME_VALIDATION_ERROR;
import static org.komodo.rest.relational.RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_NAME_ERROR;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.openshift.ProtocolType;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.relational.WorkspaceManager;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.TeiidSqlConstants;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining Dataservice information from the workspace.
 */
@Component
@Path(V1Constants.WORKSPACE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICES_SEGMENT)
@Api(tags = { V1Constants.DATA_SERVICES_SEGMENT })
public final class KomodoDataserviceService extends KomodoService
        implements TeiidSqlConstants.Tokens, TeiidSqlConstants.Phrases {

    private static final int ALL_AVAILABLE = -1;

    private static final StringNameValidator VALIDATOR = new StringNameValidator();

    @Autowired
    private TeiidOpenShiftClient openshiftClient;

    /**
     * Get the Dataservices from the komodo repository
     *
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @return a JSON document representing all the Dataservices in the Komodo
     *         workspace (never <code>null</code>)
     * @throws KomodoRestException
     *             if there is a problem constructing the Dataservices JSON document
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Return the collection of data services", response = RestDataservice[].class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = QueryParamKeys.PATTERN, value = "A regex expression used when searching. If not present, all objects are returned.", required = false, dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = QueryParamKeys.SIZE, value = "The number of objects to return. If not present, all objects are returned", required = false, dataType = "integer", paramType = "query"),
            @ApiImplicitParam(name = QueryParamKeys.START, value = "Index of the first dataservice to return", required = false, dataType = "integer", paramType = "query") })
    @ApiResponses(value = { @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response getDataservices(final @Context HttpHeaders headers, final @Context UriInfo uriInfo)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            final String searchPattern = uriInfo.getQueryParameters().getFirst(QueryParamKeys.PATTERN);

            // find Data services
            uow = createTransaction(principal, "getDataservices", true); //$NON-NLS-1$
            Dataservice[] dataServices = getWorkspaceManager(uow).findDataservices(uow, searchPattern);

            int start = 0;

            { // start query parameter
                final String qparam = uriInfo.getQueryParameters().getFirst(QueryParamKeys.START);

                if (qparam != null) {

                    try {
                        start = Integer.parseInt(qparam);

                        if (start < 0) {
                            start = 0;
                        }
                    } catch (final Exception e) {
                        start = 0;
                    }
                }
            }

            int size = ALL_AVAILABLE;

            { // size query parameter
                final String qparam = uriInfo.getQueryParameters().getFirst(QueryParamKeys.SIZE);

                if (qparam != null) {

                    try {
                        size = Integer.parseInt(qparam);

                        if (size <= 0) {
                            size = ALL_AVAILABLE;
                        }
                    } catch (final Exception e) {
                        size = ALL_AVAILABLE;
                    }
                }
            }

            final List<RestDataservice> entities = new ArrayList<>();
            int i = 0;

            KomodoProperties properties = new KomodoProperties();
            for (final Dataservice dataService : dataServices) {
                if ((start == 0) || (i >= start)) {
                    if ((size == ALL_AVAILABLE) || (entities.size() < size)) {
                        RestDataservice entity = create(dataService, uriInfo.getBaseUri(), uow,
                                properties);
                        
                        // Set published status of dataservice
                        BuildStatus status = this.openshiftClient.getVirtualizationStatus(dataService.getServiceVdb(uow).getName(uow));
                        entity.setPublishedState(status.status().name());
                        entity.setPublishPodName(status.publishPodName());
                        entity.setPodNamespace(status.namespace());
                        entity.setOdataHostName(getOdataHost(status));

                        entities.add(entity);
                        LOGGER.debug("getDataservices:Dataservice '{0}' entity was constructed", //$NON-NLS-1$
                                dataService.getName(uow));
                    } else {
                        break;
                    }
                }

                ++i;
            }

            // create response
            return commit(uow, mediaTypes, entities);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_GET_DATASERVICES_ERROR);
        }
    }

    /**
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param dataserviceName
     *            the id of the Dataservice being retrieved (cannot be empty)
     * @return the JSON representation of the Dataservice (never <code>null</code>)
     * @throws KomodoRestException
     *             if there is a problem finding the specified workspace Dataservice
     *             or constructing the JSON representation
     */
    @GET
    @Path(V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces({ MediaType.APPLICATION_JSON })
    @ApiOperation(value = "Find dataservice by name", response = RestDataservice.class)
    @ApiResponses(value = { @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
            @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response getDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Id of the dataservice to be fetched, ie. the value of the 'keng__id' property", required = true) final @PathParam("dataserviceName") String dataserviceName)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "getDataservice", true); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            KomodoProperties properties = new KomodoProperties();
            final RestDataservice restDataservice = create(dataservice, uriInfo.getBaseUri(), uow,
                    properties);
            
            // Set published status of dataservice
            BuildStatus status = this.openshiftClient.getVirtualizationStatus(dataservice.getServiceVdb(uow).getName(uow));
            restDataservice.setPublishedState(status.status().name());
            restDataservice.setPublishPodName(status.publishPodName());
            restDataservice.setPodNamespace(status.namespace());
            restDataservice.setOdataHostName(getOdataHost(status));
            
            LOGGER.debug("getDataservice:Dataservice '{0}' entity was constructed", dataservice.getName(uow)); //$NON-NLS-1$
            return commit(uow, mediaTypes, restDataservice);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_GET_DATASERVICE_ERROR,
                    dataserviceName);
        }
    }

    private RestDataservice create(Dataservice dataservice, URI baseUri, UnitOfWork uow, KomodoProperties properties) throws KException {
    	return new RestDataservice(baseUri, dataservice, false, uow);
	}

	/**
     * Create a new DataService in the komodo repository
     *
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param dataserviceName
     *            the dataservice name (cannot be empty)
     * @param dataserviceJson
     *            the dataservice JSON representation (cannot be <code>null</code>)
     * @return a JSON representation of the new dataservice (never
     *         <code>null</code>)
     * @throws KomodoRestException
     *             if there is an error creating the DataService
     */
    @POST
    @Path(FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Create a dataservice in the workspace", consumes = MediaType.APPLICATION_JSON)
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response createDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service", required = true) final @PathParam("dataserviceName") String dataserviceName,
            @ApiParam(value = "" + "JSON of the properties of the new data service:<br>" + OPEN_PRE_TAG + OPEN_BRACE
                    + BR + NBSP + "keng\\_\\_id: \"id of the data service\"" + COMMA + BR + NBSP + OPEN_PRE_CMT
                    + "(identical to dataserviceName parameter)" + CLOSE_PRE_CMT + BR + BR + NBSP
                    + "keng\\_\\_dataPath: \"path of dataservice\"" + COMMA + BR + NBSP
                    + "tko__description: \"the description\"" + BR + CLOSE_BRACE
                    + CLOSE_PRE_TAG, required = true) final String dataserviceJson)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (!isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the dataservice name is missing
        if (StringUtils.isBlank(dataserviceName)) {
            return createErrorResponseWithForbidden(mediaTypes,
                    RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_MISSING_NAME);
        }

        final RestDataservice restDataservice = KomodoJsonMarshaller.unmarshall(dataserviceJson, RestDataservice.class);
        final String jsonDataserviceName = restDataservice.getId();
        // Error if the name is missing from the supplied json body
        if (StringUtils.isBlank(jsonDataserviceName)) {
            return createErrorResponseWithForbidden(mediaTypes,
                    RelationalMessages.Error.DATASERVICE_SERVICE_JSON_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataserviceName.equals(jsonDataserviceName);
        if (!namesMatch) {
            return createErrorResponseWithForbidden(mediaTypes, DATASERVICE_SERVICE_SERVICE_NAME_ERROR, dataserviceName,
                    jsonDataserviceName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "createDataservice", false); //$NON-NLS-1$

            // Error if the repo already contains a dataservice with the supplied name.
            Dataservice existing = getWorkspaceManager(uow).findDataservice(uow, dataserviceName);
            if (existing != null) {
                return createErrorResponseWithForbidden(mediaTypes,
                        RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_ALREADY_EXISTS);
            }

            // create new Dataservice
            return doAddDataservice(uow, uriInfo.getBaseUri(), mediaTypes, restDataservice);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR,
                    dataserviceName);
        }
    }

    private Response doAddDataservice(final UnitOfWork uow, final URI baseUri, final List<MediaType> mediaTypes,
            final RestDataservice restDataservice) throws KomodoRestException {
        assert (!uow.isRollbackOnly());
        assert (uow.getState() == State.NOT_STARTED);
        assert (restDataservice != null);

        final String dataserviceName = restDataservice.getId();
        try {
            final Dataservice dataservice = getWorkspaceManager(uow).createDataservice(uow, dataserviceName);

            // Transfers the properties from the rest object to the created komodo service.
            setProperties(uow, dataservice, restDataservice);

            String serviceVdbName = dataserviceName.toLowerCase() + SERVICE_VDB_SUFFIX;
            WorkspaceManager wkspMgr = getWorkspaceManager(uow);

            // Find the service VDB definition for this Dataservice. If one exists already,
            // it is replaced.
            dataservice.setServiceVdb(uow, null);
            Vdb svcVdbObj = wkspMgr.findVdb(uow, serviceVdbName);
            if (svcVdbObj != null) {
                wkspMgr.deleteVdb(uow, svcVdbObj);
            }

            Vdb serviceVdb = wkspMgr.createVdb(uow, serviceVdbName, serviceVdbName);

            // Add to the ServiceVdb a virtual model for the View
            Model viewModel = serviceVdb.addModel(uow, SERVICE_VDB_VIEW_MODEL);
            viewModel.setModelType(uow, Type.VIRTUAL);

            dataservice.setServiceVdb(uow, serviceVdb);

            KomodoStatusObject kso = new KomodoStatusObject("Create Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Successfully created"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            throw new KomodoRestException(
                    RelationalMessages.getString(DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR, dataserviceName), e);
        }
    }

    // Sets Dataservice properties using the supplied RestDataservice object
    private void setProperties(final UnitOfWork uow, Dataservice dataService, RestDataservice restDataService)
            throws KException {
        // 'New' = requested RestDataservice properties
        String newDescription = restDataService.getDescription();

        // 'Old' = current Dataservice properties
        String oldDescription = dataService.getDescription(uow);

        // Description
        if (!StringUtils.equals(newDescription, oldDescription)) {
            dataService.setDescription(uow, newDescription);
        }
    }

    /**
     * Delete the specified Dataservice from the komodo repository
     *
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param dataserviceName
     *            the name of the data service to remove (cannot be
     *            <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException
     *             if there is a problem performing the delete
     */
    @DELETE
    @Path("{dataserviceName}")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Delete a dataservice from the workspace")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response deleteDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service to be deleted", required = true) final @PathParam("dataserviceName") String dataserviceName)
            throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeDataserviceFromWorkspace", false); //$NON-NLS-1$

            final WorkspaceManager wkspMgr = getWorkspaceManager(uow);
            

            final Dataservice dataservice = wkspMgr.findDataservice(uow, dataserviceName);
            
            // Error if the specified service does not exist
            if (dataservice == null) {
                return createErrorResponseWithForbidden(mediaTypes,
                        RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_DNE);
            }

            // Delete the Dataservice serviceVDB if found
            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            String vdbName = serviceVdb.getName(uow);

            if (serviceVdb != null) {
                wkspMgr.deleteVdb(uow, serviceVdb);
            }

            // Delete the Dataservice
            wkspMgr.deleteDataservice(uow, dataservice);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Successfully deleted"); //$NON-NLS-1$

            // delete any view editor states of that virtualization
            final String viewEditorIdPrefix = KomodoService.getViewEditorStateIdPrefix(vdbName) + "*"; //$NON-NLS-1$
            final ViewEditorState[] editorStates = getViewEditorStates(uow, viewEditorIdPrefix);

            if (editorStates.length != 0) {
                for (final ViewEditorState editorState : editorStates) {
                    removeEditorState(uow, editorState);
                }

                kso.addAttribute(vdbName, "Successfully deleted " + editorStates.length + " saved editor states"); //$NON-NLS-1$ //$NON-NLS-2$
            }

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, DATASERVICE_SERVICE_DELETE_DATASERVICE_ERROR);
        }
    }

    /**
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param dataserviceName
     *            the data service name being validated (cannot be empty)
     * @return the response (never <code>null</code>) with an entity that is either
     *         an empty string, when the name is valid, or an error message
     * @throws KomodoRestException
     *             if there is a problem validating the data service name or
     *             constructing the response
     */
    @GET
    @Path(V1Constants.NAME_VALIDATION_SEGMENT + FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces({ MediaType.TEXT_PLAIN })
    @ApiOperation(value = "Returns an error message if the data service name is invalid")
    @ApiResponses(value = {
            @ApiResponse(code = 400, message = "The URI cannot contain encoded slashes or backslashes."),
            @ApiResponse(code = 403, message = "An unexpected error has occurred."),
            @ApiResponse(code = 500, message = "The dataservice name cannot be empty.") })
    public Response validateDataserviceName(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "The dataservice name being checked", required = true) final @PathParam("dataserviceName") String dataserviceName)
            throws KomodoRestException {

        final SecurityPrincipal principal = checkSecurityContext(headers);

        if (principal.hasErrorResponse()) {
            return principal.getErrorResponse();
        }

        final String errorMsg = VALIDATOR.checkValidName(dataserviceName);

        // a name validation error occurred
        if (errorMsg != null) {
            final Response response = Response.ok().entity(errorMsg).build();
            return response;
        }

        // check for duplicate name
        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "validateDataserviceName", true); //$NON-NLS-1$
            final Dataservice service = findDataservice(uow, dataserviceName);

            if (service == null) {
            	return Response.ok().build();
            }

            // name is a duplicate
            return Response.ok().entity(RelationalMessages.getString(DATASERVICE_SERVICE_NAME_EXISTS)).build();
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            return createErrorResponseWithForbidden(headers.getAcceptableMediaTypes(), e,
                    DATASERVICE_SERVICE_NAME_VALIDATION_ERROR);
        }
    }

    /**
     * Refresh the dataservice views, using the userProfile ViewDefinitions
     *
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param dataserviceName
     *            the dataservice name (cannot be empty)
     * @return a JSON representation of the new connection (never <code>null</code>)
     * @throws KomodoRestException
     *             if there is an error creating the Connection
     */
    @POST
    @Path(StringConstants.FORWARD_SLASH + V1Constants.REFRESH_DATASERVICE_VIEWS + StringConstants.FORWARD_SLASH
            + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Refresh the dataservice views from user profile states")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response refreshViews(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the dataservice", required = true) final @PathParam("dataserviceName") String dataserviceName)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (!isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the dataservice name is missing
        if (StringUtils.isBlank(dataserviceName)) {
            return createErrorResponseWithForbidden(mediaTypes,
                    RelationalMessages.Error.DATASERVICE_SERVICE_REFRESH_VIEWS_MISSING_NAME);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "refreshDataserviceViews", false); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            Vdb serviceVdb = dataservice.getServiceVdb(uow);
            String vdbName = serviceVdb.getName(uow);

            // Get all of the editor states from the user profile
            // They are stored under ids of form "serviceVdbName.viewName"
            final String viewEditorIdPrefix = KomodoService.getViewEditorStateIdPrefix(vdbName) + "*"; //$NON-NLS-1$
            final ViewEditorState[] editorStates = getViewEditorStates(uow, viewEditorIdPrefix);

            getWorkspaceManager(uow).refreshServiceVdb(uow, serviceVdb, editorStates);

            KomodoStatusObject kso = new KomodoStatusObject("Refresh Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "View Successfully refreshed"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e,
                    RelationalMessages.Error.DATASERVICE_SERVICE_REFRESH_VIEWS_ERROR, dataserviceName);
        }
    }

    /**
     * Get OData hostname from the buildStatus
     * @param buildStatus the BuildStatus
     * @return the odata hostname
     */
    private String getOdataHost(final BuildStatus buildStatus) {
    	String odataHost = null;
    	if(buildStatus != null) {
            List<RouteStatus> routeStatuses = buildStatus.routes();
            if(!routeStatuses.isEmpty()) {
            	// Find Odata route if it exists
            	for(RouteStatus routeStatus: routeStatuses) {
            		if(routeStatus.getKind() == ProtocolType.ODATA) {
            			odataHost = routeStatus.getHost();
            			break;
            		}
            	}
            }
    	}
    	return odataHost;
    }

    /**
     * Update the specified Dataservice from the komodo repository
     *
     * @param headers         the request headers (never <code>null</code>)
     * @param uriInfo         the request URI information (never <code>null</code>)
     * @param dataserviceName the dataservice name (cannot be empty)
     * @param dataserviceJson the dataservice JSON representation (cannot be
     *                        <code>null</code>)
     * @return a JSON representation of the new connection (never <code>null</code>)
     * @throws KomodoRestException if there is an error creating the Connection
     */
    @PUT
    @Path(FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Update data service")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response updateDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service", required = true) final @PathParam("dataserviceName") String dataserviceName,
            @ApiParam(value = "" + "JSON of the properties of the new data service:<br>" + OPEN_PRE_TAG + OPEN_BRACE
                    + BR + NBSP + "keng\\_\\_id: \"id of the data service\"" + COMMA + BR + NBSP + OPEN_PRE_CMT
                    + "(identical to dataserviceName parameter)" + CLOSE_PRE_CMT + BR + BR + NBSP
                    + "keng\\_\\_dataPath: \"path of dataservice\"" + COMMA + BR + NBSP
                    + "tko__description: \"the description\"" + BR + CLOSE_BRACE
                    + CLOSE_PRE_TAG, required = true) final String dataserviceJson)
            throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (!isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        // Error if the dataservice name is missing
        if (StringUtils.isBlank(dataserviceName)) {
            return createErrorResponseWithForbidden(mediaTypes,
                    RelationalMessages.Error.DATASERVICE_SERVICE_UPDATE_MISSING_NAME);
        }

        final RestDataservice restDataservice = KomodoJsonMarshaller.unmarshall(dataserviceJson, RestDataservice.class);
        final String jsonDataserviceName = restDataservice.getId();
        // Error if the name is missing from the supplied json body
        if (StringUtils.isBlank(jsonDataserviceName)) {
            return createErrorResponseWithForbidden(mediaTypes,
                    RelationalMessages.Error.DATASERVICE_SERVICE_JSON_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataserviceName.equals(jsonDataserviceName);
        if (!namesMatch) {
            return createErrorResponseWithForbidden(mediaTypes, DATASERVICE_SERVICE_SERVICE_NAME_ERROR, dataserviceName,
                    jsonDataserviceName);
        }

        UnitOfWork uow = null;

        try {
            uow = createTransaction(principal, "updateDataservice", false); //$NON-NLS-1$

            Dataservice dataservice = findDataservice(uow, dataserviceName);
            if (dataservice == null)
                return commitNoDataserviceFound(uow, mediaTypes, dataserviceName);

            // Note that intially only the description is available for updating. 
            dataservice.setDescription(uow, restDataservice.getDescription());

            KomodoStatusObject kso = new KomodoStatusObject("Update Dataservice Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Dataservice successfully updated"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException) e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e,
                    RelationalMessages.Error.DATASERVICE_SERVICE_UPDATE_ERROR, dataserviceName);
        }
    }
}
