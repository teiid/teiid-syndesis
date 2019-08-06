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
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;

import org.komodo.KException;
import org.komodo.StringConstants;
import org.komodo.TeiidSqlConstants;
import org.komodo.WorkspaceManager;
import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.openshift.ProtocolType;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.ResourceNotFound;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.response.KomodoStatusObject;
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
    
    @Autowired
    private KomodoMetadataService metadataService;

    /**
     * Get the Dataservices from the komodo repository
     *
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @return a JSON document representing all the Dataservices in the Komodo
     *         workspace (never <code>null</code>)
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Return the collection of data services", response = RestDataservice[].class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = QueryParamKeys.SIZE, value = "The number of objects to return. If not present, all objects are returned", required = false, dataType = "integer", paramType = "query"),
            @ApiImplicitParam(name = QueryParamKeys.START, value = "Index of the first dataservice to return", required = false, dataType = "integer", paramType = "query") })
    @ApiResponses(value = { @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response getDataservices(final @Context HttpHeaders headers, final @Context UriInfo uriInfo) {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        try {
        	return runInTransaction(principal, "getDataVirtualizations", true, ()->{
	            Iterable<? extends DataVirtualization> dataServices = getWorkspaceManager().findDataVirtualizations();
	
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
	
	            for (final DataVirtualization dataService : dataServices) {
	                if ((start == 0) || (i >= start)) {
	                    if ((size == ALL_AVAILABLE) || (entities.size() < size)) {
	                        RestDataservice entity = createRestDataservice(dataService);
	
	                        entities.add(entity);
	                        LOGGER.debug("getDataservices:Dataservice '{0}' entity was constructed", //$NON-NLS-1$
	                                dataService.getName());
	                    } else {
	                        break;
	                    }
	                }
	
	                ++i;
	            }
	
	            // create response
	            return toResponse(mediaTypes, entities);
        	});
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e, DATASERVICE_SERVICE_GET_DATASERVICES_ERROR);
        }
    }

	private RestDataservice createRestDataservice(final DataVirtualization dataService) throws KException {
		RestDataservice entity = new RestDataservice(dataService, dataService.getServiceVdbName());
		entity.setServiceViewModel(SERVICE_VDB_VIEW_MODEL);
		List<String> names = getWorkspaceManager().getViewDefinitionsNames(dataService.getName());
        entity.setViewDefinitionNames(names.toArray(new String[names.size()]));
		// Set published status of dataservice
		BuildStatus status = this.openshiftClient.getVirtualizationStatus(dataService.getServiceVdbName());
		entity.setPublishedState(status.status().name());
		entity.setPublishPodName(status.publishPodName());
		entity.setPodNamespace(status.namespace());
		entity.setOdataHostName(getOdataHost(status));
		return entity;
	}

    /**
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param dataserviceName
     *            the id of the Dataservice being retrieved (cannot be empty)
     * @return the JSON representation of the Dataservice (never <code>null</code>)
     */
    @GET
    @Path(V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces({ MediaType.APPLICATION_JSON })
    @ApiOperation(value = "Find dataservice by name", response = RestDataservice.class)
    @ApiResponses(value = { @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
            @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response getDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Id of the dataservice to be fetched, ie. the value of the 'keng__id' property", required = true) final @PathParam("dataserviceName") String dataserviceName) {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        try {
            RestDataservice dataservice = runInTransaction(principal, "getDataVirtualization", true, () -> {
            	DataVirtualization dv = getWorkspaceManager().findDataVirtualization(dataserviceName);
                return createRestDataservice(dv);
            });
            if (dataservice == null) {
            	return toResponse(mediaTypes, new ResourceNotFound( dataserviceName ));
        	}

            LOGGER.debug("getDataservice:Dataservice '{0}' entity was constructed", dataservice.getName()); //$NON-NLS-1$
            return toResponse(mediaTypes, dataservice);
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e, DATASERVICE_SERVICE_GET_DATASERVICE_ERROR,
                    dataserviceName);
        }
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
     */
    @POST
    @Path(FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Create a dataservice in the workspace", consumes = MediaType.APPLICATION_JSON)
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response createDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service", required = true) final @PathParam("dataserviceName") String dataserviceName,
            @ApiParam(required = true) final RestDataservice restDataservice) {

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

        final String jsonDataserviceName = restDataservice.getName();
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
        
        final String errorMsg = VALIDATOR.checkValidName(dataserviceName);

        // a name validation error occurred
        if (errorMsg != null) {
            final Response response = Response.ok().entity(errorMsg).build();
            return response;
        }

        try {
            // create new Dataservice
        	return runInTransaction(principal, "createDataVirtualization", false, () -> {
                // Error if the repo already contains a dataservice with the supplied name.
                DataVirtualization dv = getWorkspaceManager().findDataVirtualizationByNameIgnoreCase(dataserviceName);
                if (dv != null) {
                	return createErrorResponse(Status.CONFLICT, mediaTypes, RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_ALREADY_EXISTS);
                }
                final DataVirtualization dataservice = getWorkspaceManager().createDataVirtualization(dataserviceName);

                dataservice.setDescription(restDataservice.getDescription());
                KomodoStatusObject kso = new KomodoStatusObject("Create Status"); //$NON-NLS-1$
                kso.addAttribute(dataserviceName, "Successfully created"); //$NON-NLS-1$
                kso.addAttribute(StringConstants.ID_LABEL, dataservice.getId());
                return toResponse(mediaTypes, kso);
            });
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e, DATASERVICE_SERVICE_CREATE_DATASERVICE_ERROR,
                    dataserviceName);
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
     */
    @DELETE
    @Path("{dataserviceName}")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Delete a dataservice from the workspace")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response deleteDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service to be deleted", required = true) final @PathParam("dataserviceName") String dataserviceName) {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        try {
        	KomodoStatusObject kso = runInTransaction(principal, "deleteDataVirtualization", false, ()->{
                final WorkspaceManager wkspMgr = getWorkspaceManager();

                final DataVirtualization dataservice = wkspMgr.findDataVirtualization(dataserviceName);
                
                // Error if the specified service does not exist
                if (dataservice == null) {
                    return null;
                }

                // Delete the Dataservice. The view definitions will cascade
                wkspMgr.deleteDataVirtualization(dataserviceName);

                KomodoStatusObject status = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
                status.addAttribute(dataserviceName, "Successfully deleted"); //$NON-NLS-1$

        		return status;
        	});
        	
        	if (kso == null) {
        		return createErrorResponse(Status.NOT_FOUND, mediaTypes,
                        RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_DNE);
        	}

            return toResponse(mediaTypes, kso);
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e, DATASERVICE_SERVICE_DELETE_DATASERVICE_ERROR);
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
            @ApiParam(value = "The dataservice name being checked", required = true) final @PathParam("dataserviceName") String dataserviceName) {

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
        try {
            final DataVirtualization service = runInTransaction(principal, "validateDataVirtualizationName", true, () -> {
            	return getWorkspaceManager().findDataVirtualizationByNameIgnoreCase(dataserviceName);
            });

            if (service == null) {
            	return Response.ok().build();
            }

            // name is a duplicate
            return Response.ok().entity(RelationalMessages.getString(DATASERVICE_SERVICE_NAME_EXISTS)).build();
        } catch (final Exception e) {
            return createErrorResponse(headers.getAcceptableMediaTypes(), e,
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
     */
    @POST
    @Path(StringConstants.FORWARD_SLASH + V1Constants.REFRESH_DATASERVICE_VIEWS + StringConstants.FORWARD_SLASH
            + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Refresh the dataservice views from user profile states")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response refreshViews(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the dataservice", required = true) final @PathParam("dataserviceName") String dataserviceName) {

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

        try {
        	return runInTransaction(principal, "refreshDataVirtualizationViews", false, () -> {
        		DataVirtualization dataservice = getWorkspaceManager().findDataVirtualization(dataserviceName);
                if (dataservice == null) {
                	return toResponse(mediaTypes, new ResourceNotFound( dataserviceName ));
                }

                //generate has the side effect of setting ddl
                this.metadataService.generateServiceVDB(dataservice);
                
                /*TODO: should we generate and deploy a virtualization specific preview vdb
                  that should be done outside of the txn
                */

                KomodoStatusObject kso = new KomodoStatusObject("Refresh Status"); //$NON-NLS-1$
                kso.addAttribute(dataserviceName, "View Successfully refreshed"); //$NON-NLS-1$

                return toResponse(mediaTypes, kso);
        	});
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e,
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
            		if(routeStatus.getProtocol() == ProtocolType.ODATA) {
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
     */
    @PUT
    @Path(FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Update data service")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response updateDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service", required = true) final @PathParam("dataserviceName") String dataserviceName,
            @ApiParam(required = true) final RestDataservice restDataservice) {

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

        final String jsonDataserviceName = restDataservice.getName();
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

        try {
        	return runInTransaction(principal, "createDataVirtualization", false, () -> {
                // Error if the repo already contains a dataservice with the supplied name.
                DataVirtualization existing = getWorkspaceManager().findDataVirtualization(restDataservice.getName());
                if (existing == null) {
            		return toResponse(mediaTypes, new ResourceNotFound( dataserviceName ));
                }
                
                existing.setDescription(restDataservice.getDescription());
                KomodoStatusObject kso = new KomodoStatusObject("Update Dataservice Status"); //$NON-NLS-1$
                kso.addAttribute(dataserviceName, "Dataservice successfully updated"); //$NON-NLS-1$

                return toResponse(mediaTypes, kso);
            });
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e,
                    RelationalMessages.Error.DATASERVICE_SERVICE_UPDATE_ERROR, dataserviceName);
        }
    }
    
}
