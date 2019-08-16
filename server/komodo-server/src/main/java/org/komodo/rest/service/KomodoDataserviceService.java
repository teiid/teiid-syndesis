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

import static org.komodo.rest.datavirtualization.RelationalMessages.Error.DATASERVICE_SERVICE_NAME_EXISTS;
import static org.komodo.rest.datavirtualization.RelationalMessages.Error.DATASERVICE_SERVICE_SERVICE_NAME_ERROR;

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
import org.komodo.WorkspaceManager;
import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.openshift.ProtocolType;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.KomodoService;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.ImportPayload;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.rest.datavirtualization.RestDataVirtualization;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.teiid.metadata.Schema;
import org.teiid.metadata.Table;
import org.teiid.util.FullyQualifiedName;

import io.swagger.annotations.Api;
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
public final class KomodoDataserviceService extends KomodoService {

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
     * @throws Exception 
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Return the collection of data services", response = RestDataVirtualization[].class)
    @ApiResponses(value = { @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response getDataservices(final @Context HttpHeaders headers, final @Context UriInfo uriInfo) throws Exception {
        String principal = checkSecurityContext(headers);

    	return runInTransaction(principal, "getDataVirtualizations", true, ()->{
            Iterable<? extends DataVirtualization> dataServices = getWorkspaceManager().findDataVirtualizations();

            final List<RestDataVirtualization> entities = new ArrayList<>();

            for (final DataVirtualization dataService : dataServices) {
                RestDataVirtualization entity = createRestDataservice(dataService);

                entities.add(entity);
                LOGGER.debug("getDataservices:Dataservice '%s' entity was constructed", //$NON-NLS-1$
                        dataService.getName());
            }

            // create response
            return toResponse(entities);
    	});
    }

	private RestDataVirtualization createRestDataservice(final DataVirtualization dataService) throws KException {
		RestDataVirtualization entity = new RestDataVirtualization(dataService, dataService.getServiceVdbName());
		entity.setServiceViewModel(SERVICE_VDB_VIEW_MODEL);
		// Set published status of dataservice
		BuildStatus status = this.openshiftClient.getVirtualizationStatus(dataService.getServiceVdbName());
		entity.setPublishedState(status.status().name());
		entity.setPublishPodName(status.publishPodName());
		entity.setPodNamespace(status.namespace());
		entity.setOdataHostName(getOdataHost(status));
		entity.setEmpty(this.getWorkspaceManager().findViewDefinitionsNames(dataService.getName()).isEmpty());
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
     * @throws Exception 
     */
    @GET
    @Path(V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces({ MediaType.APPLICATION_JSON })
    @ApiOperation(value = "Find dataservice by name", response = RestDataVirtualization.class)
    @ApiResponses(value = { @ApiResponse(code = 404, message = "No Dataservice could be found with name"),
            @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response getDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Id of the dataservice to be fetched, ie. the value of the 'keng__id' property", required = true) final @PathParam("dataserviceName") String dataserviceName) throws Exception {

        String principal = checkSecurityContext(headers);

        RestDataVirtualization dataservice = runInTransaction(principal, "getDataVirtualization", true, () -> {
        	DataVirtualization dv = getWorkspaceManager().findDataVirtualization(dataserviceName);
            return createRestDataservice(dv);
        });
        if (dataservice == null) {
        	notFound( dataserviceName );
    	}

        LOGGER.debug("getDataservice:Dataservice '%s' entity was constructed", dataservice.getName()); //$NON-NLS-1$
        return toResponse(dataservice);
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
	 * @throws Exception 
     */
    @POST
    @Path(FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Create a dataservice in the workspace", consumes = MediaType.APPLICATION_JSON)
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response createDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service", required = true) final @PathParam("dataserviceName") String dataserviceName,
            @ApiParam(required = true) final RestDataVirtualization restDataservice) throws Exception {

        String principal = checkSecurityContext(headers);

        // Error if the dataservice name is missing
        if (StringUtils.isBlank(dataserviceName)) {
            forbidden(RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_NAME);
        }

        final String jsonDataserviceName = restDataservice.getName();
        // Error if the name is missing from the supplied json body
        if (StringUtils.isBlank(jsonDataserviceName)) {
            forbidden(RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataserviceName.equals(jsonDataserviceName);
        if (!namesMatch) {
            forbidden(DATASERVICE_SERVICE_SERVICE_NAME_ERROR, dataserviceName, jsonDataserviceName);
        }
        
        final String errorMsg = VALIDATOR.checkValidName(dataserviceName);

        // a name validation error occurred
        if (errorMsg != null) {
            return createErrorResponse(Status.FORBIDDEN, errorMsg);
        }

        // create new Dataservice
    	return runInTransaction(principal, "createDataVirtualization", false, () -> {
            // Error if the repo already contains a dataservice with the supplied name.
            DataVirtualization dv = getWorkspaceManager().findDataVirtualizationByNameIgnoreCase(dataserviceName);
            if (dv != null) {
            	error(Status.CONFLICT, RelationalMessages.Error.DATASERVICE_SERVICE_CREATE_ALREADY_EXISTS);
            }
            final DataVirtualization dataservice = getWorkspaceManager().createDataVirtualization(dataserviceName);

            dataservice.setDescription(restDataservice.getDescription());
            KomodoStatusObject kso = new KomodoStatusObject("Create Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Successfully created"); //$NON-NLS-1$
            kso.addAttribute(StringConstants.ID_LABEL, dataservice.getId());
            return toResponse(kso);
        });
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
     * @throws Exception 
     */
    @DELETE
    @Path(V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Delete a dataservice from the workspace")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response deleteDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service to be deleted", required = true) final @PathParam("dataserviceName") String dataserviceName) throws Exception {
        String principal = checkSecurityContext(headers);

    	KomodoStatusObject kso = runInTransaction(principal, "deleteDataVirtualization", false, ()->{
            final WorkspaceManager wkspMgr = getWorkspaceManager();

            final DataVirtualization dataservice = wkspMgr.findDataVirtualization(dataserviceName);
            
            // Error if the specified service does not exist
            if (dataservice == null) {
                notFound(dataserviceName);
            }

            // Delete the Dataservice. The view definitions will cascade
            wkspMgr.deleteDataVirtualization(dataserviceName);

            KomodoStatusObject status = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            status.addAttribute(dataserviceName, "Successfully deleted"); //$NON-NLS-1$

    		return status;
    	});
    	
        return toResponse(kso);
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
     * @throws Exception 
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
            @ApiParam(value = "The dataservice name being checked", required = true) final @PathParam("dataserviceName") String dataserviceName) throws Exception {

        final String principal = checkSecurityContext(headers);

        final String errorMsg = VALIDATOR.checkValidName(dataserviceName);

        // a name validation error occurred
        if (errorMsg != null) {
            final Response response = Response.ok().entity(errorMsg).build();
            return response;
        }

        // check for duplicate name
        final DataVirtualization service = runInTransaction(principal, "validateDataVirtualizationName", true, () -> {
        	return getWorkspaceManager().findDataVirtualizationByNameIgnoreCase(dataserviceName);
        });

        if (service == null) {
        	return Response.ok().build();
        }

        // name is a duplicate
        return Response.ok().entity(RelationalMessages.getString(DATASERVICE_SERVICE_NAME_EXISTS)).build();
    }

    @PUT
    @Path(StringConstants.FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER + 
    		StringConstants.FORWARD_SLASH + V1Constants.IMPORT + StringConstants.FORWARD_SLASH
    		+ V1Constants.KOMODO_SOURCE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Import views from a given source", response = KomodoStatusObject.class)
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response importViews(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the dataservice", required = true) 
    		final @PathParam("dataserviceName") 
    		String dataserviceName,
            
            @ApiParam( value = "Name of the komodo source", required = true ) 
    		final @PathParam( "komodoSourceName" ) 
    		String komodoSourceName,
    		
    		@ApiParam(value = "Import Payload", required = true) 
    		final ImportPayload importPayload) throws Exception {

        String principal = checkSecurityContext(headers);

        // Error if the dataservice name is missing
        if (StringUtils.isBlank(dataserviceName)) {
            forbidden(RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_NAME);
        }
        
        if (StringUtils.isBlank(komodoSourceName)) {
        	forbidden(RelationalMessages.Error.CONNECTION_SERVICE_MISSING_CONNECTION_NAME);
        }

    	KomodoStatusObject kso = runInTransaction(principal, "import", false, () -> {
    		return importViews(dataserviceName, komodoSourceName, importPayload);
    	});
    	
    	return toResponse(kso);
    }

	KomodoStatusObject importViews(final String dataserviceName, final String komodoSourceName,
			final ImportPayload importPayload) throws KException, AssertionError {
		DataVirtualization dataservice = getWorkspaceManager().findDataVirtualization(dataserviceName);
		if (dataservice == null) {
			notFound( dataserviceName );
		}
		
		Schema s = metadataService.findSchema(komodoSourceName);
		
		if (s == null) {
			notFound( komodoSourceName );
		}
		
		ServiceVdbGenerator serviceVdbGenerator = new ServiceVdbGenerator(metadataService);
		
		KomodoStatusObject kso = new KomodoStatusObject("Import Status"); //$NON-NLS-1$
		
		for (String name : importPayload.getTables()) {
			Table t = s.getTable(name);
			if (t == null) {
				//could be an error/warning
				continue;
			}
			
			ViewDefinition viewDefn = getWorkspaceManager().findViewDefinitionByNameIgnoreCase(dataserviceName, name);
			if (viewDefn != null) {
				//sanity check
				if (!name.equalsIgnoreCase(viewDefn.getName())) {
					throw new AssertionError("imported view name conflicts with an existing view name");
				}
				
				//reuse the same id
				viewDefn.clearState();
				viewDefn.setUserDefined(false);
				viewDefn.setDdl(null);
				viewDefn.setDescription(null);
			} else {
				viewDefn = getWorkspaceManager().createViewDefiniton(dataserviceName, name);
			}
			viewDefn.setComplete(true);
			FullyQualifiedName fqn = new FullyQualifiedName(SCHEMA_KEY, komodoSourceName);
			fqn.append(TABLE_KEY, t.getName());
			viewDefn.addSourcePath(fqn.toString());
			
			String ddl = serviceVdbGenerator.getODataViewDdl(viewDefn);
			viewDefn.setDdl(ddl);
			
			kso.addAttribute(viewDefn.getName(), viewDefn.getId());
		}

		//TODO: should this "refresh" the views as we go 
		//this.metadataService.generateServiceVDB(dataservice);
		
		return kso;
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
     * @throws Exception 
     */
    @PUT
    @Path(FORWARD_SLASH + V1Constants.DATA_SERVICE_PLACEHOLDER)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Update data service")
    @ApiResponses(value = { @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
            @ApiResponse(code = 403, message = "An error has occurred.") })
    public Response updateDataservice(final @Context HttpHeaders headers, final @Context UriInfo uriInfo,
            @ApiParam(value = "Name of the data service", required = true) final @PathParam("dataserviceName") String dataserviceName,
            @ApiParam(required = true) final RestDataVirtualization restDataservice) throws Exception {

        String principal = checkSecurityContext(headers);

        // Error if the dataservice name is missing
        if (StringUtils.isBlank(dataserviceName)) {
            forbidden(RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_NAME);
        }

        final String jsonDataserviceName = restDataservice.getName();
        // Error if the name is missing from the supplied json body
        if (StringUtils.isBlank(jsonDataserviceName)) {
            forbidden(RelationalMessages.Error.DATASERVICE_SERVICE_MISSING_NAME);
        }

        // Error if the name parameter is different than JSON name
        final boolean namesMatch = dataserviceName.equals(jsonDataserviceName);
        if (!namesMatch) {
            forbidden(DATASERVICE_SERVICE_SERVICE_NAME_ERROR, dataserviceName, jsonDataserviceName);
        }

    	return runInTransaction(principal, "createDataVirtualization", false, () -> {
            // Error if the repo already contains a dataservice with the supplied name.
            DataVirtualization existing = getWorkspaceManager().findDataVirtualization(restDataservice.getName());
            if (existing == null) {
            	notFound( dataserviceName );
            }
            
            existing.setDescription(restDataservice.getDescription());
            KomodoStatusObject kso = new KomodoStatusObject("Update Dataservice Status"); //$NON-NLS-1$
            kso.addAttribute(dataserviceName, "Dataservice successfully updated"); //$NON-NLS-1$

            return toResponse(kso);
        });
    }
    
}
