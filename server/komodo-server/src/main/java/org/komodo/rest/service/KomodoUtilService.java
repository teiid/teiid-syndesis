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

import org.komodo.StringConstants;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.MetadataInstance.ValidationResult;
import org.komodo.rest.KomodoService;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.rest.datavirtualization.RestViewDefinitionStatus;
import org.komodo.rest.datavirtualization.ViewListing;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.teiid.metadata.AbstractMetadataRecord;
import org.teiid.metadata.Table;
import org.teiid.query.validator.ValidatorReport;
import org.teiid.util.FullyQualifiedName;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * A Komodo REST service for obtaining VDB information from the workspace.
 */
@Component
@Path( V1Constants.SERVICE_SEGMENT )
@Api( tags = {V1Constants.SERVICE_SEGMENT} )
public final class KomodoUtilService extends KomodoService {

    private static final String SUCCESS = "SUCCESS";

	private static final String ERROR = "ERROR";

	public static final String PREVIEW_VDB = "PreviewVdb";

    public static final String APP_NAME = "App Name"; //$NON-NLS-1$

    public static final String APP_TITLE = "App Title"; //$NON-NLS-1$

    public static final String APP_DESCRIPTION = "App Description"; //$NON-NLS-1$

    public static final String APP_VERSION = "App Version"; //$NON-NLS-1$

    @Autowired
    private MetadataInstance metadataInstance;
    
    @Autowired
    private KomodoMetadataService metadataService;

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return about information of this service
     */
    @GET
    @Path(V1Constants.ABOUT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation( value = "Display status of this rest service", response = String.class )
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response about(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo) {

        checkSecurityContext(headers);

        KomodoStatusObject repoStatus = new KomodoStatusObject();

        repoStatus.addAttribute(APP_NAME, V1Constants.App.name());
        repoStatus.addAttribute(APP_TITLE, V1Constants.App.title());
        repoStatus.addAttribute(APP_DESCRIPTION, V1Constants.App.description());
        repoStatus.addAttribute(APP_VERSION, V1Constants.App.version());

        // create response
        return toResponse(repoStatus);
    }

    /**
     * Get all view editor states from the user's profile
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the view editor states in the user profile (never <code>null</code>)
     * @throws Exception 
     */
    @GET
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH + V1Constants.VIEW_LISTINGS)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Return the collection of view listings",
                  response = ViewListing[].class)
    @ApiImplicitParams({
        @ApiImplicitParam(
                name = QueryParamKeys.VIRTUALIZATION,
                value = "The name of the virtualization",
                required = true,
                dataType = "string",
                paramType = "query")
      })
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getViewList( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo ) throws Exception {

        String principal = checkSecurityContext(headers);

        final List< ViewListing > viewDefinitions = new ArrayList<>();

        final String virtualization = uriInfo.getQueryParameters().getFirst( QueryParamKeys.VIRTUALIZATION );
        
        if (StringUtils.isBlank(virtualization)) {
        	return createErrorResponse(Status.FORBIDDEN, "VIRTUALIZATION name is required");
        }
        
        // find view editor states
        return runInTransaction(principal, "getViewEditorStates", true, ()->{
        
            final List<? extends ViewDefinition> viewEditorStates = getWorkspaceManager().findViewDefinitions( virtualization );
            LOGGER.debug( "getViewEditorStates:found %d ViewEditorStates", viewEditorStates.size() ); //$NON-NLS-1$

            //TODO: paging / sorting can be pushed into the repository

            for ( final ViewDefinition viewEditorState : viewEditorStates ) {
                ViewListing listing = new ViewListing();
                listing.setId(viewEditorState.getId());
                listing.setName(viewEditorState.getName());
                listing.setDescription(viewEditorState.getDescription());
                viewDefinitions.add(listing);
            }

            return toResponse(viewDefinitions );
        });
    }

    /**
     * Get the view editor state with the given id from the user's profile
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the view editor state in the user profile (never <code>null</code>)
     * @throws Exception 
     */
    @GET
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH +
                  V1Constants.VIEW_EDITOR_STATE + FORWARD_SLASH +
                  V1Constants.VIEW_EDITOR_STATE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Returns the view editor state with the given id",
                  response = ViewDefinition.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getViewEditorState( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(value = "Name of the view editor state to fetch", required = true)
                                    final @PathParam( "viewEditorStateId" ) String viewEditorStateId) throws Exception {

        String principal = checkSecurityContext(headers);

    	return runInTransaction(principal, "getViewEditorStates", true, ()->{
            ViewDefinition viewEditorState = getWorkspaceManager().findViewDefinition(viewEditorStateId);
            LOGGER.debug( "getViewEditorState:found %d ViewEditorStates",
                              viewEditorState == null ? 0 : 1 ); //$NON-NLS-1$

            if (viewEditorState == null)
                return Response.noContent().build();

            LOGGER.debug("getViewEditorStates:ViewEditorState %s entity was constructed", viewEditorState.getName()); //$NON-NLS-1$
            return toResponse(viewEditorState );
    	});
    }

    /**
     * Stash a ViewEditorState
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return stashed view editor state
     * @throws Exception 
     */
    @PUT
    @Produces( MediaType.APPLICATION_JSON )
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH + V1Constants.VIEW_EDITOR_STATE)
    @ApiOperation( value = "Store view editor state" )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response stashViewEditorState(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo,
                                               @ApiParam(required = true)
                                               final org.komodo.datavirtualization.ViewDefinition restViewEditorState) throws Exception {

        String principal = checkSecurityContext(headers);

        if (StringUtils.isBlank(restViewEditorState.getName())) {
        	forbidden(RelationalMessages.Error.VIEW_DEFINITION_MISSING_NAME);
        }
        
        if (StringUtils.isBlank(restViewEditorState.getDataVirtualizationName())) {
        	forbidden(RelationalMessages.Error.VIEW_DEFINITION_MISSING_DATAVIRTUALIZATIONNAME);
        }
        
    	ViewDefinition vd = runInTransaction(principal, "upsertViewDefinition", false, ()->{
    		return upsertViewEditorState(restViewEditorState);
    	});

        KomodoStatusObject kso = new KomodoStatusObject("Stash Status"); //$NON-NLS-1$
        kso.addAttribute("Stash Status", "Successfully stashed"); //$NON-NLS-1$
        kso.addAttribute(StringConstants.ID_LABEL, vd.getId());
        return toResponse(kso);
    }
    
    /**
     * Validate the supplied ViewDefinition
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return validation status of the supplied ViewDefinition
     */
    @POST
    @Produces( MediaType.APPLICATION_JSON )
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH + V1Constants.VALIDATE_VIEW_DEFINITION)
    @ApiOperation( value = "Validate a ViewDefinition", response = RestViewDefinitionStatus.class )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response validateViewDefinition(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo,
                                               @ApiParam(required = true)
                                               final ViewDefinition restViewDefinition) {
        checkSecurityContext(headers);

    	LOGGER.debug("Validating view : %s", restViewDefinition.getName());
    	
        RestViewDefinitionStatus viewDefnStatus = validateViewDefinition(restViewDefinition);

        return toResponse(viewDefnStatus);
    }

	private RestViewDefinitionStatus validateViewDefinition(final ViewDefinition restViewDefinition) {
		RestViewDefinitionStatus viewDefnStatus = new RestViewDefinitionStatus();
		
		String viewName = restViewDefinition.getName();
    	String viewDdl = restViewDefinition.getDdl();

        if (StringUtils.isBlank(viewName)) {
        	viewDefnStatus.setStatus(ERROR);
        	viewDefnStatus.setMessage(RelationalMessages.getString(RelationalMessages.Error.VIEW_DEFINITION_MISSING_NAME));
            return viewDefnStatus;
        }
        
        if (StringUtils.isBlank(restViewDefinition.getDataVirtualizationName())) {
        	viewDefnStatus.setStatus(ERROR);
        	viewDefnStatus.setMessage(RelationalMessages.getString(RelationalMessages.Error.VIEW_DEFINITION_MISSING_DATAVIRTUALIZATIONNAME));
            return viewDefnStatus;
        }

        if (StringUtils.isBlank(viewDdl)) {
        	viewDefnStatus.setStatus(ERROR);
        	viewDefnStatus.setMessage(RelationalMessages.getString(RelationalMessages.Error.VIEW_DEFINITION_MISSING_DDL));
            return viewDefnStatus;
        }
        
        try {
        	ValidationResult result = metadataInstance.validate(PREVIEW_VDB, restViewDefinition.getDdl());
        	ValidatorReport report = result.getReport();
        	
        	Table t = result.getSchema().getTables().get(viewName);
        	// If names do not match, create an error status
        	if(t == null) {
                String errorMsg = RelationalMessages.getString(RelationalMessages.Error.VALIDATE_VIEW_DEFINITION_NAME_MATCH_ERROR, viewName);
    			viewDefnStatus.setStatus(ERROR);
    			viewDefnStatus.setMessage(errorMsg);
        	} else {
        		// If user-defined, user may have changed description.  Reset object description from DDL
                if(restViewDefinition.isUserDefined()) {
                	
                	//TODO: it's not clear here what the user's intent is
                	
                	String ddlDescr = t.getAnnotation();
                	if (ddlDescr != null) {
                		restViewDefinition.setDescription(ddlDescr);
                	}
                }
        		
	        	String error = report.getFailureMessage();
	        	if (report.hasItems() && !error.isEmpty()) {
	            	viewDefnStatus.setStatus(ERROR);
	            	viewDefnStatus.setMessage(error);        		
	        	} else {
		        	viewDefnStatus.setStatus(SUCCESS);
		        	viewDefnStatus.setMessage("View DDL was parsed successfully");
	        	}
        	}
		} catch (Exception ex) {
			String msg = "Parsing Error for view: " + restViewDefinition.getName()
				+ "\n" + ex.getMessage();
        	LOGGER.warn(msg);
        	viewDefnStatus.setStatus(ERROR);
        	viewDefnStatus.setMessage("Parsing Error\n" + ex.getMessage());
		}
		return viewDefnStatus;
	}

    /**
     * Creates the view editor state from the RestViewEditorState
     * @param editorState the state
     * @return the ViewEditorState repo object
     * @throws Exception exception if a problem is encountered
     * 
     * TODO: could refactor to directly save / merge, rather than copy
     */
    ViewDefinition upsertViewEditorState(final ViewDefinition restViewDefn) throws Exception {
    	
    	ViewDefinition viewDefn = null;
    	
    	if (restViewDefn.getId() != null) {
    		viewDefn = getWorkspaceManager().findViewDefinition(restViewDefn.getId());
    	}
    	if (viewDefn == null) {
        	viewDefn = getWorkspaceManager().findViewDefinitionByNameIgnoreCase(restViewDefn.getDataVirtualizationName(), restViewDefn.getName());
    	}

    	boolean pathsSame = false;
        // Add a new ViewDefinition
    	if (viewDefn == null) {
    		viewDefn = getWorkspaceManager().createViewDefiniton(restViewDefn.getDataVirtualizationName(), restViewDefn.getName());
    	} else {
    		if (restViewDefn.getId() != null && viewDefn.getId() != null && !restViewDefn.getId().equals(viewDefn.getId())) {
    			throw new IllegalArgumentException("view id does not match the persistent state");
    		}
        	if (!restViewDefn.getName().equals(viewDefn.getName()) || !restViewDefn.getDataVirtualizationName().equals(viewDefn.getDataVirtualizationName())) {
        		throw new IllegalArgumentException("view name / dv name does not match the persistent state");
        	}
        	pathsSame = restViewDefn.getSourcePaths().equals(viewDefn.getSourcePaths());
    		viewDefn.clearState();
    	}

        // Set ViewDefinition of the ViewEditorState
        viewDefn.setDdl(restViewDefn.getDdl());
        
        viewDefn.setDescription(restViewDefn.getDescription());
        
        for (String restSourcePath: restViewDefn.getSourcePaths()) {
            viewDefn.addSourcePath(restSourcePath);
        }
        viewDefn.setComplete(restViewDefn.isComplete());
        viewDefn.setUserDefined(restViewDefn.isUserDefined());
        
        if (viewDefn.isComplete()) {
	        if (!viewDefn.isUserDefined()) {
	        	//regenerate if needed
	        	if (viewDefn.getDdl() == null || !pathsSame) {
		        	String ddl = new ServiceVdbGenerator(metadataService).getODataViewDdl(viewDefn);
					viewDefn.setDdl(ddl);
	        	}
	        } else if (viewDefn.getDdl() != null) {
	        	try {
		        	ValidationResult result = metadataInstance.validate(PREVIEW_VDB, viewDefn.getDdl());
		        	Table t = result.getSchema().getTables().get(viewDefn.getName());
		        	if (t != null) {
			        	viewDefn.getSourcePaths().clear();
			        	for (AbstractMetadataRecord r : t.getIncomingObjects()) {
			        		if (r instanceof Table) {
			        			FullyQualifiedName fqn = new FullyQualifiedName(SCHEMA_KEY, r.getParent().getName());
			        			fqn.append(TABLE_KEY, r.getName());
			        			viewDefn.addSourcePath(fqn.toString());
			        		}
			        	}
		        	}
	        	} catch (Exception e) {
	        		//ddl is not valid
	        		KLog.getLogger().debug("could not determine source paths", e);
	        	}
	        }
        }
        
        return viewDefn;
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws Exception 
     */
    @DELETE
    @Produces( MediaType.APPLICATION_JSON )
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH +
                  V1Constants.VIEW_EDITOR_STATE + FORWARD_SLASH +
                  V1Constants.VIEW_EDITOR_STATE_PLACEHOLDER)
    @ApiOperation( value = "Remove a view editor state from the user's profile", response = String.class )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeViewEditorState(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo,
                                               @ApiParam(
                                                         value = "Id of the view editor state to remove",
                                                         required = true
                                               )
                                               final @PathParam("viewEditorStateId") String viewEditorStateId) throws Exception {

        String principal = checkSecurityContext(headers);

        if (StringUtils.isBlank(viewEditorStateId)) {
            forbidden(RelationalMessages.Error.PROFILE_EDITOR_STATE_MISSING_NAME);
        }

        return runInTransaction(principal, "removeUserProfileViewEditorState", false, ()-> {
            if (!getWorkspaceManager().deleteViewDefinition(viewEditorStateId)) {
                return Response.noContent().build();
            }

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(viewEditorStateId, "Successfully deleted"); //$NON-NLS-1$

            return toResponse(kso);
        }); //$NON-NLS-1$
    }
}
