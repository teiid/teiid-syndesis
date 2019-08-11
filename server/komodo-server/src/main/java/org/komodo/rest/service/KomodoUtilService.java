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
import org.komodo.datavirtualization.SqlComposition;
import org.komodo.datavirtualization.SqlProjectedColumn;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.MetadataInstance.ValidationResult;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.rest.datavirtualization.RestViewDefinitionStatus;
import org.komodo.rest.datavirtualization.ViewListing;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.teiid.metadata.Table;
import org.teiid.query.validator.ValidatorReport;

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

    public static final String USER_NAME = "User Name"; //$NON-NLS-1$

    public static final String WORKSPACE = "Workspace"; //$NON-NLS-1$

    @Autowired
    private MetadataInstance metadataInstance;

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

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        KomodoStatusObject repoStatus = new KomodoStatusObject();

        repoStatus.addAttribute(APP_NAME, KomodoRestV1Application.V1Constants.App.name());
        repoStatus.addAttribute(APP_TITLE, KomodoRestV1Application.V1Constants.App.title());
        repoStatus.addAttribute(APP_DESCRIPTION, KomodoRestV1Application.V1Constants.App.description());
        repoStatus.addAttribute(APP_VERSION, KomodoRestV1Application.V1Constants.App.version());

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();

        // create response
        try {
            return toResponse(mediaTypes, repoStatus);
        } catch (Exception ex) {
            return createErrorResponse(mediaTypes, ex, RelationalMessages.Error.ABOUT_SERVICE_ERROR);
        }
    }

    /**
     * Get all view editor states from the user's profile
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the view editor states in the user profile (never <code>null</code>)
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
                paramType = "query"),
        @ApiImplicitParam(
                name = QueryParamKeys.SIZE,
                value = "The number of objects to return. If not present, all objects are returned",
                required = false,
                dataType = "integer",
                paramType = "query"),
        @ApiImplicitParam(
                name = QueryParamKeys.START,
                value = "Index of the first artifact to return",
                required = false,
                dataType = "integer",
                paramType = "query")
      })
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getViewList( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo ) {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        final List< ViewListing > viewDefinitions = new ArrayList<>();

        try {
            final String virtualization = uriInfo.getQueryParameters().getFirst( QueryParamKeys.VIRTUALIZATION );
            
            if (StringUtils.isBlank(virtualization)) {
            	return createErrorResponse(Status.FORBIDDEN, mediaTypes, "VIRTUALIZATION name is required");
            }
            
            // find view editor states
            return runInTransaction(principal, "getViewEditorStates", true, ()->{
            
	            final List<? extends ViewDefinition> viewEditorStates = getWorkspaceManager().getViewDefinitions( virtualization );
	            LOGGER.debug( "getViewEditorStates:found '{0}' ViewEditorStates", viewEditorStates.size() ); //$NON-NLS-1$
	
	            //TODO: paging / sorting can be pushed into the repository
	            //also there's no sort here, perhaps this should be sorted on name
	            
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
	            for ( final ViewDefinition viewEditorState : viewEditorStates ) {
	                if (i < start)
	                    continue;
	
	                if (size != ALL_AVAILABLE && viewDefinitions.size() > size)
	                    continue;
	
	                LOGGER.debug("getViewEditorStates:ViewEditorState '{0}' entity was constructed", viewEditorState.getName()); //$NON-NLS-1$
	                ViewListing listing = new ViewListing();
	                listing.setId(viewEditorState.getId());
	                listing.setName(viewEditorState.getName());
	                listing.setDescription(viewEditorState.getDescription());
	                viewDefinitions.add(listing);
	                ++i;
	            }
	
	            return toResponse(mediaTypes, viewDefinitions );
            });
        } catch ( final Exception e ) {
            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATES_GET_ERROR);
        }
    }

    /**
     * Get the view editor state with the given id from the user's profile
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the view editor state in the user profile (never <code>null</code>)
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
                                    final @PathParam( "viewEditorStateId" ) String viewEditorStateId) {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        try {
        	return runInTransaction(principal, "getViewEditorStates", true, ()->{
                ViewDefinition viewEditorState = getWorkspaceManager().findViewDefinition(viewEditorStateId);
                LOGGER.debug( "getViewEditorState:found '{0}' ViewEditorStates",
                                  viewEditorState == null ? 0 : 1 ); //$NON-NLS-1$

                if (viewEditorState == null)
                    return Response.noContent().build();

                LOGGER.debug("getViewEditorStates:ViewEditorState '{0}' entity was constructed", viewEditorState.getName()); //$NON-NLS-1$
                return toResponse(mediaTypes, viewEditorState );
        	});
        } catch ( final Exception e ) {
            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATES_GET_ERROR);
        }
    }

    /**
     * Stash a ViewEditorState
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return stashed view editor state
     */
    @PUT
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH + V1Constants.VIEW_EDITOR_STATE)
    @ApiOperation( value = "Store view editor state" )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response stashViewEditorState(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo,
                                               @ApiParam(required = true)
                                               final org.komodo.datavirtualization.ViewDefinition restViewEditorState) {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();
        
        if (StringUtils.isBlank(restViewEditorState.getName())) {
        	return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VIEW_DEFINITION_MISSING_NAME);
        }
        
        if (StringUtils.isBlank(restViewEditorState.getDataVirtualizationName())) {
        	return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VIEW_DEFINITION_MISSING_DATAVIRTUALIZATIONNAME);
        }
        
        try {
        	ViewDefinition vd = runInTransaction(principal, "createViewDefinition", false, ()->{
        		return createViewEditorState(restViewEditorState);
        	});

            KomodoStatusObject kso = new KomodoStatusObject("Stash Status"); //$NON-NLS-1$
            kso.addAttribute("Stash Status", "Successfully stashed"); //$NON-NLS-1$
            kso.addAttribute(StringConstants.ID_LABEL, vd.getId());
            return toResponse(mediaTypes, kso);
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATE_CREATE_ERROR);
        }
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
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        
    	LOGGER.info("Validating view : " + restViewDefinition.getName());
    	
        RestViewDefinitionStatus viewDefnStatus = validateViewDefinition(restViewDefinition);

        return toResponse(mediaTypes, viewDefnStatus);
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
     */
    private ViewDefinition createViewEditorState(final ViewDefinition restViewDefn) throws Exception {
    	
    	ViewDefinition viewDefn = null;
    	
    	if (restViewDefn.getId() != null) {
    		viewDefn = getWorkspaceManager().findViewDefinition(restViewDefn.getId());
    	}
    	if (viewDefn == null) {
        	viewDefn = getWorkspaceManager().findViewDefinitionByNameIgnoreCase(restViewDefn.getDataVirtualizationName(), restViewDefn.getName());
    	}
    	
        // Add a new ViewDefinition
    	if (viewDefn == null) {
    		viewDefn = getWorkspaceManager().createViewDefiniton(restViewDefn.getDataVirtualizationName(), restViewDefn.getName());
    	} else {
    		if (restViewDefn.getId() != null && viewDefn.getId() != null && !restViewDefn.getId().endsWith(viewDefn.getDdl())) {
    			throw new IllegalArgumentException("view id does not match the persistent state");
    		}
        	if (!restViewDefn.getName().equals(viewDefn.getName()) || !restViewDefn.getDataVirtualizationName().equals(viewDefn.getDataVirtualizationName())) {
        		throw new IllegalArgumentException("view name / dv name does not match the persistent state");
        	}
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
        // Compositions
        for (SqlComposition restComp: restViewDefn.getCompositions()) {
            SqlComposition sqlComp = viewDefn.addComposition(restComp.getName());
            sqlComp.setDescription(restComp.getDescription());
            sqlComp.setLeftSourcePath(restComp.getLeftSourcePath());
            sqlComp.setRightSourcePath(restComp.getRightSourcePath());
            sqlComp.setLeftCriteriaColumn(restComp.getLeftCriteriaColumn());
            sqlComp.setRightCriteriaColumn(restComp.getRightCriteriaColumn());
            sqlComp.setType(restComp.getType());
            sqlComp.setOperator(restComp.getOperator());
        }
        // Projected Columns
        for (SqlProjectedColumn restCol: restViewDefn.getProjectedColumns()) {
            SqlProjectedColumn sqlProjectedCol = viewDefn.addProjectedColumn(restCol.getName());
            sqlProjectedCol.setType(restCol.getType());
            sqlProjectedCol.setSelected(restCol.isSelected());
        }
        return viewDefn;
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the results of the removal
     */
    @DELETE
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
                                               final @PathParam("viewEditorStateId") String viewEditorStateId) {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        if (StringUtils.isBlank(viewEditorStateId)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_EDITOR_STATE_MISSING_NAME);
        }

        try {
            return runInTransaction(principal, "removeUserProfileViewEditorState", false, ()-> {
                if (!getWorkspaceManager().deleteViewDefinition(viewEditorStateId)) {
                    return Response.noContent().build();
                }

                KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
                kso.addAttribute(viewEditorStateId, "Successfully deleted"); //$NON-NLS-1$

                return toResponse(mediaTypes, kso);
            }); //$NON-NLS-1$
        } catch (final Exception e) {
            return createErrorResponse(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATE_REMOVE_ERROR);
        }
    }
}
