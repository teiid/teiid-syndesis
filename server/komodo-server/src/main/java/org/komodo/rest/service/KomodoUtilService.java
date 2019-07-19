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

import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestException;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlProjectedColumn;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate.RestStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinitionStatus;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.springframework.stereotype.Component;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.metadata.AbstractMetadataRecord;
import org.teiid.metadata.MetadataFactory;
import org.teiid.query.metadata.CompositeMetadataStore;
import org.teiid.query.metadata.MetadataValidator;
import org.teiid.query.metadata.SystemMetadata;
import org.teiid.query.metadata.TransformationMetadata;
import org.teiid.query.parser.QueryParser;
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

    private static final String REPO_VDB_TOTAL = "Repository Vdb Total"; //$NON-NLS-1$

    public static final String APP_NAME = "App Name"; //$NON-NLS-1$

    public static final String APP_TITLE = "App Title"; //$NON-NLS-1$

    public static final String APP_DESCRIPTION = "App Description"; //$NON-NLS-1$

    public static final String APP_VERSION = "App Version"; //$NON-NLS-1$

    public static final String USER_NAME = "User Name"; //$NON-NLS-1$

    public static final String WORKSPACE = "Workspace"; //$NON-NLS-1$

    /**
     * The sample vdbs provided by this service
     */
    @SuppressWarnings( "nls" )
    public static final String[] SAMPLES = {
        "parts_dynamic-vdb.xml", "portfolio-vdb.xml",
        "teiid-vdb-all-elements.xml", "tweet-example-vdb.xml",
        "northwind.xml", "financials.xml"
    };


    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return about information of this service
     * @throws KomodoRestException if error occurs
     */
    @GET
    @Path(V1Constants.ABOUT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation( value = "Display status of this rest service", response = String.class )
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response about(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        KomodoStatusObject repoStatus = new KomodoStatusObject();

        repoStatus.addAttribute(APP_NAME, KomodoRestV1Application.V1Constants.App.name());
        repoStatus.addAttribute(APP_TITLE, KomodoRestV1Application.V1Constants.App.title());
        repoStatus.addAttribute(APP_DESCRIPTION, KomodoRestV1Application.V1Constants.App.description());
        repoStatus.addAttribute(APP_VERSION, KomodoRestV1Application.V1Constants.App.version());

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            // find VDBs
            uow = systemTx("getVdbs", true); //$NON-NLS-1$
            Vdb[] vdbs = getWorkspaceManager().findVdbs(null);
            repoStatus.addAttribute(REPO_VDB_TOTAL, Integer.toString(vdbs.length));

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            errorMsg = RelationalMessages.getString(RelationalMessages.Error.ABOUT_SERVICE_ERROR, errorMsg);
            repoStatus.addAttribute(REPO_VDB_TOTAL, errorMsg);
        }

        // create response
        try {
            return commit(uow, mediaTypes, repoStatus);
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.ABOUT_SERVICE_ERROR);
        }
    }

    /**
     * @param sampleName
     * @return the sample content for the given sample name
     */
    public static InputStream getVdbSample(String sampleName) {
        String sampleFilePath = "sample" + File.separator + sampleName; //$NON-NLS-1$
        InputStream fileStream = KomodoUtilService.class.getResourceAsStream(sampleFilePath);
        if (fileStream == null)
            LOGGER.error(RelationalMessages.getString(
                                                      RelationalMessages.Error.VDB_SAMPLE_CONTENT_FAILURE, sampleName));

        else
            LOGGER.info(RelationalMessages.getString(
                                                     RelationalMessages.Error.VDB_SAMPLE_CONTENT_SUCCESS, sampleName));

        return fileStream;
    }

    /**
     * Get all view editor states from the user's profile
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the view editor states in the user profile (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Connection JSON document
     */
    @GET
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH + V1Constants.VIEW_EDITOR_STATE)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Return the collection of view editor states",
                  response = RestViewEditorState[].class)
    @ApiImplicitParams({
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
                value = "Index of the first artifact to return",
                required = false,
                dataType = "integer",
                paramType = "query")
      })
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getViewEditorStates( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo ) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        final List< RestViewEditorState > restViewEditorStates = new ArrayList<>();

        try {

            final String searchPattern = uriInfo.getQueryParameters().getFirst( QueryParamKeys.PATTERN );

            // find view editor states
            final String txId = "getViewEditorStates"; //$NON-NLS-1$ //$NON-NLS-2$
            uow = createTransaction(principal, txId, true );

            final ViewEditorState[] viewEditorStates = getViewEditorStates(searchPattern);
            LOGGER.debug( "getViewEditorStates:found '{0}' ViewEditorStates", viewEditorStates.length ); //$NON-NLS-1$

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
            for ( final ViewEditorState viewEditorState : viewEditorStates ) {
                if (i < start)
                    continue;

                if (size != ALL_AVAILABLE && restViewEditorStates.size() > size)
                    continue;

                RestViewEditorState restviewEditorState = new RestViewEditorState(uriInfo.getBaseUri(), viewEditorState);
                LOGGER.debug("getViewEditorStates:ViewEditorState '{0}' entity was constructed", viewEditorState.getName()); //$NON-NLS-1$
                restViewEditorStates.add(restviewEditorState);
                ++i;
            }

            return commit( uow, mediaTypes, restViewEditorStates );
        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATES_GET_ERROR);
        }
    }

    /**
     * Get the view editor state with the given id from the user's profile
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the view editor state in the user profile (never <code>null</code>)
     * @throws KomodoRestException
     *         if there is a problem constructing the Connection JSON document
     */
    @GET
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH +
                  V1Constants.VIEW_EDITOR_STATE + FORWARD_SLASH +
                  V1Constants.VIEW_EDITOR_STATE_PLACEHOLDER)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Returns the view editor state with the given id",
                  response = RestViewEditorState.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getViewEditorState( final @Context HttpHeaders headers,
                                    final @Context UriInfo uriInfo,
                                    @ApiParam(value = "Name of the view editor state to fetch", required = true)
                                    final @PathParam( "viewEditorStateId" ) String viewEditorStateId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;

        try {

            final String txId = "getViewEditorStates"; //$NON-NLS-1$ //$NON-NLS-2$
            uow = createTransaction(principal, txId, true );
            Profile profile = getUserProfile();
            ViewEditorState viewEditorState = profile.getViewEditorState(viewEditorStateId);
            LOGGER.debug( "getViewEditorState:found '{0}' ViewEditorStates",
                              viewEditorState == null ? 0 : 1 ); //$NON-NLS-1$

            if (viewEditorState == null)
                return Response.noContent().build();

            RestViewEditorState restViewEditorState = new RestViewEditorState(uriInfo.getBaseUri(), viewEditorState);
            LOGGER.debug("getViewEditorStates:ViewEditorState '{0}' entity was constructed", viewEditorState.getName()); //$NON-NLS-1$
            return commit( uow, mediaTypes, restViewEditorState );

        } catch ( final Exception e ) {
            if ( ( uow != null ) && ( uow.getState() != State.ROLLED_BACK ) ) {
                uow.rollback();
            }

            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATES_GET_ERROR);
        }
    }

    /**
     * Stash an array of ViewEditorStates
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return stashed view editor state
     * @throws KomodoRestException if error occurs
     */
    @PUT
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH + V1Constants.VIEW_EDITOR_STATES)
    @ApiOperation( value = "Store multiple view editor states in the user's profile", response = ViewEditorState.class )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response stashViewEditorStates(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo,
                                               @ApiParam(
                                                         value = "" +
                                                                 "JSON of the view editor state:<br>" +
                                                                 OPEN_PRE_TAG + OPEN_BRACKET + BR +
                                                                 OPEN_BRACE + BR +
                                                                 NBSP + RestViewEditorState.ID_LABEL + ": \"Unqiue name or identifier of the view editor state\"" + BR +
                                                                 NBSP + RestViewEditorState.CONTENT_LABEL + ": { ... \"The content of the state\" ... }" + BR +
                                                                 NBSP + RestViewEditorState.VIEW_DEFINITION_LABEL + ": { ... \"The view definition content\" ... }" + BR +
                                                                 CLOSE_BRACE + BR +
                                                                 CLOSE_BRACE +
                                                                 CLOSE_PRE_TAG,
                                                         required = true
                                               )
                                               final String viewEditorStateConfig) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        RestViewEditorState[] restViewEditorStates = KomodoJsonMarshaller.unmarshallArray(viewEditorStateConfig, RestViewEditorState[].class);

        // Validate the RestViewEditorStates, return if any errors found
        for (RestViewEditorState restViewEditorState : restViewEditorStates) {
            Response resp = this.checkRestEditorState(mediaTypes, restViewEditorState);
            if (resp != null) return resp;
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "addUserProfileViewEditorState", false); //$NON-NLS-1$

            // Create the ViewEditorState objects
            for (RestViewEditorState restViewEditorState : restViewEditorStates) {
                createViewEditorState(restViewEditorState);
            }

            KomodoStatusObject kso = new KomodoStatusObject("Stash Status"); //$NON-NLS-1$
            kso.addAttribute("Stash Status", "Successfully stashed"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATE_CREATE_ERROR);
        }
    }
    
    /**
     * Validate the supplied ViewDefinition
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return validation status of the supplied ViewDefinition
     * @throws KomodoRestException if error occurs
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
                                               @ApiParam(
                                                         value = "" +
                                                                 "JSON of the view definition:<br>" +
                                                                 OPEN_PRE_TAG + OPEN_BRACE + BR +
                                                                 NBSP + RestViewEditorState.ID_VIEW_NAME + ": \"Name of the view definition\"" + BR +
                                                                 NBSP + RestViewEditorState.DDL + ": { ... \"View definition DDL\" ... }" + BR +
                                                                 CLOSE_BRACE +
                                                                 CLOSE_PRE_TAG,
                                                         required = true
                                               )
                                               final String viewDefinitionConfig) throws KomodoRestException {
        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        RestViewDefinition restViewDefinition = KomodoJsonMarshaller.unmarshall(viewDefinitionConfig, RestViewDefinition.class);
        
    	LOGGER.info("Validating view : " + restViewDefinition.getViewName());
    	
        // Make sure the RestViewDefinition is valid
        Response resp = this.checkRestViewDefinition(mediaTypes, restViewDefinition);
        if (resp != null) return resp;

        RestViewDefinitionStatus viewDefnStatus = new RestViewDefinitionStatus();

        // If user-defined view, make sure DDL name matches ViewDefinition
        boolean namesMatch = true;
        String defnDdl = restViewDefinition.getDdl();
        if(!StringUtils.isBlank(defnDdl)) {
        	String ddlViewName = getDdlViewName(defnDdl);
        	// If names do not match, create an error status
        	if(!restViewDefinition.getViewName().equalsIgnoreCase(ddlViewName)) {
        		namesMatch = false;
                String errorMsg = RelationalMessages.getString(RelationalMessages.Error.VALIDATE_VIEW_DEFINITION_NAME_MATCH_ERROR, ddlViewName, restViewDefinition.getViewName());
    			viewDefnStatus.setStatus("ERROR");
    			viewDefnStatus.setMessage(errorMsg);
        	}
        }
        
        // Name is ok, do full parse if ddl is defined
        if( namesMatch && !StringUtils.isBlank(defnDdl)) {
	        try {
	        	QueryParser parser = QueryParser.getQueryParser();
	        	
	            ModelMetaData m = new ModelMetaData();
	            m.setName("m"); //$NON-NLS-1$
	        	
				MetadataFactory mf = new MetadataFactory("PreviewVdb", 1,SystemMetadata.getInstance().getRuntimeTypeMap(),m);
	        	parser.parseDDL(mf, restViewDefinition.getDdl());
	        	
				VDBMetaData vdb = (VDBMetaData) this.kengine.getMetadataInstance().getAdmin()
						.getVDB("PreviewVdb", "1");
				TransformationMetadata qmi = vdb.getAttachment(TransformationMetadata.class);
	        	
				CompositeMetadataStore store = qmi.getMetadataStore();
				mf.mergeInto(store);
				
				ValidatorReport report = new ValidatorReport();
	        	MetadataValidator validator = new MetadataValidator();
	        	for (AbstractMetadataRecord record : mf.getSchema().getResolvingOrder()) {
	        		validator.validate(vdb, m, record, report, qmi, mf, parser);
	        	}
	        	
	        	store.removeSchema("m");
	        	
	        	String error = report.getFailureMessage();
	        	if (report.hasItems() && !error.isEmpty()) {
	            	viewDefnStatus.setStatus("ERROR");
	            	viewDefnStatus.setMessage(error);        		
	        	} else {
		        	viewDefnStatus.setStatus("SUCCESS");
		        	viewDefnStatus.setMessage("View DDL was parsed successfully");
	        	}
			} catch (Exception ex) {
				String msg = "Parsing Error for view: " + restViewDefinition.getViewName()
					+ "\n" + ex.getMessage();
	        	LOGGER.error(msg);
	        	viewDefnStatus.setStatus("ERROR");
	        	viewDefnStatus.setMessage("Parsing Error\n" + ex.getMessage());
			}
        }

        UnitOfWork uow = null;
        
        try {
        	uow = createTransaction(principal, "Validate ViewDefinition", true); //$NON-NLS-1$
            return commit(uow, mediaTypes, viewDefnStatus);
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.VALIDATE_VIEW_DEFINITION_ERROR);
        }
    }

    /**
     * Check the RestViewEditorState for correctness before proceeding.  If no errors are found, the return value is null
     * @param mediaTypes the media types
     * @param restEditorState the editor state
     * @return the error response; null if no error found
     */
    private Response checkRestEditorState(final List<MediaType> mediaTypes, final RestViewEditorState restEditorState) {
        String stateId = restEditorState.getId();
        RestStateCommandAggregate[] commands = restEditorState.getCommands();
        RestViewDefinition restViewDefn = restEditorState.getViewDefinition();

        if (StringUtils.isBlank(stateId)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_EDITOR_STATE_MISSING_ID);
        }

        if (commands == null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_EDITOR_STATE_MISSING_COMMANDS);
        }

        if (restViewDefn == null) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_EDITOR_STATE_MISSING_VIEW_DEFINITION);
        }
        return null;
    }

    /**
     * Check the RestViewDefinition for correctness before proceeding.  If no errors are found, the return value is null
     * @param restViewDefinition the view definition
     * @return the error message - null if none found
     */
    private Response checkRestViewDefinition(final List<MediaType> mediaTypes, final RestViewDefinition restViewDefinition) {
    	String viewName = restViewDefinition.getViewName();
    	String viewDdl = restViewDefinition.getDdl();

        if (StringUtils.isBlank(viewName)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VIEW_DEFINITION_MISSING_NAME);
        }

        if (StringUtils.isBlank(viewDdl)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.VIEW_DEFINITION_MISSING_DDL);
        }

        return null;
    }

    /**
     * Creates the view editor state from the RestViewEditorState
     * @param editorState the state
     * @return the ViewEditorState repo object
     * @throws Exception exception if a problem is encountered
     */
    private ViewEditorState createViewEditorState(final RestViewEditorState editorState) throws Exception {
        String stateId = editorState.getId();
        RestStateCommandAggregate[] commands = editorState.getCommands();
        RestViewDefinition restViewDefn = editorState.getViewDefinition();

        // Add a new ViewEditorState to the userProfile
        Profile userProfile = getUserProfile();
        ViewEditorState viewEditorState = userProfile.addViewEditorState(stateId);

        // Add commands to the ViewEditorState
        for (RestStateCommandAggregate restCmd : commands) {
            RestStateCommand restUndo = restCmd.getUndo();
            RestStateCommand restRedo = restCmd.getRedo();

            StateCommandAggregate stateCmdAgg = viewEditorState.addCommand();
            stateCmdAgg.setUndo(restUndo.getId(), restUndo.getArguments());
            stateCmdAgg.setRedo(restRedo.getId(), restRedo.getArguments());
        }

        // Set ViewDefinition of the ViewEditorState
        ViewDefinition viewDefn = viewEditorState.setViewDefinition();
        viewDefn.setViewName(restViewDefn.getViewName());
        viewDefn.setDdl(restViewDefn.getDdl());
        // If user-defined, user may have changed description.  Reset object description from DDL
        if(restViewDefn.isUserDefined()) {
          String ddlDescr = getDdlViewDescription(restViewDefn.getDdl());
          viewDefn.setDescription(ddlDescr);
        } else {
          viewDefn.setDescription(restViewDefn.getDescription());
        }
        for (String restSourcePath: restViewDefn.getSourcePaths()) {
            viewDefn.addSourcePath(restSourcePath);
        }
        viewDefn.setComplete(restViewDefn.isComplete());
        viewDefn.setUserDefined(restViewDefn.isUserDefined());
        // Compositions
        for (RestSqlComposition restComp: restViewDefn.getSqlCompositions()) {
            SqlComposition sqlComp = viewDefn.addSqlComposition(restComp.getId());
            sqlComp.setDescription(restComp.getDescription());
            sqlComp.setLeftSourcePath(restComp.getLeftSourcePath());
            sqlComp.setRightSourcePath(restComp.getRightSourcePath());
            sqlComp.setLeftCriteriaColumn(restComp.getLeftCriteriaColumn());
            sqlComp.setRightCriteriaColumn(restComp.getRightCriteriaColumn());
            sqlComp.setType(restComp.getType());
            sqlComp.setOperator(restComp.getOperator());
        }
        // Projected Columns
        for (RestSqlProjectedColumn restCol: restViewDefn.getProjectedColumns()) {
            SqlProjectedColumn sqlProjectedCol = viewDefn.addProjectedColumn(restCol.getName());
            sqlProjectedCol.setName(restCol.getName());
            sqlProjectedCol.setType(restCol.getType());
            sqlProjectedCol.setSelected(restCol.isSelected());
        }
        return viewEditorState;
    }

    /**
     * Find and return the VIEW name from the supplied View DDL.  If none found, returns empty string
     * @param ddl the VIEW ddl
     * @return view name
     */
    private String getDdlViewName(final String ddl) {
    	String viewName = "";
    	if(ddl != null) {
        	// Replace tab and return characters with space, then remove all extra spaces
        	String trimmedDdl = ddl.replaceAll("[\\n\\t]", " ").trim().replaceAll("\\s{2,}", " ");
        	
        	// Split string using space
        	String[] tokens = trimmedDdl.split(" ");
        	
        	// view name will follow 'CREATE VIEW'
        	if( tokens.length > 2 && tokens[0].equalsIgnoreCase("CREATE") && tokens[1].equalsIgnoreCase("VIEW")) {
        		viewName = tokens[2].trim();
        	}
    	}
    	return viewName;
    }

    /**
     * Find and return the VIEW table ANNOTATION from the supplied View DDL.  If none found, returns empty string
     * @param ddl the VIEW ddl
     * @return the table description
     */
    private String getDdlViewDescription(final String ddl) {
    	String viewDescription = "";
    	if(ddl != null) {
    		// Replace tab and return characters with space, then remove all extra spaces
    		String trimmedDdl = ddl.replaceAll("[\\n\\t]", " ").trim().replaceAll("\\s{2,}", " ");

    		int annotationIndx = trimmedDdl.indexOf("ANNOTATION");
    		if(annotationIndx != -1) {
    			String annotationStr = trimmedDdl.substring(annotationIndx);
    			int firstTickIndx = annotationStr.indexOf(StringConstants.QUOTE_MARK);
    			if(firstTickIndx != -1) {
    				String descStr = annotationStr.substring(firstTickIndx+1);
    				int secondTickIndx = descStr.indexOf(StringConstants.QUOTE_MARK);
    				viewDescription = descStr.substring(0, secondTickIndx);
    			}
    		}
    	}
    	return viewDescription;
    }
    
    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return a JSON document representing the results of the removal
     * @throws KomodoRestException if error occurs
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
                                               final @PathParam("viewEditorStateId") String viewEditorStateId) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        if (StringUtils.isBlank(viewEditorStateId)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_EDITOR_STATE_MISSING_ID);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeUserProfileViewEditorState", false); //$NON-NLS-1$

            if (!removeEditorState(viewEditorStateId)) {
                return Response.noContent().build();
            }

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(viewEditorStateId, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PROFILE_EDITOR_STATE_REMOVE_ERROR);
        }
    }
}
