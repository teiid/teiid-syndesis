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

import static org.komodo.rest.relational.RelationalMessages.Error.SCHEMA_SERVICE_GET_SCHEMA_ERROR;

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriInfo;

import org.komodo.core.repository.SynchronousCallback;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.profile.GitRepository;
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
import org.komodo.rest.relational.response.RestGitRepository;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlProjectedColumn;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate.RestStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.StringUtils;
import org.springframework.stereotype.Component;

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

    private static final String REPO_WKSP_LABEL = "Repository Workspace"; //$NON-NLS-1$

    private static final String REPO_CONFIG_LABEL = "Repository Configuration"; //$NON-NLS-1$

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
            Repository repo = this.kengine.getDefaultRepository();
            Id id = repo.getId();
            repoStatus.addAttribute(REPO_WKSP_LABEL, id.getWorkspaceName());
            repoStatus.addAttribute(REPO_CONFIG_LABEL, id.getConfiguration().toString());

            // find VDBs
            uow = systemTx("getVdbs", true); //$NON-NLS-1$
            Vdb[] vdbs = getWorkspaceManager(uow).findVdbs(uow);
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
     * Attempt to import the sample data into the engine
     *
     * @return the response indicating the sample data load has been attempted
     */
    @SuppressWarnings( "nls" )
    @POST
    @Path(V1Constants.SAMPLE_DATA)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Import sample data into VdbBuilder and display the status of the operation",
                             response = KomodoStatusObject.class)
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response importSampleData(final @Context HttpHeaders headers,
                                                                       final @Context UriInfo uriInfo) {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        KomodoStatusObject status = new KomodoStatusObject("Sample Vdb Import");

        for (String sampleName : SAMPLES) {
            InputStream sampleStream = getVdbSample(sampleName);
            if (sampleStream == null) {
                status.addAttribute(sampleName, RelationalMessages.getString(
                                                          RelationalMessages.Error.VDB_SAMPLE_CONTENT_FAILURE, sampleName));
                continue;
            }

            UnitOfWork uow = null;
            try {
                SynchronousCallback callback = new SynchronousCallback();
                uow = createTransaction(principal, "Import vdb " + sampleName, false, callback); //$NON-NLS-1$

                String msg = null;

                ImportOptions importOptions = new ImportOptions();
                importOptions.setOption(OptionKeys.HANDLE_EXISTING, ExistingNodeOptions.RETURN);
                ImportMessages importMessages = new ImportMessages();

                Repository repo = this.kengine.getDefaultRepository();
                KomodoObject workspace = repo.komodoWorkspace(uow);
                VdbImporter importer = new VdbImporter(repo);
                importer.importVdb(uow, sampleStream, workspace, importOptions, importMessages);
                uow.commit();

                List<String> errorMsgs = importMessages.getErrorMessages();
                if (errorMsgs.isEmpty()) {
                    msg = RelationalMessages.getString(
                                                       RelationalMessages.Error.VDB_SAMPLE_IMPORT_SUCCESS,
                                                                                                              sampleName);
                } else if (errorMsgs.iterator().next().contains("node already exists")) {
                    msg = RelationalMessages.getString(
                                                       RelationalMessages.Error.VDB_SAMPLE_IMPORT_VDB_EXISTS,
                                                                                                              sampleName);
                } else {
                    String errMsg = StringUtils.toCommaSeparatedList(errorMsgs.toArray());
                    msg = RelationalMessages.getString(
                                                           RelationalMessages.Error.VDB_SAMPLE_IMPORT_ERRORS,
                                                                                                               sampleName, errMsg);
                }

                if (callback.await(3, TimeUnit.MINUTES)) {
                    status.addAttribute(sampleName, msg);
                } else {
                    status.addAttribute(sampleName, RelationalMessages.getString(
                                                                                 RelationalMessages.Error.VDB_SAMPLE_IMPORT_TIMEOUT,
                                                                                 sampleName, msg));
                }

            } catch ( final Exception e ) {
                if ( ( uow != null ) && ( uow.getState() != State.COMMITTED ) ) {
                    uow.rollback();
                }

                status.addAttribute(sampleName, RelationalMessages.getString(
                                                                             RelationalMessages.Error.VDB_SERVICE_LOAD_SAMPLE_ERROR, sampleName, e));
            }
        }

        ResponseBuilder builder = Response.ok( KomodoJsonMarshaller.marshall(status, true), MediaType.APPLICATION_JSON );
        return builder.build();
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @param ktype
     *        the komodo type parameter
     * @return a JSON document representing the schema of the teiid VDB (never <code>null</code>).
     *                If a ktype parameter is specified conforming to a KomodoType then only the associated
     *                element of the teiid schema is returned.
     * @throws KomodoRestException
     *         if there is a problem constructing the VDBs JSON document
     */
    @GET
    @Path(V1Constants.SCHEMA_SEGMENT)
    @Produces( MediaType.APPLICATION_JSON )
    @ApiOperation(value = "Display the schema structure of the teiid vdb",
                            response = String.class)
    @ApiResponses(value = {
        @ApiResponse(code = 404, message = "If ktype is not a recognised type"),
        @ApiResponse(code = 404, message = "If ktype is recognised but not associated with a teiid schema element"),
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response getSchema( final @Context HttpHeaders headers,
                             final @Context UriInfo uriInfo,
                             @ApiParam(
                                       value = "Type of schema element to be returned",
                                       allowableValues = "Vdb, VdbImport, Connection," +
                                                                             "VdbTranslator, Model, " +
                                                                             "VdbModelSource, VdbDataRole, " +
                                                                             "VdbPermission, VdbCondition, VdbMask",
                                       required = false,
                                       allowMultiple = false
                             )
                             @QueryParam(value = "ktype") String ktype) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        try {

            String schema = null;
            if (ktype == null) {
                //
                // Request to return whole of schema
                //
                schema = KomodoJsonMarshaller.teiidElementSchema(null);
                return Response.ok().entity(schema).build();
            }

            KomodoType komodoType = KomodoType.getKomodoType(ktype);
            if (komodoType == null) {
                return createErrorResponse(Status.NOT_FOUND, mediaTypes, RelationalMessages.Error.SCHEMA_SERVICE_GET_SCHEMA_UNKNOWN_KTYPE, ktype);
            } else {
                schema = KomodoJsonMarshaller.teiidElementSchema(komodoType);
                if (EMPTY_STRING.equals(schema)) {
                    return createErrorResponse(Status.NOT_FOUND, mediaTypes, RelationalMessages.Error.SCHEMA_SERVICE_GET_SCHEMA_NOT_FOUND, ktype);
                }
            }

            return Response.ok().entity(schema).build();

        } catch ( final Exception e ) {
            if ( e instanceof KomodoRestException ) {
                throw ( KomodoRestException )e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, SCHEMA_SERVICE_GET_SCHEMA_ERROR);
        }
    }

    /**
     * @param headers
     *            the request headers (never <code>null</code>)
     * @param uriInfo
     *            the request URI information (never <code>null</code>)
     * @param validateValue
     *            the value being validated (cannot be empty)
     * @return the response (never <code>null</code>) with an entity that is
     *         either an empty string, when the name is valid, or an error
     *         message
     * @throws KomodoRestException
     *             if there is a problem validating the value or constructing
     *             the response
     */
    @GET
    @Path( V1Constants.VALIDATE_SEGMENT + StringConstants.FORWARD_SLASH + V1Constants.VALIDATE_PLACEHOLDER )
    @Produces( { MediaType.TEXT_PLAIN } )
    @ApiOperation( value = "Returns an error message if the value is invalid" )
    @ApiResponses( value = {
            @ApiResponse( code = 400, message = "The URI cannot contain encoded slashes or backslashes." ),
            @ApiResponse( code = 403, message = "An unexpected error has occurred." ),
            @ApiResponse( code = 500, message = "The value cannot be empty." )
    } )
    public Response validateValue( final @Context HttpHeaders headers,
                                     final @Context UriInfo uriInfo,
                                     @ApiParam( value = "The value being checked", required = true )
                                     final @PathParam( "validateValue" ) String validateValue ) throws KomodoRestException {

        final SecurityPrincipal principal = checkSecurityContext( headers );

        if ( principal.hasErrorResponse() ) {
            return principal.getErrorResponse();
        }

        final String errorMsg = VALIDATOR.checkValidName( validateValue );

        // a name validation error occurred
        if ( errorMsg != null ) {
            return Response.ok().entity( errorMsg ).build();
        }

        return Response.ok().build();
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return user profile
     * @throws KomodoRestException if error occurs
     */
    @GET
    @Path(V1Constants.USER_PROFILE)
    @ApiOperation( value = "Display profile of the individual user", response = String.class )
    @ApiResponses(value = {
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response userProfile(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        KomodoStatusObject userProfileStatus = new KomodoStatusObject();

        userProfileStatus.addAttribute(USER_NAME, principal.getUserName());

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "userProfile", true ); //$NON-NLS-1$

            Repository repo = this.kengine.getDefaultRepository();
            KomodoObject workspace = repo.komodoWorkspace(uow);
            userProfileStatus.addAttribute(WORKSPACE, workspace.getAbsolutePath());

            Profile userProfile = getUserProfile(uow);
            for (GitRepository repository : userProfile.getGitRepositories(uow)) {
                RestGitRepository restRepo = new RestGitRepository(uriInfo.getBaseUri(), repository, uow);
                String jsonRepo = KomodoJsonMarshaller.marshall(restRepo);
                userProfileStatus.addAttribute("Git Repository", jsonRepo);
            }

            Vdb[] vdbs = getWorkspaceManager(uow).findVdbs(uow);
            userProfileStatus.addAttribute(REPO_VDB_TOTAL, Integer.toString(vdbs.length));
        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            String errorMsg = e.getLocalizedMessage() != null ? e.getLocalizedMessage() : e.getClass().getSimpleName();
            errorMsg = RelationalMessages.getString(RelationalMessages.Error.USER_PROFILE_SERVICE_ERROR, errorMsg);
            userProfileStatus.addAttribute(REPO_VDB_TOTAL, errorMsg);
        }

        // create response
        try {
            return commit(uow, mediaTypes, userProfileStatus);
        } catch (Exception ex) {
            return createErrorResponseWithForbidden(mediaTypes, ex, RelationalMessages.Error.USER_PROFILE_SERVICE_ERROR);
        }
    }

    /**
     * @param headers
     *        the request headers (never <code>null</code>)
     * @param uriInfo
     *        the request URI information (never <code>null</code>)
     * @return stashed git repository
     * @throws KomodoRestException if error occurs
     */
    @PUT
    @Path(V1Constants.USER_PROFILE + FORWARD_SLASH + V1Constants.GIT_REPOSITORY)
    @ApiOperation( value = "Store a git repository configuration in the user's profile", response = GitRepository.class )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response stashGitRepository(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo,
                                               @ApiParam(
                                                         value = "" +
                                                                 "JSON of the git repository configuration:<br>" +
                                                                 OPEN_PRE_TAG +
                                                                 OPEN_BRACE + BR +
                                                                 NBSP + RestGitRepository.NAME_LABEL + ": \"Unqiue name or identifier of the git repository\"" + BR +
                                                                 NBSP + RestGitRepository.URL_LABEL + ": \"Destination git repository url\"" + COMMA + BR +
                                                                 NBSP + RestGitRepository.USER_LABEL + ": \"User name to access the git repository\"" + COMMA + BR +
                                                                 NBSP + RestGitRepository.PASSWORD_LABEL + ": \"Password to access the git repository\"" + COMMA + BR +
                                                                 NBSP + RestGitRepository.COMMIT_AUTHOR_LABEL + ": \"Name of author to use for commits\"" + COMMA + BR +
                                                                 NBSP + RestGitRepository.COMMIT_EMAIL_LABEL + ": \"Email of author to use for commits\"" + COMMA + BR +
                                                                 NBSP + RestGitRepository.BRANCH_LABEL + ": \"The git repository branch\"" + BR +
                                                                 NBSP + OPEN_PRE_CMT + "(Optional. By default: \"master\")" + CLOSE_PRE_CMT  + BR +
                                                                 CLOSE_BRACE +
                                                                 CLOSE_PRE_TAG,
                                                         required = true
                                               )
                                               final String gitRepositoryConfig) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        RestGitRepository restGitRepo = KomodoJsonMarshaller.unmarshall(gitRepositoryConfig, RestGitRepository.class);
        String repoName = restGitRepo.getName();
        String repoUrlStr = restGitRepo.getUrl();
        String repoUser = restGitRepo.getUser();
        String repoPassword = restGitRepo.getPassword();

        if (StringUtils.isBlank(repoName)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_GIT_REPO_MISSING_REPO_NAME);
        }

        if (StringUtils.isBlank(repoUrlStr)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_GIT_REPO_CREATE_MISSING_REPO_URL);
        }

        if (StringUtils.isBlank(repoUser)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_GIT_REPO_CREATE_MISSING_REPO_USER);
        }

        if (StringUtils.isBlank(repoPassword)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_GIT_REPO_CREATE_MISSING_REPO_PASSWORD);
        }

        URL repoUrl;
        try {
            repoUrl = new URL(repoUrlStr);
        } catch (MalformedURLException ex) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_GIT_REPO_CREATE_MALFORMED_URL);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "addUserProfileGitRepository", false); //$NON-NLS-1$

            Profile userProfile = getUserProfile(uow);

            //
            // Secure the password using encryption
            //
            String encrypted = encryptSensitiveData(headers, principal.getUserName(), repoPassword);
            if (encrypted == null)
                return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.ENCRYPT_FAILURE, principal.getUserName());

            encrypted = ENCRYPTED_PREFIX + encrypted;
            GitRepository gitRepository = userProfile.addGitRepository(uow, repoName, repoUrl,
                                                                                                                                     repoUser, encrypted);
            String branch = restGitRepo.getBranch();
            if (branch != null)
                gitRepository.setBranch(uow, branch);

            String commitAuthor = restGitRepo.getCommitAuthor();
            if (commitAuthor != null)
                gitRepository.setCommitAuthor(uow, commitAuthor);

            String commitEmail = restGitRepo.getCommitEmail();
            if (commitEmail != null)
                gitRepository.setCommitEmail(uow, commitEmail);

            final RestGitRepository entity = new RestGitRepository(uriInfo.getBaseUri(), gitRepository, uow);
            return commit(uow, mediaTypes, entity);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PROFILE_GIT_REPO_CREATE_ERROR);
        }
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
                  V1Constants.GIT_REPOSITORY + FORWARD_SLASH +
                  V1Constants.GIT_REPO_PLACEHOLDER)
    @ApiOperation( value = "Remove a git repository configuration from the user's profile", response = String.class )
    @ApiResponses(value = {
        @ApiResponse(code = 406, message = "Only JSON is returned by this operation"),
        @ApiResponse(code = 403, message = "An error has occurred.")
    })
    public Response removeGitRepository(final @Context HttpHeaders headers,
                                               final @Context UriInfo uriInfo,
                                               @ApiParam(
                                                         value = "Name of the git repository to remove",
                                                         required = true
                                               )
                                               final @PathParam("gitRepositoryName") String gitRepositoryName) throws KomodoRestException {

        SecurityPrincipal principal = checkSecurityContext(headers);
        if (principal.hasErrorResponse())
            return principal.getErrorResponse();

        List<MediaType> mediaTypes = headers.getAcceptableMediaTypes();
        if (! isAcceptable(mediaTypes, MediaType.APPLICATION_JSON_TYPE))
            return notAcceptableMediaTypesBuilder().build();

        if (StringUtils.isBlank(gitRepositoryName)) {
            return createErrorResponseWithForbidden(mediaTypes, RelationalMessages.Error.PROFILE_GIT_REPO_MISSING_REPO_NAME);
        }

        UnitOfWork uow = null;
        try {
            uow = createTransaction(principal, "removeUserProfileGitRepository", false); //$NON-NLS-1$

            Profile userProfile = getUserProfile(uow);
            GitRepository[] repositories = userProfile.getGitRepositories(uow, gitRepositoryName);
            if (repositories.length == 0)
                return Response.noContent().build();

            userProfile.removeGitRepository(uow, gitRepositoryName);

            KomodoStatusObject kso = new KomodoStatusObject("Delete Status"); //$NON-NLS-1$
            kso.addAttribute(gitRepositoryName, "Successfully deleted"); //$NON-NLS-1$

            return commit(uow, mediaTypes, kso);

        } catch (final Exception e) {
            if ((uow != null) && (uow.getState() != State.ROLLED_BACK)) {
                uow.rollback();
            }

            if (e instanceof KomodoRestException) {
                throw (KomodoRestException)e;
            }

            return createErrorResponseWithForbidden(mediaTypes, e, RelationalMessages.Error.PROFILE_GIT_REPO_REMOVE_ERROR);
        }
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

            final ViewEditorState[] viewEditorStates = getViewEditorStates(uow, searchPattern);
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

                RestViewEditorState restviewEditorState = new RestViewEditorState(uriInfo.getBaseUri(), viewEditorState, uow);
                LOGGER.debug("getViewEditorStates:ViewEditorState '{0}' entity was constructed", viewEditorState.getName(uow)); //$NON-NLS-1$
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
            Profile profile = getUserProfile(uow);
            ViewEditorState[] viewEditorStates = profile.getViewEditorStates(uow, viewEditorStateId);
            LOGGER.debug( "getViewEditorState:found '{0}' ViewEditorStates",
                              viewEditorStates == null ? 0 : viewEditorStates.length ); //$NON-NLS-1$

            if (viewEditorStates == null || viewEditorStates.length == 0)
                return Response.noContent().build();

            RestViewEditorState restViewEditorState = new RestViewEditorState(uriInfo.getBaseUri(), viewEditorStates[0], uow);
            LOGGER.debug("getViewEditorStates:ViewEditorState '{0}' entity was constructed", viewEditorStates[0].getName(uow)); //$NON-NLS-1$
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
                createViewEditorState(restViewEditorState, uow);
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
     * Creates the view editor state from the RestViewEditorState
     * @param editorState the state
     * @param uow the transaction
     * @return the ViewEditorState repo object
     * @throws Exception exception if a problem is encountered
     */
    private ViewEditorState createViewEditorState(final RestViewEditorState editorState, UnitOfWork uow) throws Exception {
        String stateId = editorState.getId();
        RestStateCommandAggregate[] commands = editorState.getCommands();
        RestViewDefinition restViewDefn = editorState.getViewDefinition();

        // Add a new ViewEditorState to the userProfile
        Profile userProfile = getUserProfile(uow);
        ViewEditorState viewEditorState = userProfile.addViewEditorState(uow, stateId);

        // Add commands to the ViewEditorState
        for (RestStateCommandAggregate restCmd : commands) {
            RestStateCommand restUndo = restCmd.getUndo();
            RestStateCommand restRedo = restCmd.getRedo();

            StateCommandAggregate stateCmdAgg = viewEditorState.addCommand(uow);
            stateCmdAgg.setUndo(uow, restUndo.getId(), restUndo.getArguments());
            stateCmdAgg.setRedo(uow, restRedo.getId(), restRedo.getArguments());
        }

        // Set ViewDefinition of the ViewEditorState
        ViewDefinition viewDefn = viewEditorState.setViewDefinition(uow);
        viewDefn.setViewName(uow, restViewDefn.getViewName());
        viewDefn.setDescription(uow, restViewDefn.getDescription());
        for (String restSourcePath: restViewDefn.getSourcePaths()) {
            viewDefn.addSourcePath(uow, restSourcePath);
        }
        viewDefn.setComplete(uow, restViewDefn.isComplete());
        // Compositions
        for (RestSqlComposition restComp: restViewDefn.getSqlCompositions()) {
            SqlComposition sqlComp = viewDefn.addSqlComposition(uow, restComp.getId());
            sqlComp.setDescription(uow, restComp.getDescription());
            sqlComp.setLeftSourcePath(uow, restComp.getLeftSourcePath());
            sqlComp.setRightSourcePath(uow, restComp.getRightSourcePath());
            sqlComp.setLeftCriteriaColumn(uow, restComp.getLeftCriteriaColumn());
            sqlComp.setRightCriteriaColumn(uow, restComp.getRightCriteriaColumn());
            sqlComp.setType(uow, restComp.getType());
            sqlComp.setOperator(uow, restComp.getOperator());
        }
        // Projected Columns
        for (RestSqlProjectedColumn restCol: restViewDefn.getProjectedColumns()) {
            SqlProjectedColumn sqlProjectedCol = viewDefn.addProjectedColumn(uow, restCol.getName());
            sqlProjectedCol.setName(uow, restCol.getName());
            sqlProjectedCol.setType(uow, restCol.getType());
            sqlProjectedCol.setSelected(uow, restCol.isSelected());
        }
        return viewEditorState;
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

            if (!removeEditorState(uow, viewEditorStateId)) {
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
