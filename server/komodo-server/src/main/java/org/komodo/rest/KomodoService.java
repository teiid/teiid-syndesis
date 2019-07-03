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
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.COMMIT_TIMEOUT;
import static org.komodo.rest.Messages.Error.RESOURCE_NOT_FOUND;
import static org.komodo.rest.Messages.General.GET_OPERATION_NAME;

import java.util.Base64;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.Variant;
import javax.ws.rs.core.Variant.VariantListBuilder;

import org.komodo.core.KEngine;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestBasicEntity.ResourceNotFound;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.RestEntityFactory;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.KException;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.komodo.utils.KLog;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

import com.google.gson.Gson;

/**
 * A Komodo service implementation.
 */
public abstract class KomodoService implements V1Constants {

    public static final String ENCRYPTED_PREFIX = "ENCRYPTED-";

    protected static final String ENCRYPTION_ALGORITHM = "Blowfish";

    protected static final KLog LOGGER = KLog.getLogger();

    protected static final StringNameValidator VALIDATOR = new StringNameValidator();

    protected static final int ALL_AVAILABLE = -1;

    /**
     * VDB properties for DSB
     */
    protected final static String DSB_PROP_OWNER = "dsbOwner"; //$NON-NLS-1$
    protected final static String DSB_PROP_SERVICE_SOURCE = "dsbServiceSource"; //$NON-NLS-1$
    protected final static String DSB_PROP_SOURCE_CONNECTION = "dsbSourceConnection"; //$NON-NLS-1$
    protected final static String DSB_PROP_SOURCE_TRANSLATOR = "dsbSourceTranslator"; //$NON-NLS-1$
    protected final static String DSB_PROP_METADATA_STATUS = "dsbMetadataStatus"; //$NON-NLS-1$
    protected final static String DSB_PROP_METADATA_STATUS_MSG = "dsbMetadataStatusMessage"; //$NON-NLS-1$

    private static final int TIMEOUT = 30;
    private static final TimeUnit UNIT = TimeUnit.SECONDS;

    /**
     * Query parameter keys used by the service methods.
     */
    public interface QueryParamKeys {

        /**
         * A regex expression used when searching. If not present, all objects are returned.
         */
        String PATTERN = "pattern"; //$NON-NLS-1$

        /**
         * The number of objects to return. If not present, all objects are returned.
         */
        String SIZE = "size"; //$NON-NLS-1$

        /**
         * The index of the first object to return. Defaults to zero.
         */
        String START = "start"; //$NON-NLS-1$

        /**
         * The Komodo Type required.
         */
        String KTYPE = "ktype"; //$NON-NLS-1$
    }

    private class ErrorResponse {
        private final String error;

        public ErrorResponse(String error) {
            this.error = error;
        }

        @SuppressWarnings( "unused" )
        public String getError() {
            return error;
        }
    }

    protected static class SecurityPrincipal {

        private final String userName;

        private final Response errorResponse;

        public SecurityPrincipal(String userName, Response errorResponse) {
            this.userName = userName;
            this.errorResponse = errorResponse;
        }

        public String getUserName() {
            return userName;
        }

        public boolean hasErrorResponse() {
            return errorResponse != null;
        }

        public Response getErrorResponse() {
            return errorResponse;
        }
    }

    /**
     * <strong>*** The result ID needs to match the format that Beetle Studio uses. ***</strong>
     *
     * @param vdbName the VDB the view is contained in (cannot be empty)
     * @param viewName the view name (cannot be empty)
     * @return the ID of the editor state of the specified view (never empty)
     */
    public static String getViewEditorStateId( final String vdbName,
                                               final String viewName ) {
        assert( !StringUtils.isBlank( vdbName ) );
        assert( !StringUtils.isBlank( viewName ) );
        return KomodoService.getViewEditorStateIdPrefix( vdbName ) + viewName;
    }

    /**
     * <strong>*** The prefix needs to match the format that Beetle Studio uses. ***</strong>
     *
     * @param vdbName the VDB the view is contained in (cannot be empty)
     * @return the prefix of the view editor state ID (never empty)
     */
    public static String getViewEditorStateIdPrefix( final String vdbName ) {
        assert( !StringUtils.isBlank( vdbName ) );
        return vdbName + '.';
    }

    protected final static SecurityPrincipal SYSTEM_USER = new SecurityPrincipal(RepositoryImpl.SYSTEM_USER, null);

    @Autowired
    protected KEngine kengine;

    protected RestEntityFactory entityFactory = new RestEntityFactory();

    @Context
    protected SecurityContext securityContext;

    /**
     * @param value the value
     * @return the value encoded for json
     */
    public static String protectPrefix(String value) {
        if (value == null)
            return null;

        value = value.replaceAll(COLON, PREFIX_SEPARATOR);
        return value;
    }

    /**
     * @param value the value
     * @return the value decoded from json transit
     */
    public static String unprotectPrefix(String value) {
        if (value == null)
            return null;

        value = value.replaceAll(PREFIX_SEPARATOR, COLON);
        return value;
    }

    protected OAuthCredentials getAuthenticationToken() {
        return AuthHandlingFilter.threadOAuthCredentials.get();
    }

    protected SecurityPrincipal checkSecurityContext(HttpHeaders headers) {
        OAuthCredentials oAuthCredentials = AuthHandlingFilter.threadOAuthCredentials.get();

        //
        // Without oauth proxy running oAuthCredentials is not null but its user is.
        // This will allow the default to the 'komodo' user but the catalog-service resource methods
        // will not be available.
        //
        if (oAuthCredentials != null && oAuthCredentials.getUser() != null) {
            return new SecurityPrincipal(oAuthCredentials.getUser(), null);
        }

		return new SecurityPrincipal(
		                             SystemConstants.REPOSITORY_PERSISTENCE_CONNECTION_USERNAME_DEFAULT,
		                             createErrorResponse(Status.UNAUTHORIZED,
		                             headers.getAcceptableMediaTypes(), RelationalMessages.Error.SECURITY_FAILURE_ERROR));
    }

    /**
     * @param content
     * @return a base64 encoded version of the given content
     */
    protected String encode(byte[] content) {
        if (content == null)
            return null;

        return Base64.getEncoder().encodeToString(content);
    }

    /**
     * @param content
     * @return a decoded version of the given base64-encoded content
     */
    protected byte[] decode(String content) {
        if (content == null)
            return null;

        return Base64.getDecoder().decode(content);
    }

    protected WorkspaceManager getWorkspaceManager(UnitOfWork transaction) throws KException {
    	Repository repo = this.kengine.getDefaultRepository();
        return WorkspaceManager.getInstance(repo, transaction);
    }

    protected Profile getUserProfile(UnitOfWork transaction) throws KException {
        Repository repo = this.kengine.getDefaultRepository();
        KomodoObject userProfileObj = repo.komodoProfile(transaction);
        Profile userProfile = getWorkspaceManager(transaction).resolve(transaction, userProfileObj, Profile.class);
        if (userProfile == null) {
            String msg = RelationalMessages.getString(RelationalMessages.Error.NO_USER_PROFILE, transaction.getUserName());
            throw new KException(msg);
        }

        return userProfile;
    }

    /**
     * @param uow the transaction
     * @param viewEditorStateId the editor state identifier
     * @return <code>true</code> if editor state was deleted; <code>false</code> if not found
     * @throws Exception if an error occurs
     */
    protected boolean removeEditorState(UnitOfWork uow, String viewEditorStateId) throws Exception {
        Profile userProfile = getUserProfile(uow);
        ViewEditorState[] states = userProfile.getViewEditorStates(uow,  viewEditorStateId);

        if (states.length != 0) {
            userProfile.removeViewEditorState(uow, viewEditorStateId);
            return true;
        }

        return false;
    }

    /**
     * @param uow the transaction to use
     * @param editorState the editor state being deleted
     * @return <code>true</code> if successfully deleted
     * @throws Exception if an error occurs
     */
    protected boolean removeEditorState( final UnitOfWork uow,
                                         final ViewEditorState editorState ) throws Exception {
        return removeEditorState( uow, editorState.getName( uow ) );
    }

     /**
     *
     * @param uow the transaction to use
     * @param searchPattern the optional search pattern
     * @return the view editor states (never <code>null</code> but can be empty)
     * @throws Exception if an error occurs
     */
    protected ViewEditorState[] getViewEditorStates( final UnitOfWork uow,
                                                     final String searchPattern ) throws Exception {
        final Profile profile = getUserProfile( uow );
        ViewEditorState[] viewEditorStates = null;

        if ( StringUtils.isBlank( searchPattern ) ) {
            viewEditorStates = profile.getViewEditorStates( uow );
        } else {
            viewEditorStates = profile.getViewEditorStates( uow, searchPattern );
        }

        return viewEditorStates;
    }

    protected String encryptSensitiveData(final HttpHeaders headers, String user, String plainText) {
        String authorization = headers.getHeaderString(HttpHeaders.AUTHORIZATION);
        if (authorization == null)
            return null;

        try {
            SecretKeySpec skeyspec=new SecretKeySpec(authorization.getBytes(),ENCRYPTION_ALGORITHM);
            Cipher cipher = Cipher.getInstance(ENCRYPTION_ALGORITHM);
            cipher.init(Cipher.ENCRYPT_MODE, skeyspec);
            byte[] encrypted = cipher.doFinal(plainText.getBytes());
            return new String(encrypted);
        } catch (Exception ex) {
            KLog.getLogger().error(RelationalMessages.getString(RelationalMessages.Error.ENCRYPT_FAILURE, user), ex);
            return null;
        }
    }

    protected String decryptSensitiveData(final HttpHeaders headers, String user, String encrypted) {
        String authorization = headers.getHeaderString(HttpHeaders.AUTHORIZATION);
        if (authorization == null)
            return null;

        try {
            SecretKeySpec skeyspec=new SecretKeySpec(authorization.getBytes(),ENCRYPTION_ALGORITHM);
            Cipher cipher = Cipher.getInstance(ENCRYPTION_ALGORITHM);
            cipher.init(Cipher.DECRYPT_MODE, skeyspec);
            byte[] plainText = cipher.doFinal(encrypted.getBytes());
            return new String(plainText);
        } catch (Exception ex) {
            KLog.getLogger().error(RelationalMessages.getString(RelationalMessages.Error.DECRYPT_FAILURE, user), ex);
            return null;
        }
    }

    protected Object createErrorResponseEntity(List<MediaType> acceptableMediaTypes, String errorMessage) {
        Object responseEntity = null;

        if (acceptableMediaTypes.contains(MediaType.APPLICATION_JSON_TYPE)) {
            Gson gson = new Gson();
            responseEntity = gson.toJson(new ErrorResponse(errorMessage));
        } else if (acceptableMediaTypes.contains(MediaType.APPLICATION_XML_TYPE)) {
        	return "<error>"+errorMessage+"</error>";
        } else
            responseEntity = errorMessage;

        return responseEntity;
    }

    protected Response createErrorResponse(Status returnCode, List<MediaType> mediaTypes, Throwable ex,
                                           RelationalMessages.Error errorType, Object... errorMsgInputs) {
        String errorMsg = ex.getLocalizedMessage() != null ? ex.getLocalizedMessage() : ex.getClass().getSimpleName();

        //
        // Allow for splitting the message into actual message & stack trace by
        // dividing them with -----
        //
        StringBuffer buf = new StringBuffer(errorMsg).append(NEW_LINE).append("-----").append(NEW_LINE);
        String stackTrace = StringUtils.exceptionToString(ex);
        buf.append(stackTrace).append(NEW_LINE);

        String resultMsg = null;
        if (errorMsgInputs == null || errorMsgInputs.length == 0)
            resultMsg = RelationalMessages.getString(errorType, buf.toString());
        else
            resultMsg = RelationalMessages.getString(errorType, errorMsgInputs, buf.toString());

        return createErrorResponse(returnCode, mediaTypes, resultMsg);
    }

    protected Response createErrorResponse(Status returnCode, List<MediaType> mediaTypes,
                                           RelationalMessages.Error errorType, Object... errorMsgInputs) {
        String resultMsg = null;
        if (errorMsgInputs == null || errorMsgInputs.length == 0)
            resultMsg = RelationalMessages.getString(errorType);
        else
            resultMsg = RelationalMessages.getString(errorType, errorMsgInputs);

        return createErrorResponse(returnCode, mediaTypes, resultMsg);
    }

    protected Response createErrorResponseWithForbidden(List<MediaType> mediaTypes, Throwable ex,
                                                        RelationalMessages.Error errorType, Object... errorMsgInputs) {
        return createErrorResponse(Status.FORBIDDEN, mediaTypes, ex, errorType, errorMsgInputs);
    }

    protected Response createErrorResponseWithForbidden(List<MediaType> mediaTypes,
                                                        RelationalMessages.Error errorType, Object... errorMsgInputs) {
        return createErrorResponse(Status.FORBIDDEN, mediaTypes, errorType, errorMsgInputs);
    }

    protected Response createErrorResponse(Status returnCode, List<MediaType> mediaTypes, String resultMsg) {
        Object responseEntity = createErrorResponseEntity(mediaTypes, resultMsg);

        //
        // Log the error in the komodo log for future reference
        //
        KLog.getLogger().error(Messages.getString(Messages.Error.RESPONSE_ERROR, returnCode, resultMsg));

        return Response.status(returnCode).entity(responseEntity).build();
    }

    protected ResponseBuilder notAcceptableMediaTypesBuilder() {
        List<Variant> variants = VariantListBuilder.newInstance()
                                                                   .mediaTypes(MediaType.APPLICATION_XML_TYPE,
                                                                                       MediaType.APPLICATION_JSON_TYPE)
                                                                   .build();

        return Response.notAcceptable(variants);
    }

    protected boolean isAcceptable(List<MediaType> acceptableTypes, MediaType candidate) {
        if (acceptableTypes == null || acceptableTypes.isEmpty())
            return false;

        if (candidate == null)
            return false;

        for (MediaType acceptableType : acceptableTypes) {
            if (candidate.isCompatible(acceptableType))
                return true;
        }

        return false;
    }

    protected Response commit(List<MediaType> acceptableMediaTypes, final KRestEntity entity) throws Exception {
        ResponseBuilder builder = null;

        if ( entity == RestBasicEntity.NO_CONTENT ) {
            builder = Response.noContent();
        } else if ( entity instanceof ResourceNotFound ) {
            final ResourceNotFound resourceNotFound = ( ResourceNotFound )entity;

            String notFoundMsg = Messages.getString( RESOURCE_NOT_FOUND,
                                                     resourceNotFound.getResourceName(),
                                                     resourceNotFound.getOperationName() );
            Object responseEntity = createErrorResponseEntity(acceptableMediaTypes, notFoundMsg);
            builder = Response.status( Status.NOT_FOUND ).entity(responseEntity);
        } else {

            //
            // Json will always be preferred over XML if both or the wildcard are present in the header
            //
            if (isAcceptable(acceptableMediaTypes, MediaType.APPLICATION_JSON_TYPE))
                builder = Response.ok( KomodoJsonMarshaller.marshall( entity ), MediaType.APPLICATION_JSON );
            else if (isAcceptable(acceptableMediaTypes, MediaType.APPLICATION_XML_TYPE) && entity.supports(MediaType.APPLICATION_XML_TYPE))
                builder = Response.ok( entity.getXml(), MediaType.APPLICATION_XML );
            else {
                builder = notAcceptableMediaTypesBuilder();
            }
        }

        return builder.build();
    }

    protected Response commit( final UnitOfWork transaction, List<MediaType> acceptableMediaTypes,
                               final KRestEntity entity ) throws Exception {
        assert( transaction.getCallback() instanceof SynchronousCallback );
        final int timeout = TIMEOUT;
        final TimeUnit unit = UNIT;

        final SynchronousCallback callback = ( SynchronousCallback )transaction.getCallback();
        transaction.commit();

        if ( !callback.await( timeout, unit ) ) {
            // callback timeout occurred
            String errorMessage = Messages.getString( COMMIT_TIMEOUT, transaction.getName(), timeout, unit );
            Object responseEntity = createErrorResponseEntity(acceptableMediaTypes, errorMessage);
            return Response.status( Status.INTERNAL_SERVER_ERROR )
                           .entity(responseEntity)
                           .build();
        }

        Throwable error = callback.error();

        if ( error != null ) {
            // callback was called because of an error condition
            Object responseEntity = createErrorResponseEntity(acceptableMediaTypes, error.getLocalizedMessage());
            return Response.status( Status.INTERNAL_SERVER_ERROR )
                .entity(responseEntity)
                .build();
        }

        LOGGER.debug( "commit: successfully committed '{0}', rollbackOnly = '{1}'", //$NON-NLS-1$
                      transaction.getName(),
                      transaction.isRollbackOnly() );

        return commit(acceptableMediaTypes, entity);
    }

    protected Response commit(UnitOfWork transaction, List<MediaType> acceptableMediaTypes) throws Exception {
        assert( transaction.getCallback() instanceof SynchronousCallback );
        final int timeout = TIMEOUT;
        final TimeUnit unit = UNIT;

        final SynchronousCallback callback = ( SynchronousCallback )transaction.getCallback();
        transaction.commit();

        if ( ! callback.await( timeout, unit ) ) {
            // callback timeout occurred
            String errorMessage = Messages.getString( COMMIT_TIMEOUT, transaction.getName(), timeout, unit );
            Object responseEntity = createErrorResponseEntity(acceptableMediaTypes, errorMessage);
            return Response.status( Status.INTERNAL_SERVER_ERROR )
                           .type( MediaType.TEXT_PLAIN )
                           .entity(responseEntity)
                           .build();
        }

        Throwable error = transaction.getError();
        if ( error != null ) {
            // callback was called because of an error condition
            Object responseEntity = createErrorResponseEntity(acceptableMediaTypes, error.getLocalizedMessage());
            return Response.status( Status.INTERNAL_SERVER_ERROR )
                            .entity(responseEntity)
                            .build();
        }

        error = callback.error();
        if ( error != null ) {
         // callback was called because of an error condition
            Object responseEntity = createErrorResponseEntity(acceptableMediaTypes, error.getLocalizedMessage());
            return Response.status( Status.INTERNAL_SERVER_ERROR )
                           .entity(responseEntity)
                           .build();
        }

        return Response.ok().build();
    }

    protected Response commit( final UnitOfWork transaction, List<MediaType> acceptableMediaTypes,
                               final List<? extends KRestEntity> entities ) throws Exception {

        commit(transaction, acceptableMediaTypes);

        LOGGER.debug( "commit: successfully committed '{0}', rollbackOnly = '{1}'", //$NON-NLS-1$
                      transaction.getName(),
                      transaction.isRollbackOnly() );
        ResponseBuilder builder = null;

        KRestEntity entity;
        if ( entities.size() == 1 && (entity = entities.iterator().next()) instanceof ResourceNotFound ) {
            final ResourceNotFound resourceNotFound = ( ResourceNotFound )entity;

            String notFoundMessage = Messages.getString( RESOURCE_NOT_FOUND,
                                                         resourceNotFound.getResourceName(),
                                                         resourceNotFound.getOperationName() );
            Object responseEntity = createErrorResponseEntity(acceptableMediaTypes, notFoundMessage);
            builder = Response.status( Status.NOT_FOUND ).entity(responseEntity);
        } else {

            if (isAcceptable(acceptableMediaTypes, MediaType.APPLICATION_JSON_TYPE))
                builder = Response.ok( KomodoJsonMarshaller.marshallArray(entities.toArray(new KRestEntity[0]), true), MediaType.APPLICATION_JSON );
            else {
                builder = notAcceptableMediaTypesBuilder();
            }
        }

        return builder.build();
    }

    /**
     * @param user
     *        the user initiating the transaction
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @param callback the callback to fire when the transaction is committed
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected UnitOfWork createTransaction(final SecurityPrincipal user, final String name,
                                            final boolean rollbackOnly, final UnitOfWorkListener callback) throws KException {
    	Repository repo = this.kengine.getDefaultRepository();
        final UnitOfWork result = repo.createTransaction( user.getUserName(),
                                                               (getClass().getSimpleName() + COLON + name + COLON + System.currentTimeMillis()),
                                                               rollbackOnly, callback, "anonymous");
        LOGGER.debug( "createTransaction:created '{0}', rollbackOnly = '{1}'", result.getName(), result.isRollbackOnly() ); //$NON-NLS-1$
        return result;
    }

    /**
     * @param user
     *        the user initiating the transaction
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected UnitOfWork createTransaction(final SecurityPrincipal user, final String name,
                                            final boolean rollbackOnly ) throws KException {
    	Repository repo = this.kengine.getDefaultRepository();
        final SynchronousCallback callback = new SynchronousCallback();
        final UnitOfWork result = repo.createTransaction(user.getUserName(),
                                                               (getClass().getSimpleName() + COLON + name + COLON + System.currentTimeMillis()),
                                                               rollbackOnly, callback, "anonymous");
        LOGGER.debug( "createTransaction:created '{0}', rollbackOnly = '{1}'", result.getName(), result.isRollbackOnly() ); //$NON-NLS-1$
        return result;
    }

    protected Vdb findVdb(UnitOfWork uow, String vdbName) throws KException {
        if (! getWorkspaceManager(uow).hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
            return null;
        }

        final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        final Vdb vdb = getWorkspaceManager(uow).resolve( uow, kobject, Vdb.class );

        LOGGER.debug( "VDB '{0}' was found", vdbName ); //$NON-NLS-1$
        return vdb;
    }

    protected UnitOfWork systemTx(String description, boolean rollback) throws KException {
        SynchronousCallback callback = new SynchronousCallback();
        return createTransaction(SYSTEM_USER, description, rollback, callback); //$NON-NLS-1$
    }

    protected void awaitCallback(UnitOfWork transaction) throws KException {
        if (transaction == null)
            return;

        UnitOfWorkListener callback = transaction.getCallback();
        if (! (callback instanceof SynchronousCallback)) {
            return;
        }

        SynchronousCallback syncCallback = (SynchronousCallback) callback;
        try {
            if (! syncCallback.await(3, TimeUnit.MINUTES)) {
                throw new CallbackTimeoutException();
            }

            if (transaction.getError() != null) {
                throw new KException(transaction.getError());
            }

            if (syncCallback.hasError()) {
                throw new KException(syncCallback.error());
            }

        } catch (Exception ex) {
            throw new KException(ex);
        }
    }

    protected Dataservice findDataservice(UnitOfWork uow, String dataserviceName) throws KException {
        if (! getWorkspaceManager(uow).hasChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE ) ) {
            return null;
        }

        final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
        final Dataservice dataservice = getWorkspaceManager(uow).resolve( uow, kobject, Dataservice.class );

        LOGGER.debug( "Dataservice '{0}' was found", dataserviceName ); //$NON-NLS-1$
        return dataservice;
    }

    protected Connection findConnection(UnitOfWork uow, String connectionName) throws KException {
        if (! getWorkspaceManager(uow).hasChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE ) ) {
            return null;
        }

        final KomodoObject kobject = getWorkspaceManager(uow).getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
        final Connection connection = getWorkspaceManager(uow).resolve( uow, kobject, Connection.class );

        LOGGER.debug( "Connection '{0}' was found", connectionName ); //$NON-NLS-1$
        return connection;
    }

    protected String uri(String... segments) {
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < segments.length; ++i) {
            buffer.append(segments[i]);
            if (i < (segments.length - 1))
                buffer.append(FORWARD_SLASH);
        }

        return buffer.toString();
    }

    protected Response commitNoVdbFound(UnitOfWork uow, List<MediaType> mediaTypes, String vdbName) throws Exception {
        LOGGER.debug( "VDB '{0}' was not found", vdbName ); //$NON-NLS-1$
        return commit( uow, mediaTypes, new ResourceNotFound( vdbName, Messages.getString( GET_OPERATION_NAME ) ) );
    }

    protected Response commitNoSourceVdbFound(UnitOfWork uow, List<MediaType> mediaTypes) throws Exception {
        LOGGER.debug( "sourceVDB was not found" ); //$NON-NLS-1$
        return commit( uow, mediaTypes, new ResourceNotFound( "sourceVdb", Messages.getString( GET_OPERATION_NAME ) ) );
    }

    protected Response commitNoDataserviceFound(UnitOfWork uow, List<MediaType> mediaTypes, String dataserviceName) throws Exception {
        LOGGER.debug( "Dataservice '{0}' was not found", dataserviceName ); //$NON-NLS-1$
        return commit( uow, mediaTypes, new ResourceNotFound( dataserviceName, Messages.getString( GET_OPERATION_NAME ) ) );
    }

    protected Response commitNoConnectionFound(UnitOfWork uow, List<MediaType> mediaTypes, String connectionName) throws Exception {
        LOGGER.debug( "Connection '{0}' was not found", connectionName ); //$NON-NLS-1$
        return commit( uow, mediaTypes, new ResourceNotFound( connectionName, Messages.getString( GET_OPERATION_NAME ) ) );
    }

    protected Response commitNoTemplateFound(UnitOfWork uow, List<MediaType> mediaTypes, String templateName) throws Exception {
        LOGGER.debug( "Template '{0}' was not found", templateName ); //$NON-NLS-1$
        return commit( uow, mediaTypes, new ResourceNotFound( templateName, Messages.getString( GET_OPERATION_NAME ) ) );
    }

    protected Response commitNoModelFound(UnitOfWork uow, List<MediaType> mediaTypes, String modelName, String vdbName) throws Exception {
        return commit(uow, mediaTypes,
                      new ResourceNotFound(uri(vdbName, MODELS_SEGMENT, modelName),
                                           Messages.getString( GET_OPERATION_NAME)));
    }

}
