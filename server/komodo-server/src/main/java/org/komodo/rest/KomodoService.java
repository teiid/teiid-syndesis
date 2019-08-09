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

import static org.komodo.rest.Messages.Error.RESOURCE_NOT_FOUND;

import java.util.concurrent.Callable;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.SecurityContext;

import org.komodo.KEngine;
import org.komodo.KException;
import org.komodo.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.utils.KLog;
import org.komodo.utils.StringNameValidator;
import org.komodo.utils.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * A Komodo service implementation.
 */
public abstract class KomodoService extends AbstractTransactionService implements V1Constants {
	
    /**
	 * System user for transactions to be executed internally
	 */
	public static final String SYSTEM_USER_NAME = "SYSTEM";

	public static final String ENCRYPTED_PREFIX = "ENCRYPTED-";

    protected static final KLog LOGGER = KLog.getLogger();

    protected static final StringNameValidator VALIDATOR = new StringNameValidator();

    protected static final int ALL_AVAILABLE = -1;

    /**
     * Query parameter keys used by the service methods.
     */
    public interface QueryParamKeys {

        /**
         * The number of objects to return. If not present, all objects are returned.
         */
        String SIZE = "size"; //$NON-NLS-1$

        /**
         * The index of the first object to return. Defaults to zero.
         */
        String START = "start"; //$NON-NLS-1$

		String VIRTUALIZATION = "virtualization";
    }

    @JsonSerialize(as = ErrorResponse.class)
    static class ErrorResponse {
        private final String error;
        @JsonIgnore
        private Status status;

        public ErrorResponse(String error, Status status) {
            this.error = error;
            this.status = status;
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
    
    public final static SecurityPrincipal SYSTEM_USER = new SecurityPrincipal(SYSTEM_USER_NAME, null);

    @Autowired
    protected KEngine kengine;
    
    @Autowired
    protected CredentialsProvider credentialsProvider;

    @Context
    protected SecurityContext securityContext;

    protected OAuthCredentials getAuthenticationToken() {
        return credentialsProvider.getCredentials();
    }

    protected SecurityPrincipal checkSecurityContext(HttpHeaders headers) {
        OAuthCredentials oAuthCredentials = getAuthenticationToken();

        //
        // Without oauth proxy running oAuthCredentials is not null but its user is.
        // This will allow the default to the 'komodo' user but the catalog-service resource methods
        // will not be available.
        //
        if (oAuthCredentials != null && oAuthCredentials.getUser() != null) {
            return new SecurityPrincipal(oAuthCredentials.getUser(), null);
        }

		return new SecurityPrincipal(
		                             "komodo",
		                             createErrorResponse(Status.UNAUTHORIZED,
		                             RelationalMessages.Error.SECURITY_FAILURE_ERROR));
    }

    protected WorkspaceManager getWorkspaceManager() throws KException {
    	return this.kengine.getWorkspaceManager();
    }

    public static Response createErrorResponse(Status returnCode, RelationalMessages.Error errorType,
                                           Object... errorMsgInputs) {
        String resultMsg = null;
        if (errorMsgInputs == null || errorMsgInputs.length == 0)
            resultMsg = RelationalMessages.getString(errorType);
        else
            resultMsg = RelationalMessages.getString(errorType, errorMsgInputs);

        return createErrorResponse(returnCode, resultMsg);
    }
    
    public static Response createErrorResponse(Throwable ex, RelationalMessages.Error errorType,
            Object... errorMsgInputs) {
		if (ex != null) {
			LOGGER.error(errorType.toString(), ex);
		}
		
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

        return createErrorResponse(Status.INTERNAL_SERVER_ERROR, resultMsg);
	}

    public static Response createErrorResponseWithForbidden(RelationalMessages.Error errorType,
                                                        Object... errorMsgInputs) {
        return createErrorResponse(Status.FORBIDDEN, errorType, errorMsgInputs);
    }

    public static Response createErrorResponse(Status returnCode, String resultMsg) {
        //
        // Log the error in the komodo log for future reference
        //
        KLog.getLogger().error(Messages.getString(Messages.Error.RESPONSE_ERROR, returnCode, resultMsg));

    	ErrorResponse error = new ErrorResponse(resultMsg, returnCode);

        return toResponse(error);
    }

    public static Response toResponse(Object entity) {
        Status status = Status.OK;
        
        if (entity == null) {
            return Response.noContent().build();
        }
    	
        if ( entity instanceof ErrorResponse ) {
        	status = ((ErrorResponse)entity).status;
        }

        if ( entity instanceof ResourceNotFound ) {
            final ResourceNotFound resourceNotFound = ( ResourceNotFound )entity;

            String notFoundMsg = Messages.getString( RESOURCE_NOT_FOUND,
                                                     resourceNotFound.getResourceName());
            entity = new ErrorResponse(notFoundMsg, Status.NOT_FOUND);
            status = Status.NOT_FOUND;
        }
        
		return Response.status(status)
				.entity(KomodoJsonMarshaller.marshall(entity))
				.type(MediaType.APPLICATION_JSON)
				.build();
    }
    
    protected <T> T runInTransaction(SecurityPrincipal user, String txnName, boolean rollbackOnly, Callable<T> callable) throws Exception {
    	return runInTransaction(user.getUserName(), txnName, rollbackOnly, callable);
    }
    
}
