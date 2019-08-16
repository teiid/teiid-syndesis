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

import static org.komodo.rest.Messages.Error.*;

import javax.ws.rs.ClientErrorException;
import javax.ws.rs.ForbiddenException;
import javax.ws.rs.NotFoundException;
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
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.utils.KLog;
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
    public static class ErrorResponse {
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

    @Autowired
    protected KEngine kengine;

    @Autowired
    protected CredentialsProvider credentialsProvider;

    @Context
    protected SecurityContext securityContext;

    protected OAuthCredentials getAuthenticationToken() {
        return credentialsProvider.getCredentials();
    }

    protected String checkSecurityContext(HttpHeaders headers) {
        OAuthCredentials oAuthCredentials = getAuthenticationToken();

        //
        // Without oauth proxy running oAuthCredentials is not null but its user is.
        // This will allow the default to the 'komodo' user but the catalog-service resource methods
        // will not be available.
        //
        if (oAuthCredentials == null || oAuthCredentials.getUser() == null) {
            error(Status.UNAUTHORIZED,
                    RelationalMessages.Error.SECURITY_FAILURE_ERROR);
        }

        return oAuthCredentials.getUser();
    }

    protected WorkspaceManager getWorkspaceManager() throws KException {
        return this.kengine.getWorkspaceManager();
    }

    public static void notFound(String resourceName) {
        String message = Messages.getString( RESOURCE_NOT_FOUND,
                resourceName);
        throw new NotFoundException(message, toResponse(new ErrorResponse(message, Status.NOT_FOUND)));
    }

    public static void error(Status returnCode, RelationalMessages.Error errorType,
                                           Object... errorMsgInputs) {
        String resultMsg = RelationalMessages.getString(errorType, errorMsgInputs);

        throw new ClientErrorException(resultMsg, createErrorResponse(returnCode, resultMsg));
    }

    public static void forbidden(RelationalMessages.Error errorType,
                                                        Object... errorMsgInputs) {
        String resultMsg = RelationalMessages.getString(errorType, errorMsgInputs);

        throw new ForbiddenException(resultMsg, createErrorResponse(Status.FORBIDDEN, resultMsg));
    }

    public static Response createErrorResponse(Status returnCode, String resultMsg) {
        LOGGER.debug(Messages.getString(Messages.Error.RESPONSE_ERROR, returnCode, resultMsg));

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

        return Response.status(status)
                .entity(KomodoJsonMarshaller.marshall(entity))
                .type(MediaType.APPLICATION_JSON)
                .build();
    }

}
