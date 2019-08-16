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

import static org.komodo.StringConstants.NEW_LINE;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import org.komodo.rest.KomodoService.ErrorResponse;
import org.komodo.rest.datavirtualization.RelationalMessages;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.springframework.stereotype.Component;

/**
 * Maps {@link Throwable errors} to {@link Response responses}.
 */
@Provider
@Component
public class KomodoExceptionMapper implements ExceptionMapper< Throwable > {

    /**
     * {@inheritDoc}
     *
     * @see javax.ws.rs.ext.ExceptionMapper#toResponse(java.lang.Throwable)
     */
    @Override
    public Response toResponse( final Throwable t ) {
        if (t instanceof WebApplicationException) {
            return ((WebApplicationException)t).getResponse();
        }

        String errorMsg = t.getLocalizedMessage() != null ? t.getLocalizedMessage() : t.getClass().getSimpleName();

        //
        // Allow for splitting the message into actual message & stack trace by
        // dividing them with -----
        //
        StringBuffer buf = new StringBuffer(errorMsg).append(NEW_LINE).append("-----").append(NEW_LINE);
        String stackTrace = StringUtils.exceptionToString(t);
        buf.append(stackTrace).append(NEW_LINE);

        String resultMsg = RelationalMessages.getString(RelationalMessages.Error.INTERNAL_ERROR, buf.toString());

        KLog.getLogger().error(errorMsg, t);

        ErrorResponse error = new ErrorResponse(resultMsg, Status.INTERNAL_SERVER_ERROR);

        return KomodoService.toResponse(error);
    }

}
