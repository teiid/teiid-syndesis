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
package org.komodo.metadata;

import java.util.ArrayList;
import java.util.List;

import org.komodo.StringConstants;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

public class DeployStatus implements StringConstants {

    // Progress Messages
    private List<String> progressMessages = new ArrayList<String>();

    // Error Messages
    private List<String> errorMessages = new ArrayList<String>();

    public boolean ok() {
        return errorMessages.isEmpty();
    }

    /**
     * Add an error message based on the supplied exception.
     * @param exception the exception
     */
    public void addErrorMessage(Throwable exception) {
        ArgCheck.isNotNull(exception, "error"); //$NON-NLS-1$

        while (exception.getCause() != null) {
            //
            // Zero down to the root cause of the exception
            //
            exception = exception.getCause();
        }

        String message = exception.getLocalizedMessage();
        if (message == null || message.isEmpty())
            message = StringUtils.exceptionToString(exception);

        errorMessages.add(message);
    }

    /**
     * Add an error message
     * @param message the error message
     */
    public void addErrorMessage(String message) {
        ArgCheck.isNotNull(message, "error message"); //$NON-NLS-1$

        if (errorMessages == null) {
            errorMessages = new ArrayList<String>();
        }
        errorMessages.add(message);
    }

    /**
     * Get the error messages
     * @return messages
     */
    public List<String> getErrorMessages() {
        if (errorMessages == null) {
            errorMessages = new ArrayList<String>();
        }

        return errorMessages;
    }

    /**
     * Get the error messages
     * @return error messages
     */
    public String errorMessagesToString() {
        if (errorMessages == null)
            return EMPTY_STRING;

        StringBuffer errorMsgs = new StringBuffer();
        for (String errorMsg : errorMessages) {
            errorMsgs.append(errorMsg);
            errorMsgs.append(NEW_LINE);
        }

        return errorMsgs.toString();
    }

    /**
     * Add an progress message
     * @param message the progress message
     */
    public void addProgressMessage(String message) {
        ArgCheck.isNotNull(message, "progress message"); //$NON-NLS-1$

        if (progressMessages == null) {
            progressMessages = new ArrayList<String>();
        }

        progressMessages.add(message);
    }

    public List<String> getProgressMessages() {
        if (progressMessages == null) {
            progressMessages = new ArrayList<String>();
        }

        return progressMessages;
    }
}
