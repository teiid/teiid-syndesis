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
package org.komodo.core.repository;

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KObjectFactory;
import org.komodo.spi.repository.KPropertyFactory;

public interface RepoEngine extends StringConstants {

    /**
     * Request types to send to this engine thread
     */
    enum RequestType {

        /**
         * Request to save the specified session.
         *
         * @see JcrEngineThread.SessionRequest
         */
        COMMIT_SESSION,

        /**
         * Request to create a session.
         */
        CREATE_SESSION,

        /**
         * Request to rollback the specified session.
         *
         * @see JcrEngineThread.SessionRequest
         */
        ROLLBACK_SESSION,

        /**
         * Request starting of the engine
         */
        START,

        /**
         * Request stopping of the engine
         */
        STOP,

        /**
         * Request to clear the engine repository
         */
        CLEAR;

        public static boolean isSessionRequest(final RequestType requestType) {
            return ((requestType == CREATE_SESSION) || (requestType == COMMIT_SESSION) || (requestType == ROLLBACK_SESSION));
        }

    }

    /**
     * Callback interface that can be implemented by third-parties and incorporated into requests to the engine thread.
     */
    interface RequestCallback {

        /**
         * @param error
         *        the error that occurred (never <code>null</code>)
         */
        void errorOccurred(final Throwable error);

        /**
         * Respond to the request being successfully completed.
         *
         * @param results
         *        the results (can be <code>null</code>)
         */
        void respond(final Object results);

    }

    /**
     * Request crafted by wishing to be notified of a request outcome.
     */
    class Request {

        private RequestType requestType;

        private RequestCallback callback;

        /**
         * @param requestType
         *        type of request (cannot be <code>null</code>)
         * @param callback
         *        callback for execution after change of engine state (can be <code>null</code>)
         */
        public Request(RequestType requestType, RequestCallback callback) {
            this.setRequestType(requestType);
            this.callback = callback;
        }

        /**
         * @return the requestType
         */
        public RequestType getRequestType() {
            return this.requestType;
        }

        public void setRequestType(RequestType requestType) {
            this.requestType = requestType;
        }

        /**
         * @return the callback
         */
        public RequestCallback getCallback() {
            return this.callback;
        }
    }

    /**
     * @return the factory for handling nodes in this engine
     */
    KObjectFactory getNodeFactory();

    /**
     * @return the factory for handling node properties
     */
    KPropertyFactory getPropertyFactory();

    /**
     * @return the manager for handling queries
     */
    KQueryManager getQueryManager();

    /**
     * Pass a request to the engine
     *
     * @param request
     *        the request made to the engine
     */
    void accept(Request request);

    /**
     * @return true is the engine is active or false otherwise
     */
    boolean isAlive();

    /**
     * @return any error that may have occurred when the run method threw an error
     */
    Exception getError();

    /**
     * Start the engine
     */
    void start();

    /**
     * @return true if the engine is running, otherwise false
     */
    boolean isRunning();

}
