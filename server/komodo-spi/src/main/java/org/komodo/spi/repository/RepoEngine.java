/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.spi.repository;

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.query.KQueryManager;

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
