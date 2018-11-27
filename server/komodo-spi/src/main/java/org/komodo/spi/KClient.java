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
package org.komodo.spi;

import org.komodo.spi.metadata.MetadataObserver;
import org.komodo.spi.repository.RepositoryObserver;

/**
 *
 */
public interface KClient extends RepositoryObserver, MetadataObserver {

    /**
     * The client state.
     */
    public static enum State {

        /**
         * Client has been successfully started.
         */
        STARTED,

        /**
         * Client is shutdown.
         */
        SHUTDOWN,

        /**
         * There was an error starting or shutting down the client.
         */
        ERROR

    }

    /**
     * @return the engine state (never <code>null</code>)
     */
    public State getState();
}
