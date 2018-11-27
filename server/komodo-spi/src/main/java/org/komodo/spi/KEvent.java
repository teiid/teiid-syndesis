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

import org.komodo.spi.repository.Repository;

/**
 * A Komodo event.
 *
 * @param <T> class of the source object
 */
public class KEvent<T> {

    /**
     * Events types
     */
    public enum Type {
        /**
         * repository added
         */
        REPOSITORY_ADDED,

        /**
         * repository removed
         */
        REPOSITORY_REMOVED,

        /**
         * repository started
         */
        REPOSITORY_STARTED,

        /**
         * repository stopped
         */
        REPOSITORY_STOPPED,

        /**
         * repository cleared
         */
        REPOSITORY_CLEARED,

        /**
         * Metadata server started
         */
        METADATA_SERVER_STARTED,

        /**
         * Metadata server stopped
         */
        METADATA_SERVER_STOPPED,

        /**
         * engine started
         */
        ENGINE_STARTED,

        /**
         * engine shutdown
         */
        ENGINE_SHUTDOWN
    }

    private final T source;
    private final Type type;

    /**
     * Constructor
     * @param source event source
     * @param type event type
     */
    public KEvent(T source, Type type) {
        this.source = source;
        this.type = type;
    }

    /**
     * @return the event source (never <code>null</code>)
     */
    public T getSource() {
        return source;
    }

    /**
     * @return the event type (never <code>null</code>)
     */
    public Type getType() {
        return type;
    }

    @Override
    public String toString() {
        return "KEvent [source=" + source + ", type=" + type + "]";
    }

    /**
     * @param repository added
     * @return repository added event
     */
    public static KEvent<Repository> repositoryAddedEvent(Repository repository) {
        return new KEvent<Repository>(repository, Type.REPOSITORY_ADDED);
    }

    /**
     * @param repository removed
     * @return repository removed event
     */
    public static KEvent<Repository> repositoryRemovedEvent(Repository repository) {
        return new KEvent<Repository>(repository, Type.REPOSITORY_REMOVED);
    }
}
