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
