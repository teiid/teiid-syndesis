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

import org.komodo.spi.KClient;

/**
 * Event sent by a repository client
 */
public final class MetadataClientEvent {

    /**
     * Event types describing the repository client
     */
    public enum EventType {
        /**
         * Sent when an {@link KomodoClient} has just started
         */
        STARTED,

        /**
         * Sent just prior to an {@link KomodoClient} shutting down
         */
        SHUTTING_DOWN,
    }

    private final EventType eventType;

    private final KClient source;

    /**
     * @param eventType type of this event
     * @param source the source of this event
     *
     */
    public MetadataClientEvent(EventType eventType, KClient source) {
        this.eventType = eventType;
        this.source = source;
    }

    /**
     * @return the type of this event
     */
    public EventType getType() {
        return eventType;
    }

    /**
     * @return the source
     */
    public KClient getSource() {
        return this.source;
    }

    /**
     * @param source the source of the new event
     *
     * @return event representing the given client has started
     */
    public static MetadataClientEvent createStartedEvent(KClient source) {
        return new MetadataClientEvent(EventType.STARTED, source);
    }

    /**
     * @param source the source of the new event
     *
     * @return event representing the given client has shut down (never <code>null</code>)
     */
    public static MetadataClientEvent createShuttingDownEvent(KClient source) {
        return new MetadataClientEvent(EventType.SHUTTING_DOWN, source);
    }

}
