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

import org.komodo.utils.ArgCheck;

/**
 * Indicates the objects has a JSON representation.
 */
public class RestBasicEntity extends AbstractKEntity {

    /**
     * A {@link RestBasicEntity} that indicates the resource was not found.
     */
    public static class ResourceNotFound extends RestBasicEntity {

        private final String resourceName;

        /**
         * @param resourceName
         *        the name of the resource that was not found (cannot be empty)
         */
        public ResourceNotFound(final String resourceName) {
            super();
            ArgCheck.isNotEmpty(resourceName, "resourceName"); //$NON-NLS-1$

            this.resourceName = resourceName;
        }

        /**
         * @return the resource name (never empty)
         */
        public String getResourceName() {
            return this.resourceName;
        }

    }

    /**
     * Indicates no content is being returned.
     */
    public static final RestBasicEntity NO_CONTENT = new RestBasicEntity() {
        // nothing to do
    };

    /**
     * Used for NO_CONTENT and ResourceNotFound
     */
    public RestBasicEntity() {
        super();
    }

    /**
     * @return the id
     */
    public String getId() {
        Object id = tuples.get(ID);
        return id != null ? id.toString() : null;
    }

    /**
     * @param id the id to set
     */
    public void setId(String id) {
        tuples.put(ID, id);
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestBasicEntity [tuples=" + this.tuples + "]";
    }
}
