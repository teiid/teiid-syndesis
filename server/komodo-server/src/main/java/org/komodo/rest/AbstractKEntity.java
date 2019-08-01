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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.MediaType;

import org.komodo.utils.ArgCheck;

public abstract class AbstractKEntity implements KRestEntity {

    /**
     * A {@link RestBasicEntity} that indicates the resource was not found.
     */
    public static class ResourceNotFound extends RestBasicEntity {

        private final String operationName;
        private final String resourceName;

        /**
         * @param resourceName
         *        the name of the resource that was not found (cannot be empty)
         * @param operationName
         *        the operation that was executed (cannot be empty)
         */
        public ResourceNotFound(final String resourceName, final String operationName) {
            super();
            ArgCheck.isNotEmpty(resourceName, "resourceName"); //$NON-NLS-1$
            ArgCheck.isNotEmpty(operationName, "operationName"); //$NON-NLS-1$

            this.resourceName = resourceName;
            this.operationName = operationName;
        }

        /**
         * @return the operation name (never empty)
         */
        public String getOperationName() {
            return this.operationName;
        }

        /**
         * @return the resource name (never empty)
         */
        public String getResourceName() {
            return this.resourceName;
        }

    }

    protected Map<String, Object> tuples = new LinkedHashMap<>();

    protected List<RestProperty> properties = new ArrayList<>();

    // Transient to ensure its never serialized by Gson
    private transient String xml;

    /**
     * Used for NO_CONTENT and ResourceNotFound
     */
    public AbstractKEntity() {
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    /**
     * @return the tuples
     */
    public Map<String, Object> getTuples() {
        return Collections.unmodifiableMap(this.tuples);
    }

    /**
     * @param key the key
     * @param value the value
     */
    public void addTuple(String key, Object value) {
        tuples.put(key, value);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        AbstractKEntity other = (AbstractKEntity)obj;
        if (this.properties == null) {
            if (other.properties != null)
                return false;
        } else if (!this.properties.equals(other.properties))
            return false;
        if (this.tuples == null) {
            if (other.tuples != null)
                return false;
        } else if (!this.tuples.equals(other.tuples))
            return false;
        return true;
    }

    /**
     * @return the properties (never <code>null</code> but can be empty)
     */
    public final List<RestProperty> getProperties() {
        return this.properties;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.properties == null) ? 0 : this.properties.hashCode());
        result = prime * result + ((this.tuples == null) ? 0 : this.tuples.hashCode());
        return result;
    }

    /**
     * Add a property
     *
     * @param name the property name
     * @param value the property value
     */
    public final void addProperty(String name, Object value) {
        this.properties.add(new RestProperty(name, value));
    }

    /**
     * @param newProperties
     *        the new properties (can be <code>null</code>)
     */
    public final void setProperties(final List<RestProperty> newProperties) {
        this.properties.clear();

        if ((newProperties != null) && !newProperties.isEmpty()) {
            for (RestProperty property : newProperties) {
                ArgCheck.isNotNull(property);
                this.properties.add(property);
            }
        }
    }

    /**
     * @return the xml
     */
    @Override
    public String getXml() {
        return this.xml;
    }

    /**
     * @param xml the xml manifestation of this vdb
     */
    public void setXml(String xml) {
        this.xml = xml;
    }

    /**
     * @param instance new entity to clone into
     */
    public void clone(AbstractKEntity instance) {
        instance.tuples = this.tuples;
        instance.properties = this.properties;
        instance.xml = this.xml;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "AbstractKEntity [tuples=" + this.tuples + ", properties=" + this.properties + "]";
    }
}
