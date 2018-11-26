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

import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public class RestProperty implements StringConstants {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = "name"; //$NON-NLS-1$

    /**
     * Label used to describe value
     */
    public static final String VALUE_LABEL = "value"; //$NON-NLS-1$

    private final String name;

    private final Object value;

    /**
     * @param name the name
     * @param value the value
     *
     */
    public RestProperty(String name, Object value) {
        this.name = name;
        this.value = value;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the value
     */
    public Object getValue() {
        return value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        RestProperty other = (RestProperty)obj;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        if (this.value == null) {
            if (other.value != null)
                return false;
        } else
            if (!this.value.equals(other.value))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "KomodoRestProperty [name=" + this.name + ", value=" + this.value + "]";
    }
}
