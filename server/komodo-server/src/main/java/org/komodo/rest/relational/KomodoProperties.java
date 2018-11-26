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
package org.komodo.rest.relational;

import java.util.HashMap;

/**
 * Convenience properties object that provides key as a {@link String}
 */
public class KomodoProperties extends HashMap<String, Object> {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * @param key the property key
     * @param value the property value
     */
    public void addProperty(String key, Object value) {
        this.put(key, value);
    }

    /**
     * @param key the property key
     * @return the value for the given property key
     */
    public Object getProperty(String key) {
        return this.get(key);
    }

    /**
     * @param key the property key
     * @param defaultValue the default value if no property has been set
     * @return the value of the property or the default value
     */
    @SuppressWarnings( "unchecked" )
    public <T> T getProperty(String key, T defaultValue) {
        if (! this.containsKey(key))
            return defaultValue;

        return (T) this.get(key);
    }
}
