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
package org.komodo.rest.relational.connection;

import org.komodo.rest.KomodoService;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;

public class ConnectionSchemaProperty {

    /**
     * Label used to describe jndi name
     */
    public static final String JNDI_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.JNDI_NAME);

    /**
     * Label used to describe driver name
     */
    public static final String DRIVER_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.DRIVER_NAME);

    public static final String TYPE_LABEL = "keng__type";

    public static final String REQUIRED_LABEL = "keng__required";

    public static final String REPEATABLE_LABEL = "keng__repeatable";

    public static final String LIMIT_LABEL = "keng__limit";

    public static final String PROPERTIES_NAME_LABEL = "property";

    private String name;

    private String type = "string";

    private boolean required = true;

    private boolean repeatable = false;

    private int limit = 1;

    public ConnectionSchemaProperty(String name, String type, boolean required, boolean repeatable, int limit) {
        setName(name);
        setType(type);
        setRequired(required);
        setRepeatable(repeatable);
        setLimit(limit);
    }

    public ConnectionSchemaProperty(String name) {
        setName(name);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public boolean isRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
    }

    public boolean isRepeatable() {
        return repeatable;
    }

    public void setRepeatable(boolean repeatable) {
        this.repeatable = repeatable;
    }

    public int getLimit() {
        return limit;
    }

    public void setLimit(int limit) {
        this.limit = limit;
    }
}