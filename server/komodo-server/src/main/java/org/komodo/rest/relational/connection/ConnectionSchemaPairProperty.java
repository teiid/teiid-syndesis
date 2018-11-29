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

import java.util.ArrayList;
import java.util.List;

/**
    "property": {
        "keng__properties": {
            "value": {
                "keng__type": "string",
                "keng__required": "false"
            },
            "name": {
                "keng__type": "string",
                "keng__required": "false"
            }
        }
        "keng__type": "string",
        "keng__required": "false",
        "keng__repeatable": "true",
        "keng__limit": "-1"
    }
 */
public class ConnectionSchemaPairProperty extends ConnectionSchemaProperty {

    public static final String PROPERTY_NAME_LABEL = "name";

    public static final String PROPERTY_VALUE_LABEL = "value";

    private List<ConnectionSchemaProperty> properties = new ArrayList<ConnectionSchemaProperty>();

    public ConnectionSchemaPairProperty(String name) {
        super(name, "string", false, true, -1);

        properties.add(new ConnectionSchemaProperty(PROPERTY_NAME_LABEL));
        properties.add(new ConnectionSchemaProperty(PROPERTY_VALUE_LABEL));
    }

    public List<ConnectionSchemaProperty> getProperties() {
        return properties;
    }

    public void setProperties(List<ConnectionSchemaProperty> properties) {
        this.properties = properties;
    }
}
