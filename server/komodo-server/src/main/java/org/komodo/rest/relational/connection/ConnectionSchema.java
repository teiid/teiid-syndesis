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
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.repository.KomodoType;

/**
    {
        "keng__id": "connection",
        "keng__kType": "Connection",
        "keng__description": "Describes the configuration for a connection",
        "keng__properties": {
            "tko__jndiname": {
                "keng__type": "string",
                "keng__required": "true",
                "keng__repeatable": "false"
            },
            "tko__drivername": {
                "keng__type": "string",
                "keng__required": "true",
                "keng__repeatable": "false"
            },
            "property": {
                "keng__properties": {
                    "value": {
                        "keng__type": "string",
                        "keng__required": "false"
                    },
                    "name": {
                        "keng__type": "string",
                        "keng__required": "false",
                        "keng__values": [
                        ]
                    }
                }
                "keng__type": "string",
                "keng__required": "false",
                "keng__repeatable": "true",
                "keng__limit": "-1"
            }
        }
*/
public class ConnectionSchema implements KRestEntity {

    /**
     * Label for the whole connection schema
     */
    public static final String NAME_LABEL = KomodoType.CONNECTION.name().toLowerCase();

    /**
     * Label for the id
     */
    public static final String ID_LABEL = "keng__id";

    /**
     * Label for the ktype
     */
    public static final String KTYPE_LABEL = "keng__kType";

    /**
     * Label for the description
     */
    public static final String DESCRIPTION_LABEL = "keng__description";

    /**
     * Label for the properties
     */
    public static final String PROPERTIES_LABEL = "keng__properties";

    private String id = KomodoType.CONNECTION.name().toLowerCase();

    private String kType = KomodoType.CONNECTION.getType();

    private String description = "Describes the configuration for a connection";

    private List<ConnectionSchemaProperty> properties = new ArrayList<ConnectionSchemaProperty>(4);

    public ConnectionSchema() {
        properties.add(new ConnectionSchemaProperty(
                                                    ConnectionSchemaProperty.JNDI_NAME_LABEL));

        properties.add(new ConnectionSchemaProperty(
                                                    ConnectionSchemaProperty.DRIVER_NAME_LABEL));

        ConnectionSchemaPairProperty exProperties = new ConnectionSchemaPairProperty(
                                                                         ConnectionSchemaProperty.PROPERTIES_NAME_LABEL);
        properties.add(exProperties);
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getkType() {
        return kType;
    }

    public void setkType(String kType) {
        this.kType = kType;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<ConnectionSchemaProperty> getProperties() {
        return properties;
    }

    public void setProperties(List<ConnectionSchemaProperty> properties) {
        this.properties = properties;
    }
}
