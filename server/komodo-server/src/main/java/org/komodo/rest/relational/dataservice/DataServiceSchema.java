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
package org.komodo.rest.relational.dataservice;

import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.core.KomodoLexicon;
import org.komodo.rest.KRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.spi.repository.KomodoType;

public class DataServiceSchema implements KRestEntity {

    /**
     * Label for the whole data source schema
     */
    public static final String NAME_LABEL = KomodoType.DATASERVICE.name().toLowerCase();

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
    public static final String DESCRIPTION_LABEL = KomodoService.protectPrefix(KomodoLexicon.LibraryComponent.DESCRIPTION);

    /**
     * Label for the properties
     */
    public static final String PROPERTIES_LABEL = "keng__properties";

    private String id = KomodoType.DATASERVICE.name().toLowerCase();

    private String kType = KomodoType.DATASERVICE.getType();

    private String description = "A description for the dataservice";

    private List<DataServiceSchemaProperty> properties = new ArrayList<DataServiceSchemaProperty>(4);

    public DataServiceSchema() {
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

    public List<DataServiceSchemaProperty> getProperties() {
        return properties;
    }

    public void setProperties(List<DataServiceSchemaProperty> properties) {
        this.properties = properties;
    }
}
