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
package org.komodo.rest.relational.response;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.storage.StorageConnector.Descriptor;

public class RestStorageType implements KRestEntity {

    public static final String NAME_LABEL = "name";

    public static final String DESCRIPTORS_LABEL = "descriptors";

    public static final String DESCRIPTION_LABEL = "description";

    private String name;

    private List<RestStorageTypeDescriptor> descriptors;

    private String description;

    /**
     * Constructor for use when deserializing
     */
    public RestStorageType() {
        super();
        this.descriptors = Collections.emptyList();
    }

    public RestStorageType(String name, String description, Collection<Descriptor> descriptors) {
        this.name = name;
        this.description = description;

        if (descriptors == null || descriptors.isEmpty())
            this.descriptors = Collections.emptyList();
        else {
            this.descriptors = new ArrayList<>();
            for (Descriptor descriptor : descriptors) {
                this.descriptors.add(new RestStorageTypeDescriptor(descriptor));
            }
        }
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType)
        || MediaType.APPLICATION_XML_TYPE.equals(mediaType);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<RestStorageTypeDescriptor> getDescriptors() {
        return descriptors;
    }

    public void setDescriptors(RestStorageTypeDescriptor[] descriptors) {
        if (descriptors == null || descriptors.length == 0)
            this.descriptors = Collections.emptyList();

        this.descriptors = new ArrayList<>();
        for (RestStorageTypeDescriptor descriptor : descriptors) {
            this.descriptors.add(descriptor);
        }
    }
}
