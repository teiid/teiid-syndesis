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

import org.komodo.spi.storage.StorageConnector.Descriptor;

public class RestStorageTypeDescriptor {

    public static final String NAME_LABEL = "name";

    public static final String DESCRIPTION_LABEL = "description";

    public static final String REQUIRED_LABEL = "required";

    public static final String ENCODED_LABEL = "encoded";

    private String name;

    private String description;

    private boolean required;

    private boolean encoded;

    public RestStorageTypeDescriptor() {
        super();
    }

    public RestStorageTypeDescriptor(Descriptor descriptor) {
        this.name = descriptor.getName();
        this.description = descriptor.getDescription();
        this.required = descriptor.isRequired();
        this.encoded = descriptor.isEncoded();
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

    public boolean isRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
    }

    public boolean isEncoded() {
        return encoded;
    }

    public void setEncoded(boolean encoded) {
        this.encoded = encoded;
    }
}