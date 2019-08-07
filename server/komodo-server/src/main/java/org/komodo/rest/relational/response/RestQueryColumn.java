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

import org.komodo.metadata.query.QSColumn;

public class RestQueryColumn {

    /**
     * Label for name
     */
    public static final String NAME_LABEL = "name";

    /**
     * Label for label
     */
    public static final String LABEL_LABEL = "label";

    /**
     * Label for type
     */
    public static final String TYPE_LABEL = "type";

    private String name;

    private String label;

    private String type;

    /**
     * Constructor for use when deserializing
     */
    public RestQueryColumn() {
        super();
    }

    public RestQueryColumn(QSColumn column) {
        this.name = column.getColumnName();
        this.label = column.getColumnLabel();
        this.type = column.getDataType();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
