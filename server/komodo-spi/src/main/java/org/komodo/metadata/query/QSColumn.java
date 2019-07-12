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
package org.komodo.metadata.query;

import org.komodo.metadata.DataTypeService.DataTypeName;

public class QSColumn {

    private final DataTypeName dataType;

    private final String columnName;

    private final String columnLabel;

    public QSColumn(DataTypeName dataType, String columnName, String columnLabel) {
        this.dataType = dataType;
        this.columnName = columnName;
        this.columnLabel = columnLabel;
    }

    public DataTypeName getDataType() {
        return dataType;
    }

    public String getColumnName() {
        return columnName;
    }

    public String getColumnLabel() {
        return columnLabel;
    }
}
