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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.query.QSColumn;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.query.QSRow;

public class RestQueryResult implements KRestEntity {

    /**
     * Label for columns
     */
    public static final String COLUMNS_LABEL = "columns";

    /**
     * Label for rows
     */
    public static final String ROWS_LABEL = "rows";

    private List<RestQueryColumn> columns = new ArrayList<>();

    private List<RestQueryRow> rows = new ArrayList<>();

    public RestQueryResult() {
        super();
    }

    public RestQueryResult(QSResult result) {
        if (result == null)
            return;

        for (QSColumn column : result.getColumns()) {
            columns.add(new RestQueryColumn(column));
        }

        for (QSRow row : result.getRows()) {
            rows.add(new RestQueryRow(row));
        }
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    public RestQueryRow[] getRows() {
        return rows.toArray(new RestQueryRow[0]);
    }

    public void setRows(RestQueryRow[] rows) {
        this.rows = Arrays.asList(rows);
    }

    public RestQueryColumn[] getColumns() {
        return columns.toArray(new RestQueryColumn[0]);
    }

    public void setColumns(RestQueryColumn[] columns) {
        if (columns == null || columns.length == 0)
            this.columns = Collections.emptyList();

        this.columns = new ArrayList<>();
        for (RestQueryColumn column : columns) {
            this.columns.add(column);
        }
    }
}
