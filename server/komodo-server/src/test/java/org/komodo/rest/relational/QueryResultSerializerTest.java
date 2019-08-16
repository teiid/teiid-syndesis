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

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.komodo.StringConstants;
import org.komodo.metadata.query.QSColumn;
import org.komodo.metadata.query.QSResult;
import org.komodo.metadata.query.QSRow;
import org.komodo.rest.KomodoJsonMarshaller;

public class QueryResultSerializerTest implements StringConstants {

    private static String[][] columnsData = {
        { "Id", "ID", "long" },
        { "Name", "Name", "varchar" },
        { "Code", "Code", "varchar" }
    };

    private static Object[][] rowsData = {
        { 1, "Florida", "FL" },
        { 2, "Washington", "WA" },
        { 3, "Missouri", "MI" },
        { 4, "District of Columbia", "DC" },
        { 5, "Montana", "MO" }
    };

    private static int COLUMN_NAME = 0;

    private static int COLUMN_LABEL = 1;

    private static int COLUMN_TYPE = 2;

    private QSResult queryResult;

    private String JSON;

    @Before
    public void init() throws Exception {
        queryResult = new QSResult();

        for (int i = 0; i < columnsData.length; ++i) {
            String[] columnData = columnsData[i];

            QSColumn column = new QSColumn(columnData[COLUMN_TYPE], columnData[COLUMN_NAME], columnData[COLUMN_LABEL]);

            queryResult.addColumn(column);
        }

        for (int i = 0; i < rowsData.length; ++i) {
            Object[] rowData = rowsData[i];

            QSRow row = new QSRow();
            for (Object o : rowData) {
                row.add(o);
            }

            queryResult.addRow(row);
        }

        JSON = "{\n" +
                "  \"columns\" : [ {\n" +
                "    \"type\" : \"long\",\n" +
                "    \"name\" : \"Id\",\n" +
                "    \"label\" : \"ID\"\n" +
                "  }, {\n" +
                "    \"type\" : \"varchar\",\n" +
                "    \"name\" : \"Name\",\n" +
                "    \"label\" : \"Name\"\n" +
                "  }, {\n" +
                "    \"type\" : \"varchar\",\n" +
                "    \"name\" : \"Code\",\n" +
                "    \"label\" : \"Code\"\n" +
                "  } ],\n" +
                "  \"rows\" : [ {\n" +
                "    \"row\" : [ 1, \"Florida\", \"FL\" ]\n" +
                "  }, {\n" +
                "    \"row\" : [ 2, \"Washington\", \"WA\" ]\n" +
                "  }, {\n" +
                "    \"row\" : [ 3, \"Missouri\", \"MI\" ]\n" +
                "  }, {\n" +
                "    \"row\" : [ 4, \"District of Columbia\", \"DC\" ]\n" +
                "  }, {\n" +
                "    \"row\" : [ 5, \"Montana\", \"MO\" ]\n" +
                "  } ]\n" +
                "}";
    }

    @Test
    public void shouldExportResult() throws Exception {
        String json = KomodoJsonMarshaller.marshall( this.queryResult );
        assertEquals(JSON, json);
    }

    /**
     * Import is not used, and the import will fail as some state is final with no setters
     * @throws Exception
     */
    @Test(expected = RuntimeException.class)
    public void shouldImportResult() throws Exception {
        KomodoJsonMarshaller.unmarshall( JSON, QSResult.class );
    }
}
