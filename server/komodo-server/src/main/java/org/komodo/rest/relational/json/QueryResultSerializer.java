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
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.RestQueryColumn;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestQueryResult}.
 */
public class QueryResultSerializer extends TypeAdapter<RestQueryResult> {

    @Override
    public RestQueryResult read(JsonReader in) throws IOException {
        final RestQueryResult queryResult = new RestQueryResult();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestQueryResult.COLUMNS_LABEL:
                    RestQueryColumn[] columns = BUILDER.fromJson(in, RestQueryColumn[].class);
                    queryResult.setColumns(columns);
                    break;
                case RestQueryResult.ROWS_LABEL:
                    RestQueryRow[] rows = BUILDER.fromJson(in, RestQueryRow[].class);
                    queryResult.setRows(rows);
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return queryResult;
    }

    @Override
    public void write(JsonWriter out, RestQueryResult value) throws IOException {
        out.beginObject();

        if (value.getColumns().length > 0) {
            out.name(RestQueryResult.COLUMNS_LABEL);
            BUILDER.toJson(value.getColumns(), RestQueryColumn[].class, out);
        }

        if (value.getRows().length > 0) {
            out.name(RestQueryResult.ROWS_LABEL);
            BUILDER.toJson(value.getRows(), RestQueryRow[].class, out);
        }

        out.endObject();
    }
}
