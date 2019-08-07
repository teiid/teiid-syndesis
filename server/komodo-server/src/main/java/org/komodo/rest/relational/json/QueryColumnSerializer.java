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
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.RestQueryColumn;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestQueryColumn}s.
 */
public class QueryColumnSerializer extends TypeAdapter<RestQueryColumn> {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestQueryColumn read( final JsonReader in ) throws IOException {
        final RestQueryColumn queryColumn = new RestQueryColumn();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestQueryColumn.NAME_LABEL:
                    queryColumn.setName(in.nextString());
                    break;
                case RestQueryColumn.LABEL_LABEL:
                    queryColumn.setLabel(in.nextString());
                    break;
                case RestQueryColumn.TYPE_LABEL:
                    queryColumn.setType(in.nextString());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return queryColumn;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out, final RestQueryColumn value ) throws IOException {

        out.beginObject();

        out.name(RestQueryColumn.NAME_LABEL);
        out.value(value.getName());

        out.name(RestQueryColumn.LABEL_LABEL);
        out.value(value.getLabel());

        out.name(RestQueryColumn.TYPE_LABEL);
        out.value(value.getType());

        out.endObject();
    }

}
