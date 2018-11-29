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
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;

import org.komodo.rest.Messages;
import org.komodo.rest.relational.request.KomodoDataserviceSingleSourceAttributes;

import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoDataserviceSingleSourceAttribute}s.
 */
public final class DataserviceSingleSourceAttributesSerializer extends TypeAdapter< KomodoDataserviceSingleSourceAttributes > {

    private static final Type STRING_LIST_TYPE = new TypeToken< List< String > >() {/* nothing to do */}.getType();
    private static final Type MAP_LIST_TYPE = new TypeToken< Map<String,List<String>> >() {/* nothing to do */}.getType();

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoDataserviceSingleSourceAttributes read( final JsonReader in ) throws IOException {
        final KomodoDataserviceSingleSourceAttributes ssrcAttrs = new KomodoDataserviceSingleSourceAttributes();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoDataserviceSingleSourceAttributes.DATASERVICE_NAME_LABEL:
                	ssrcAttrs.setDataserviceName(in.nextString());
                    break;
                case KomodoDataserviceSingleSourceAttributes.DATASERVICE_TABLE_PATHS_LABEL:
                    List<String> tablePaths = BUILDER.fromJson(in, List.class);
                	ssrcAttrs.setTablePaths(tablePaths);
                    break;
                case KomodoDataserviceSingleSourceAttributes.DATASERVICE_MODEL_SOURCE_PATH_LABEL:
                	ssrcAttrs.setModelSourcePath(in.nextString());
                    break;
                case KomodoDataserviceSingleSourceAttributes.DATASERVICE_COLUMN_NAMES_LABEL:
                    Map<String, List<String>> colNames = BUILDER.fromJson(in, Map.class);
                    ssrcAttrs.setColumnNames(colNames);
                    break;
                case KomodoDataserviceSingleSourceAttributes.DATASERVICE_VIEW_DDL_LABEL:
                	ssrcAttrs.setViewDdl(in.nextString());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return ssrcAttrs;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoDataserviceSingleSourceAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoDataserviceSingleSourceAttributes.DATASERVICE_NAME_LABEL);
        out.value(value.getDataserviceName());

        if (! value.getTablePaths().isEmpty()) {
            out.name(KomodoDataserviceSingleSourceAttributes.DATASERVICE_TABLE_PATHS_LABEL);
            BUILDER.toJson(value.getTablePaths(), STRING_LIST_TYPE, out);
        }

        out.name(KomodoDataserviceSingleSourceAttributes.DATASERVICE_MODEL_SOURCE_PATH_LABEL);
        out.value(value.getModelSourcePath());
        
        if (! value.getColumnNames().isEmpty()) {
            out.name(KomodoDataserviceSingleSourceAttributes.DATASERVICE_COLUMN_NAMES_LABEL);
            BUILDER.toJson(value.getColumnNames(), MAP_LIST_TYPE, out);
        }

        out.name(KomodoDataserviceSingleSourceAttributes.DATASERVICE_VIEW_DDL_LABEL);
        out.value(value.getViewDdl());

        out.endObject();
    }

}
