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

import org.komodo.rest.Messages;
import org.komodo.rest.relational.request.KomodoViewsInfo;

import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoViewsInfo}s.
 */
public final class ViewsInfoSerializer extends TypeAdapter< KomodoViewsInfo > {

    private static final Type STRING_LIST_TYPE = new TypeToken< List< String > >() {/* nothing to do */}.getType();

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoViewsInfo read( final JsonReader in ) throws IOException {
        final KomodoViewsInfo viewsInfo = new KomodoViewsInfo();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoViewsInfo.VIEW_NAMES_LABEL:
                    List<String> viewNames = BUILDER.fromJson(in, List.class);
                    viewsInfo.setViewNames(viewNames);
                    break;
                case KomodoViewsInfo.TABLE_PATHS_LABEL:
                    List<String> tablePaths = BUILDER.fromJson(in, List.class);
                    viewsInfo.setTablePaths(tablePaths);
                    break;
                case KomodoViewsInfo.MODEL_SOURCE_PATHS_LABEL:
                    List<String> modelSrcPaths = BUILDER.fromJson(in, List.class);
                    viewsInfo.setModelSourcePaths(modelSrcPaths);
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return viewsInfo;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoViewsInfo value ) throws IOException {

        out.beginObject();
        
        if (! value.getViewNames().isEmpty()) {
            out.name(KomodoViewsInfo.VIEW_NAMES_LABEL);
            BUILDER.toJson(value.getViewNames(), STRING_LIST_TYPE, out);
        }

        if (! value.getTablePaths().isEmpty()) {
            out.name(KomodoViewsInfo.TABLE_PATHS_LABEL);
            BUILDER.toJson(value.getTablePaths(), STRING_LIST_TYPE, out);
        }

        if (! value.getModelSourcePaths().isEmpty()) {
            out.name(KomodoViewsInfo.MODEL_SOURCE_PATHS_LABEL);
            BUILDER.toJson(value.getModelSourcePaths(), STRING_LIST_TYPE, out);
        }

        out.endObject();
    }

}
