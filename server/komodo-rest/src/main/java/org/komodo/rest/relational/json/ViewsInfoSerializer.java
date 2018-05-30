/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
