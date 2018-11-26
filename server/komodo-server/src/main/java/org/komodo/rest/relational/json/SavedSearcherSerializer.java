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
import java.util.Arrays;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.KomodoSavedSearcher;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoSearchObject}s.
 */
public final class SavedSearcherSerializer extends TypeAdapter< KomodoSavedSearcher > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoSavedSearcher read( final JsonReader in ) throws IOException {
        final KomodoSavedSearcher status = new KomodoSavedSearcher();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoSavedSearcher.NAME_LABEL:
                    status.setName(in.nextString());
                    break;
                case KomodoSavedSearcher.QUERY_LABEL:
                    status.setQuery(in.nextString());
                    break;
                case KomodoSavedSearcher.PARAMETER_LABEL:
                    final String[] parameters = BUILDER.fromJson( in, String[].class );
                    status.setParameters(Arrays.asList(parameters));
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return status;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoSavedSearcher value ) throws IOException {

        out.beginObject();

        // Title of object
        out.name(KomodoSavedSearcher.NAME_LABEL);
        out.value(value.getName());

        out.name(KomodoSavedSearcher.QUERY_LABEL);
        out.value(value.getQuery());

        if (value.getParameters() != null && ! value.getParameters().isEmpty()) {
            out.name(KomodoSavedSearcher.PARAMETER_LABEL);
            out.beginArray();
            for (String val: value.getParameters().toArray(new String[0])) {
                out.value(val);
            }
            out.endArray();
        }

        out.endObject();
    }

}
