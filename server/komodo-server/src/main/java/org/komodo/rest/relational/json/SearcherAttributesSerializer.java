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
import java.util.Map;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.request.KomodoSearcherAttributes;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoSearchObject}s.
 */
public final class SearcherAttributesSerializer extends PathAttributeSerializer< KomodoSearcherAttributes > {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    @Override
    protected KomodoSearcherAttributes createEntity() {
        return new KomodoSearcherAttributes();
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoSearcherAttributes read( final JsonReader in ) throws IOException {
        final KomodoSearcherAttributes searcherAttr = createEntity();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (readPath(in, name, searcherAttr) != null)
                continue;

            switch ( name ) {
                case KomodoSearcherAttributes.SEARCH_NAME_LABEL:
                    searcherAttr.setSearchName(in.nextString());
                    break;
                case KomodoSearcherAttributes.TYPE_LABEL:
                    searcherAttr.setType(in.nextString());
                    break;
                case KomodoSearcherAttributes.PARENT_LABEL:
                    searcherAttr.setParent(in.nextString());
                    break;
                case KomodoSearcherAttributes.ANCESTOR_LABEL:
                    searcherAttr.setAncestor(in.nextString());
                    break;
                case KomodoSearcherAttributes.CONTAINS_LABEL:
                    searcherAttr.setContains(in.nextString());
                    break;
                case KomodoSearcherAttributes.OBJECT_NAME_LABEL:
                    searcherAttr.setObjectName(in.nextString());
                    break;
                case KomodoSearcherAttributes.PARAMETERS_LABEL:
                    Map<String, String> parameters = BUILDER.fromJson(in, Map.class);
                    for (Map.Entry<String, String> parameter : parameters.entrySet()) {
                        searcherAttr.setParameter(parameter.getKey(), parameter.getValue());
                    }
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return searcherAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoSearcherAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoSearcherAttributes.SEARCH_NAME_LABEL);
        out.value(value.getSearchName());

        out.name(KomodoSearcherAttributes.ANCESTOR_LABEL);
        out.value(value.getAncestor());

        out.name(KomodoSearcherAttributes.CONTAINS_LABEL);
        out.value(value.getContains());

        out.name(KomodoSearcherAttributes.OBJECT_NAME_LABEL);
        out.value(value.getObjectName());

        out.name(KomodoSearcherAttributes.PARENT_LABEL);
        out.value(value.getParent());

        writePath(out, value);

        out.name(KomodoSearcherAttributes.TYPE_LABEL);
        out.value(value.getType());

        if (! value.getParameters().isEmpty()) {
            out.name(KomodoSearcherAttributes.PARAMETERS_LABEL);
            BUILDER.toJson(value.getParameters(), STRING_MAP_TYPE, out);
        }

        out.endObject();
    }

}
