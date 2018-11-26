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

import java.io.IOException;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoArtifactPathAttribute}.
 */
public class QueryAttributeSerializer extends TypeAdapter<KomodoQueryAttribute> {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoQueryAttribute read( final JsonReader in ) throws IOException {
        final KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();
            switch (name) {
                case KomodoQueryAttribute.QUERY_LABEL:
                    queryAttr.setQuery(in.nextString());
                    break;
                case KomodoQueryAttribute.TARGET_LABEL:
                    queryAttr.setTarget(in.nextString());
                    break;
                case KomodoQueryAttribute.LIMIT_LABEL:
                    queryAttr.setLimit(in.nextInt());
                    break;
                case KomodoQueryAttribute.OFFSET_LABEL:
                    queryAttr.setOffset(in.nextInt());
                    break;
            }
        }

        in.endObject();

        return queryAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out, final KomodoQueryAttribute value ) throws IOException {
        out.beginObject();

        out.name(KomodoQueryAttribute.QUERY_LABEL);
        out.value(value.getQuery());

        out.name(KomodoQueryAttribute.TARGET_LABEL);
        out.value(value.getTarget());

        out.name(KomodoQueryAttribute.LIMIT_LABEL);
        out.value(value.getLimit());

        out.name(KomodoQueryAttribute.OFFSET_LABEL);
        out.value(value.getOffset());

        out.endObject();
    }

}
