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
import org.komodo.rest.relational.request.KomodoFileAttributes;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoFileAttributes}.
 */
public final class FileAttributesSerializer extends AbstractContentSerializer<KomodoFileAttributes> {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoFileAttributes read( final JsonReader in ) throws IOException {
        final KomodoFileAttributes fileAttr = new KomodoFileAttributes();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (readContent(in, fileAttr, name) != null)
                continue;

            switch ( name ) {
                case KomodoFileAttributes.NAME_LABEL:
                    fileAttr.setName(in.nextString());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return fileAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out, final KomodoFileAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoFileAttributes.NAME_LABEL);
        out.value(value.getName());

        writeContent(out, value);

        out.endObject();
    }

}