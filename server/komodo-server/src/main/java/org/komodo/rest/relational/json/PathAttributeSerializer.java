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
import org.komodo.rest.relational.request.KomodoPathAttribute;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoArtifactPathAttribute}.
 */
public class PathAttributeSerializer<T extends KomodoPathAttribute> extends TypeAdapter<T> {

    @SuppressWarnings( "unchecked" )
    protected T createEntity() {
        return (T) new KomodoPathAttribute();
    }

    protected String readPath(JsonReader in, String name, T pathAttr) throws IOException {
        if (KomodoPathAttribute.PATH_LABEL.equals(name)) {
            String path = in.nextString();
            pathAttr.setPath(path);
            return path;
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public T read( final JsonReader in ) throws IOException {
        final T pathAttr = createEntity();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();
            readPath(in, name, pathAttr);
        }

        in.endObject();

        return pathAttr;
    }

    protected void writePath(JsonWriter out, T value) throws IOException {
        out.name(KomodoPathAttribute.PATH_LABEL);
        out.value(value.getPath());
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out, final T value ) throws IOException {

        out.beginObject();
        writePath(out, value);
        out.endObject();
    }

}
