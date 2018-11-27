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
import org.komodo.rest.relational.AbstractKomodoContentAttribute;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status AbstractKomodoContentAttribute}.
 */
public abstract class AbstractContentSerializer<T extends AbstractKomodoContentAttribute> extends TypeAdapter<T> {

    protected String readContent(final JsonReader in, final AbstractKomodoContentAttribute contentAttr, final String name)
        throws IOException {
        if (AbstractKomodoContentAttribute.CONTENT_LABEL.equals(name)) {
            contentAttr.setContent(in.nextString());
            return name;
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public abstract T read(final JsonReader in) throws IOException;

    protected void writeContent(final JsonWriter out, final AbstractKomodoContentAttribute value) throws IOException {
        out.name(AbstractKomodoContentAttribute.CONTENT_LABEL);
        out.value(value.getContent());
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public abstract void write(final JsonWriter out, final T value) throws IOException;
}
