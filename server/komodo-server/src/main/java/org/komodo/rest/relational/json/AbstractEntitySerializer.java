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

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;
import java.lang.reflect.Type;
import java.net.URI;
import java.net.URL;
import java.util.Map;

import org.komodo.rest.AbstractKEntity;
import org.komodo.rest.Messages;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.json.JsonConstants;

import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for the Komodo REST objects.
 *
 * @param <T>
 *        the {@link RestBasicEntity} subclass
 */
public abstract class AbstractEntitySerializer< T extends AbstractKEntity > extends TypeAdapter< T >
    implements JsonConstants {

    protected static final Type BOOLEAN_MAP_TYPE = new TypeToken< Map< String, Boolean > >() {/* nothing to do */}.getType();

    protected static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    protected void beginRead( final JsonReader in ) throws IOException {
        in.beginObject();
    }

    protected void beginWrite( final JsonWriter out ) throws IOException {
        out.beginObject();
    }

    protected void endRead( final JsonReader in ) throws IOException {
        in.endObject();
    }

    protected void endWrite( final JsonWriter out ) throws IOException {
        out.endObject();
    }

    /**
     * @return new instance of the targetted entity
     */
    protected abstract T createEntity();

    /**
     * Sub-classes should implement this to write further data to the json
     *
     * @param name
     * @param entity
     * @param in
     *
     * @throws IOException
     */
    protected abstract String readExtension(String name, T entity, JsonReader in);

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public T read(final JsonReader in) throws IOException {
        final T entity = createEntity();

        beginRead(in);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (readExtension(name, entity, in) != null)
                continue;

                JsonToken token = in.peek();
                switch (token) {
                    case BOOLEAN:
                        entity.addTuple(name, in.nextBoolean());
                        break;
                    case NUMBER:
                    {
                        double value = in.nextDouble();
                        if (value % 1 == 0)
                            entity.addTuple(name, (int) value);
                        else
                            entity.addTuple(name, value);
                        break;
                    }
                    case STRING:
                    {
                        String value = in.nextString();

                        try {
                            URI uri = new URL(value).toURI();
                            entity.addTuple(name, uri);
                        } catch (Exception ex) {
                            // Cannot parse so add as string
                            entity.addTuple(name, value);
                        }

                        break;
                    }
                    case NULL:
                        in.nextNull();
                        entity.addTuple(name, null);
                        break;
                    case BEGIN_ARRAY:
                    {
                        final Object[] value = BUILDER.fromJson( in, Object[].class );

                        //
                        // BUILDER always converts json numbers to double regardless
                        // of them being integers so need to do some checking and on-the-fly
                        // conversion
                        //
                        for (int i = 0; i < value.length; ++i) {
                            if (value[i] instanceof Double && ((double) value[i] % 1) == 0)
                                value[i] = ((Double) value[i]).intValue();
                        }

                        entity.addTuple(name, value);
                        break;
                    }
                    default:
                        throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
                }
        }

        if ( !isComplete( entity ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, getClass().getSimpleName() ) );
        }

        endRead(in);
        return entity;
    }

    /**
     * Sub-classes should implement this to write further data to the json
     *
     * @param out
     * @param entity
     * @throws IOException
     */
    protected abstract void writeExtensions(final JsonWriter out, final T entity) throws IOException;

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write(final JsonWriter out, final T entity) throws IOException {
        if (!isComplete(entity)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, getClass().getSimpleName()));
        }

        beginWrite(out);

        writeTuples(out, entity);

        writeExtensions(out, entity);

        endWrite(out);
    }

    protected void writeValue(final JsonWriter out, Object value) throws IOException {
        if (value == null)
            out.nullValue();
        else if (value instanceof Boolean)
            out.value((Boolean) value);
        else if (value instanceof Integer)
            out.value((int) value);
        else if (value instanceof Long)
            out.value((long) value);
        else if (value instanceof Double)
            out.value((double) value);
        else if (value instanceof Float)
            out.value((double) value);
        else if (value instanceof String[]) {
            out.beginArray();
            for (String val: (String[]) value) {
                out.value(val);
            }
            out.endArray();
        } else if (value instanceof Object[]) {
            out.beginArray();
            for (Object val: (Object[]) value) {
                writeValue(out, val);
            }
            out.endArray();
        } else
            out.value(value.toString());
    }

    protected void writeTuples(final JsonWriter out, final T entity) throws IOException {
        for (Map.Entry<String, Object>entry : entity.getTuples().entrySet()) {
            out.name(entry.getKey());
            Object value = entry.getValue();
            writeValue(out, value);
        }
    }

    /**
     * @param entity the entity
     * @return true if entity's id, data path and kType have been populated, false otherwise
     */
    protected abstract boolean isComplete(T entity);
}
