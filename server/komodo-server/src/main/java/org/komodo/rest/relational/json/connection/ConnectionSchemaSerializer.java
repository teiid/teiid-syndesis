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
package org.komodo.rest.relational.json.connection;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;

import org.komodo.rest.relational.connection.ConnectionSchema;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status ConnectionSchema}.
 */
public class ConnectionSchemaSerializer extends TypeAdapter<ConnectionSchema> {

    @Override
    public void write(JsonWriter out, ConnectionSchema source) throws IOException {
        out.beginObject();

        out.name(ConnectionSchema.ID_LABEL);
        out.value(source.getId());

        out.name(ConnectionSchema.KTYPE_LABEL);
        out.value(source.getkType());

        out.name(ConnectionSchema.DESCRIPTION_LABEL);
        out.value(source.getDescription());

        if (source.getProperties() == null || source.getProperties().isEmpty())
            return;

        out.name(ConnectionSchema.PROPERTIES_LABEL);
        BUILDER.toJson(source.getProperties(), ConnectionSchemaPropertyListSerializer.class, out);

        out.endObject();
    }

    @Override
    public ConnectionSchema read(JsonReader in) throws IOException {
        throw new UnsupportedOperationException();
    }
}
