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
import java.util.Collections;
import java.util.List;

import org.komodo.rest.relational.connection.ConnectionSchemaProperty;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public class ConnectionSchemaPropertyListSerializer extends TypeAdapter<List<ConnectionSchemaProperty>> {

    @Override
    public List<ConnectionSchemaProperty> read(JsonReader in) throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public void write(JsonWriter out, List<ConnectionSchemaProperty> properties) throws IOException {
        if (properties == null)
            properties = Collections.emptyList();

        out.beginObject();
        for (ConnectionSchemaProperty property : properties) {
            out.name(property.getName());
            BUILDER.toJson(property, property.getClass(), out);
        }

        out.endObject();
    }
}
