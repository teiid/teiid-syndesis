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
import org.komodo.rest.relational.response.RestStorageTypeDescriptor;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestStorageTypeDescriptor}.
 */
public class StorageTypeDescriptorSerializer extends TypeAdapter<RestStorageTypeDescriptor> {

    @Override
    public RestStorageTypeDescriptor read(JsonReader in) throws IOException {
        final RestStorageTypeDescriptor storageTypeDescriptor = new RestStorageTypeDescriptor();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestStorageTypeDescriptor.NAME_LABEL:
                    storageTypeDescriptor.setName(in.nextString());
                    break;
                case RestStorageTypeDescriptor.DESCRIPTION_LABEL:
                    storageTypeDescriptor.setDescription(in.nextString());
                    break;
                case RestStorageTypeDescriptor.REQUIRED_LABEL:
                    storageTypeDescriptor.setRequired(in.nextBoolean());
                    break;
                case RestStorageTypeDescriptor.ENCODED_LABEL:
                    storageTypeDescriptor.setEncoded(in.nextBoolean());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return storageTypeDescriptor;
    }

    @Override
    public void write(JsonWriter out, RestStorageTypeDescriptor value) throws IOException {
        out.beginObject();

        out.name(RestStorageTypeDescriptor.NAME_LABEL);
        out.value(value.getName());

        out.name(RestStorageTypeDescriptor.DESCRIPTION_LABEL);
        out.value(value.getDescription());

        out.name(RestStorageTypeDescriptor.REQUIRED_LABEL);
        out.value(value.isRequired());

        out.name(RestStorageTypeDescriptor.ENCODED_LABEL);
        out.value(value.isEncoded());

        out.endObject();
    }
}
