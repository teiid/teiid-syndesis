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
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.RestStorageType;
import org.komodo.rest.relational.response.RestStorageTypeDescriptor;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestStorageType}s.
 */
public class StorageTypeSerializer extends TypeAdapter< RestStorageType > {

        /**
         * {@inheritDoc}
         *
         * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
         */
        @Override
        public RestStorageType read( final JsonReader in ) throws IOException {
            final RestStorageType storageType = new RestStorageType();
            in.beginObject();

            while ( in.hasNext() ) {
                final String name = in.nextName();

                switch ( name ) {
                    case RestStorageType.NAME_LABEL:
                        storageType.setName(in.nextString());
                        break;
                    case RestStorageType.DESCRIPTION_LABEL:
                        storageType.setDescription(in.nextString());
                        break;
                    case RestStorageType.DESCRIPTORS_LABEL:
                        RestStorageTypeDescriptor[] descriptors = BUILDER.fromJson(in, RestStorageTypeDescriptor[].class);
                        storageType.setDescriptors(descriptors);
                        break;
                    default:
                        throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
                }
            }

            in.endObject();

            return storageType;
        }

        /**
         * {@inheritDoc}
         *
         * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
         */
        @Override
        public void write( final JsonWriter out, final RestStorageType value ) throws IOException {

            out.beginObject();

            // Title of object
            out.name(RestStorageType.NAME_LABEL);
            out.value(value.getName());

            out.name(RestStorageType.DESCRIPTION_LABEL);
            out.value(value.getDescription());

            if (value.getDescriptors() == null || value.getDescriptors().isEmpty())
                return;

            out.name(RestStorageType.DESCRIPTORS_LABEL);
            BUILDER.toJson(value.getDescriptors().toArray(new RestStorageTypeDescriptor[0]), RestStorageTypeDescriptor[].class, out);

            out.endObject();
        }
}
