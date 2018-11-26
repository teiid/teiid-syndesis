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
import java.util.Arrays;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatusVdb;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestTeiidVdbStatusVdb}s.
 */
public final class MetadataVdbStatusVdbSerializer extends TypeAdapter< RestMetadataVdbStatusVdb > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestMetadataVdbStatusVdb read( final JsonReader in ) throws IOException {
        final RestMetadataVdbStatusVdb vdb = new RestMetadataVdbStatusVdb();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestMetadataVdbStatusVdb.VDB_STATUS_NAME:
                    vdb.setName(in.nextString());
                    break;
                case RestMetadataVdbStatusVdb.VDB_STATUS_DEPLOYED_NAME:
                    vdb.setDeployedName(in.nextString());
                    break;
                case RestMetadataVdbStatusVdb.VDB_STATUS_VERSION:
                    vdb.setVersion(in.nextString());
                    break;
                case RestMetadataVdbStatusVdb.VDB_STATUS_ACTIVE:
                    vdb.setActive(in.nextBoolean());
                    break;
                case RestMetadataVdbStatusVdb.VDB_STATUS_LOADING:
                    vdb.setLoading(in.nextBoolean());
                    break;
                case RestMetadataVdbStatusVdb.VDB_STATUS_FAILED:
                    vdb.setFailed(in.nextBoolean());
                    break;
                case RestMetadataVdbStatusVdb.VDB_STATUS_ERROR:
                    final String[] errors = BUILDER.fromJson( in, String[].class );
                    vdb.setErrors(Arrays.asList(errors));
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return vdb;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestMetadataVdbStatusVdb value ) throws IOException {

        out.beginObject();

        out.name(RestMetadataVdbStatusVdb.VDB_STATUS_NAME);
        out.value(value.getName());

        out.name(RestMetadataVdbStatusVdb.VDB_STATUS_DEPLOYED_NAME);
        out.value(value.getDeployedName());

        out.name(RestMetadataVdbStatusVdb.VDB_STATUS_VERSION);
        out.value(value.getVersion());

        out.name(RestMetadataVdbStatusVdb.VDB_STATUS_ACTIVE);
        out.value(value.isActive());

        out.name(RestMetadataVdbStatusVdb.VDB_STATUS_LOADING);
        out.value(value.isLoading());

        out.name(RestMetadataVdbStatusVdb.VDB_STATUS_FAILED);
        out.value(value.isFailed());

        out.name(RestMetadataVdbStatusVdb.VDB_STATUS_ERROR);
        out.beginArray();
        if(value.getErrors() != null ) {
            for (String val: value.getErrors()) {
                out.value(val);
            }
        }
        out.endArray();

        out.endObject();
    }
}
