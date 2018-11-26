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
import java.util.Arrays;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatusVdb;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

public class MetadataVdbStatusSerializer extends AbstractEntitySerializer<RestMetadataVdbStatus> {

    @Override
    protected boolean isComplete(final RestMetadataVdbStatus status) {
        return status.getBaseUri() != null;
    }

    @Override
    protected RestMetadataVdbStatus createEntity() {
        return new RestMetadataVdbStatus();
    }

    @Override
    protected String readExtension(String name, RestMetadataVdbStatus entity, JsonReader in) {
        // nothing required
        return null;
    }

    @Override
    protected void writeExtensions(JsonWriter out, RestMetadataVdbStatus entity) throws IOException {
        // nothing required    
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.rest.relational.json.AbstractEntitySerializer#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestMetadataVdbStatus read(final JsonReader in) throws IOException {
        final RestMetadataVdbStatus entity = createEntity();

        beginRead(in);

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestMetadataVdbStatus.VDBS_LABEL.equals(name)) {
                final RestMetadataVdbStatusVdb[] vdbStatuses = BUILDER.fromJson(in, RestMetadataVdbStatusVdb[].class );
                entity.setVdbProperties(Arrays.asList(vdbStatuses));
            } else if (LINKS.equals(name))
                readLinks(in, entity);
            else {
                JsonToken token = in.peek();
                switch (token) {
                    case BOOLEAN:
                        entity.addTuple(name, in.nextBoolean());
                        break;
                    case NUMBER:
                        entity.addTuple(name, in.nextInt());
                        break;
                    case STRING:
                        entity.addTuple(name, in.nextString());
                        break;
                    case NULL:
                        in.nextNull();
                        entity.addTuple(name, null);
                        break;
                    case BEGIN_ARRAY:
                        final String[] value = BUILDER.fromJson( in, String[].class );
                        entity.addTuple(name, value);
                        break;
                    default:
                        throw new IOException(Messages.getString(Messages.Error.UNEXPECTED_JSON_TOKEN, name));
                }
            }
        }

        if ( !isComplete( entity ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, getClass().getSimpleName() ) );
        }

        endRead(in);
        return entity;
    }

    @Override
    public void write(JsonWriter out, RestMetadataVdbStatus entity) throws IOException {
        if (!isComplete(entity)) {
            throw new IOException(Messages.getString(INCOMPLETE_JSON, getClass().getSimpleName()));
        }

        beginWrite(out);

        writeTuples(out, entity);

        out.name(RestMetadataVdbStatus.VDBS_LABEL);
        BUILDER.toJson( entity.getVdbProperties().toArray(new RestMetadataVdbStatusVdb[0]), RestMetadataVdbStatusVdb[].class, out );

        writeLinks(out, entity);

        endWrite(out);
    }
}
