/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
