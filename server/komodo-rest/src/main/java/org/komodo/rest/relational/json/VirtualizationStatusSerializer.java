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

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.virtualization.RestVirtualizationStatus;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestTeiidVdbStatusVdb}s.
 */
public final class VirtualizationStatusSerializer extends TypeAdapter< RestVirtualizationStatus > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestVirtualizationStatus read( final JsonReader in ) throws IOException {
        final RestVirtualizationStatus virtual = new RestVirtualizationStatus();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestVirtualizationStatus.VDB_NAME_LABEL:
                    virtual.setVdbName(in.nextString());
                    break;
                case RestVirtualizationStatus.BUILD_NAME_LABEL:
                    virtual.setBuildName(in.nextString());
                    break;
                case RestVirtualizationStatus.DEPLOYMENT_NAME_LABEL:
                    virtual.setDeploymentName(in.nextString());
                    break;
                case RestVirtualizationStatus.NAMESPACE_LABEL:
                    virtual.setNamespace(in.nextString());
                    break;
                case RestVirtualizationStatus.LAST_UPDATED_LABEL:
                    virtual.setLastUpdated(in.nextString());
                    break;
                case RestVirtualizationStatus.STATUS_LABEL:
                    virtual.setStatus(in.nextString());
                    break;
                case RestVirtualizationStatus.STATUS_MSG_LABEL:
                    virtual.setStatusMsg(in.nextString());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return virtual;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestVirtualizationStatus value ) throws IOException {

        out.beginObject();

        out.name(RestVirtualizationStatus.VDB_NAME_LABEL);
        out.value(value.getVdbName());

        out.name(RestVirtualizationStatus.BUILD_NAME_LABEL);
        out.value(value.getBuildName());

        out.name(RestVirtualizationStatus.DEPLOYMENT_NAME_LABEL);
        out.value(value.getDeploymentName());

        out.name(RestVirtualizationStatus.NAMESPACE_LABEL);
        out.value(value.getNamespace());

        out.name(RestVirtualizationStatus.LAST_UPDATED_LABEL);
        out.value(value.getLastUpdated());

        out.name(RestVirtualizationStatus.STATUS_LABEL);
        out.value(value.getStatus());

        out.name(RestVirtualizationStatus.STATUS_MSG_LABEL);
        out.value(value.getStatusMsg());

        out.endObject();
    }
}
