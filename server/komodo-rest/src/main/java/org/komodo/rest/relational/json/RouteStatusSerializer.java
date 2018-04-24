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
import org.komodo.rest.relational.response.virtualization.RestRouteStatus;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status RestRouteStatus}es.
 */
public final class RouteStatusSerializer extends TypeAdapter< RestRouteStatus > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestRouteStatus read( final JsonReader in ) throws IOException {
        final RestRouteStatus route = new RestRouteStatus();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestRouteStatus.NAME_LABEL:
                    route.setName(in.nextString());
                    break;
                case RestRouteStatus.KIND_LABEL:
                    route.setKind(in.nextString());
                    break;
                case RestRouteStatus.TARGET_LABEL:
                    route.setTarget(in.nextString());
                    break;
                case RestRouteStatus.HOST_LABEL:
                    route.setHost(in.nextString());
                    break;
                case RestRouteStatus.PORT_LABEL:
                    route.setPort(in.nextString());
                    break;
                case RestRouteStatus.PATH_LABEL:
                    route.setPath(in.nextString());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return route;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestRouteStatus value ) throws IOException {

        out.beginObject();

        out.name(RestRouteStatus.NAME_LABEL);
        out.value(value.getName());

        out.name(RestRouteStatus.KIND_LABEL);
        out.value(value.getKind());

        out.name(RestRouteStatus.TARGET_LABEL);
        out.value(value.getTarget());

        out.name(RestRouteStatus.HOST_LABEL);
        out.value(value.getHost());

        out.name(RestRouteStatus.PORT_LABEL);
        out.value(value.getPort());

        out.name(RestRouteStatus.PATH_LABEL);
        out.value(value.getPath());

        out.endObject();
    }
}
