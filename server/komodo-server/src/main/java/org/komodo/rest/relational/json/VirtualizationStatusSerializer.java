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
import java.util.List;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.virtualization.RestRouteStatus;
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
                case RestVirtualizationStatus.ROUTES_LABEL:
                    final RestRouteStatus[] routes = BUILDER.fromJson( in, RestRouteStatus[].class );
                    virtual.setRoutes(routes);
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

        List<RestRouteStatus> routes = value.getRoutes();
        if (routes != null && routes.size() > 0) {
            out.name(RestVirtualizationStatus.ROUTES_LABEL);
            BUILDER.toJson(routes.toArray(new RestRouteStatus[0]), RestRouteStatus[].class, out);
        }

        out.endObject();
    }
}
