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

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;

import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.utils.StringUtils;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestVdbDataRole}s.
 */
public final class VdbDataRoleSerializer extends BasicEntitySerializer<RestVdbDataRole> {

    @Override
    protected boolean isComplete( final RestVdbDataRole dataRole ) {
        return super.isComplete(dataRole) && !StringUtils.isBlank( dataRole.getName() );
    }

    @Override
    protected RestVdbDataRole createEntity() {
        return new RestVdbDataRole();
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see org.komodo.rest.relational.json.BasicEntitySerializer#readExtension(java.lang.String, org.komodo.rest.RestBasicEntity,
     *      com.google.gson.stream.JsonReader)
     */
    @Override
    protected String readExtension( final String name,
                                    final RestVdbDataRole dataRole,
                                    final JsonReader in ) {
        if ( RestVdbDataRole.PERMISSIONS_LABEL.equals( name ) ) {
            final RestVdbPermission[] permissions = BUILDER.fromJson( in, RestVdbPermission[].class );
            dataRole.setPermissions( permissions );
            return Integer.toString( permissions.length );
        }

        return null; // not processed
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see org.komodo.rest.relational.json.BasicEntitySerializer#writeExtensions(com.google.gson.stream.JsonWriter, org.komodo.rest.RestBasicEntity)
     */
    @Override
    protected void writeExtensions( final JsonWriter out,
                                    final RestVdbDataRole dataRole ) throws IOException {
        final RestVdbPermission[] permissions = dataRole.getPermissions();
        
        if ( permissions.length != 0 ) {
            out.name( RestVdbDataRole.PERMISSIONS_LABEL );
            out.beginArray();
            
            for ( final RestVdbPermission permission : permissions ) {
                BUILDER.getAdapter( RestVdbPermission.class ).write( out, permission );
            }
            
            out.endArray();
        }
    }
    
}
