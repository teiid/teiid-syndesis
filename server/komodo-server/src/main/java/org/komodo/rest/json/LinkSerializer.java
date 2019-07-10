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
package org.komodo.rest.json;

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;

import java.io.IOException;

import javax.ws.rs.core.UriBuilder;

import org.komodo.rest.Messages;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestLink}s.
 */
public final class LinkSerializer extends TypeAdapter< RestLink > {

    private boolean isComplete( final RestLink link ) {
        return ( ( link.getRel() != null ) && ( link.getHref() != null ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public RestLink read( final JsonReader in ) throws IOException {
        final RestLink link = new RestLink();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case JsonConstants.HREF:
                    final String uri = in.nextString();
                    link.setHref( UriBuilder.fromUri( uri ).build() );
                    break;
                case JsonConstants.REL:
                    final String rel = in.nextString();
                    link.setRel( LinkType.fromString( rel ) );
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        if ( !isComplete( link ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, LinkSerializer.class.getSimpleName() ) );
        }

        return link;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final RestLink value ) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestLink.class.getSimpleName() ) );
        }

        out.beginObject();

        // rel
        out.name( JsonConstants.REL );
        out.value( value.getRel().toString() );

        // href
        out.name( JsonConstants.HREF );
        out.value( value.getHref().toString() );

        out.endObject();
    }

}
