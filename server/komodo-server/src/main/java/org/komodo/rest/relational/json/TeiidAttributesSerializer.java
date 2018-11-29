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
import org.komodo.rest.relational.request.KomodoTeiidAttributes;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoSearchObject}s.
 */
public final class TeiidAttributesSerializer extends TypeAdapter< KomodoTeiidAttributes > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoTeiidAttributes read( final JsonReader in ) throws IOException {
        final KomodoTeiidAttributes teiidAttrs = new KomodoTeiidAttributes();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case KomodoTeiidAttributes.ADMIN_USER_LABEL:
                    teiidAttrs.setAdminUser(in.nextString());
                    break;
                case KomodoTeiidAttributes.ADMIN_PASSWD_LABEL:
                    teiidAttrs.setAdminPasswd(in.nextString());
                    break;
                case KomodoTeiidAttributes.ADMIN_PORT_LABEL:
                    teiidAttrs.setAdminPort(in.nextInt());
                    break;
                case KomodoTeiidAttributes.ADMIN_SECURE_LABEL:
                    teiidAttrs.setAdminSecure(in.nextBoolean());
                    break;
                case KomodoTeiidAttributes.JDBC_USER_LABEL:
                    teiidAttrs.setJdbcUser(in.nextString());
                    break;
                case KomodoTeiidAttributes.JDBC_PASSWD_LABEL:
                    teiidAttrs.setJdbcPasswd(in.nextString());
                    break;
                case KomodoTeiidAttributes.JDBC_PORT_LABEL:
                    teiidAttrs.setJdbcPort(in.nextInt());
                    break;
                case KomodoTeiidAttributes.JDBC_SECURE_LABEL:
                    teiidAttrs.setJdbcSecure(in.nextBoolean());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return teiidAttrs;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoTeiidAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoTeiidAttributes.ADMIN_USER_LABEL);
        out.value(value.getAdminUser());

        out.name(KomodoTeiidAttributes.ADMIN_PASSWD_LABEL);
        out.value(value.getAdminPasswd());

        out.name(KomodoTeiidAttributes.ADMIN_PORT_LABEL);
        out.value(value.getAdminPort());

        Boolean adminSecure = value.isAdminSecure();
        if (adminSecure != null) {
            out.name(KomodoTeiidAttributes.ADMIN_SECURE_LABEL);
            out.value(adminSecure);
        }

        out.name(KomodoTeiidAttributes.JDBC_USER_LABEL);
        out.value(value.getJdbcUser());

        out.name(KomodoTeiidAttributes.JDBC_PASSWD_LABEL);
        out.value(value.getJdbcPasswd());

        out.name(KomodoTeiidAttributes.JDBC_PORT_LABEL);
        out.value(value.getJdbcPort());

        Boolean jdbcSecure = value.isJdbcSecure();
        if (jdbcSecure != null) {
            out.name(KomodoTeiidAttributes.JDBC_SECURE_LABEL);
            out.value(jdbcSecure);
        }

        out.endObject();
    }

}
