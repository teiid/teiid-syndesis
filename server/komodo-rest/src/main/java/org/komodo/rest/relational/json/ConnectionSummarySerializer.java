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

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;

import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.response.RestConnectionSummary;
import org.komodo.rest.relational.response.RestNamedVdbStatus;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for a {@link RestConnectionSummary}.
 */
public final class ConnectionSummarySerializer  extends BasicEntitySerializer< RestConnectionSummary > {

    /**
     * {@inheritDoc}
     * 
     * @see org.komodo.rest.relational.json.BasicEntitySerializer#createEntity()
     */
    @Override
    protected RestConnectionSummary createEntity() {
        return new RestConnectionSummary();
    }
    
    @Override
    protected String readExtension(String name, RestConnectionSummary summary, JsonReader in) {
        if (RestConnectionSummary.CONNECTION_LABEL.equals(name)) {
            RestConnection conn = BUILDER.fromJson(in, RestConnection.class);
            summary.setConnection(conn);
            return name;
        } else if (RestConnectionSummary.STATUS_LABEL.equals(name)) {
            RestNamedVdbStatus status = BUILDER.fromJson(in, RestNamedVdbStatus.class);
            summary.setStatus(status);
            return name;
        }

        return null;
    }

    @Override
    protected void writeExtensions(JsonWriter out, RestConnectionSummary summary) throws IOException {
    	RestConnection conn = summary.getConnection();
    	if(conn!=null) {
    		out.name(RestConnectionSummary.CONNECTION_LABEL);
    		BUILDER.getAdapter( RestConnection.class ).write( out, summary.getConnection() );
    	}
    	RestNamedVdbStatus status = summary.getStatus();
    	if(status!=null) {
    		out.name(RestConnectionSummary.STATUS_LABEL);
    		BUILDER.getAdapter( RestNamedVdbStatus.class ).write( out, summary.getStatus() );
    	}
    }

    /**
     * {@inheritDoc}
     * 
     * @see org.komodo.rest.relational.json.BasicEntitySerializer#isComplete(org.komodo.rest.RestBasicEntity)
     */
    @Override
    protected boolean isComplete( final RestConnectionSummary entity ) {
        return true;
    }

}
