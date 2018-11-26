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

import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.response.RestConnectionSummary;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus;

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
            RestMetadataConnectionStatus status = BUILDER.fromJson(in, RestMetadataConnectionStatus.class);
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
    	RestMetadataConnectionStatus status = summary.getStatus();
    	if(status!=null) {
    		out.name(RestConnectionSummary.STATUS_LABEL);
    		BUILDER.getAdapter( RestMetadataConnectionStatus.class ).write( out, summary.getStatus() );
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
