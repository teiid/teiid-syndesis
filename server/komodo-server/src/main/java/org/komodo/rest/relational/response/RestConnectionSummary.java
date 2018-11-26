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
package org.komodo.rest.relational.response;

import java.net.URI;

import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus;
import org.komodo.spi.KException;

/**
 * An entity containing a connection and it's status.
 */
public final class RestConnectionSummary extends RestBasicEntity {

    /**
     * JSON label for the connection.
     */
    public static final String CONNECTION_LABEL = "connection"; // //$NON-NLS-1$

    /**
     * JSON label for the connection status.
     */
    public static final String STATUS_LABEL = "status"; // //$NON-NLS-1$

	private RestConnection connection;
	private RestMetadataConnectionStatus status;

	/**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestConnectionSummary() {
    	// nothing to do
    }

    /**
     * @param baseUri the base URI of the REST request (cannot be <code>null</code>) 
     * @param connection the connection included in this summary (can be <code>null</code>)
     * @param status the connection status included in this summary (can be <code>null</code>)
     * @throws KException if an error occurs
     */
    public RestConnectionSummary( final URI baseUri,
                                   final RestConnection connection,
                                   final RestMetadataConnectionStatus status ) throws KException {
        setConnection( connection );
        setStatus( status );
    }

    /**
     * @return the connections (can be <code>null</code>)
     */
    public RestConnection getConnection() {
    	return this.connection;
    }

    /**
     * @return the connection status (can be <code>null</code>)
     */
    public RestMetadataConnectionStatus getStatus() {
    	return this.status;
    }

    /**
     * @param connection the connection being set (can be <code>null</code>)
     */
    public void setConnection( final RestConnection connection ) {
    	this.connection = connection;
    }

    /**
     * @param status the status being set (can be <code>null</code>)
     */
    public void setStatus( final RestMetadataConnectionStatus status ) {
    	this.status = status;
    }

}
