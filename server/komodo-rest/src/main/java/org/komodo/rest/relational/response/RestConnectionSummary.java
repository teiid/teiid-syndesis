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
package org.komodo.rest.relational.response;

import java.net.URI;

import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.connection.RestConnection;
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
	private RestNamedVdbStatus status;

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
                                   final RestNamedVdbStatus status ) throws KException {
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
    public RestNamedVdbStatus getStatus() {
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
    public void setStatus( final RestNamedVdbStatus status ) {
    	this.status = status;
    }

}
