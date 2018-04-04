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

import javax.ws.rs.core.MediaType;

import org.komodo.rest.KRestEntity;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatusVdb;

public final class RestNamedVdbStatus implements KRestEntity {

    /**
     * An empty array of statuses.
     */
    public static final RestNamedVdbStatus[] NO_STATUSES = new RestNamedVdbStatus[ 0 ];

    public static final String OBJECT_NAME_LABEL = "objectName";

    public static final String VDB_STATUS_LABEL = "vdbStatus";

    private String name;

    private RestMetadataVdbStatusVdb status;

    /**
     * Constructor for use when deserializing.
     */
    public RestNamedVdbStatus() {
        super();
    }

    public RestNamedVdbStatus( final String name ) {
        this.name = name;
    }

    public RestNamedVdbStatus( final String name,
    		                   final RestMetadataVdbStatusVdb status ) {
        this.name = name;
        this.status = status;
    }

    public String getName() {
        return name;
    }

    public RestMetadataVdbStatusVdb getVdbStatus() {
        return this.status == null ? RestMetadataVdbStatusVdb.NO_VDB_STATUS : this.status;
    }

	@Override
	public Object getXml() {
		throw new UnsupportedOperationException();
	}

    public void setName( final String name ) {
        this.name = name;
    }

    public void setVdbStatus( final RestMetadataVdbStatusVdb status ) {
    	this.status = status;
    }

	@Override
	public boolean supports( final MediaType mediaType ) {
        return MediaType.APPLICATION_JSON_TYPE.equals( mediaType );
	}

}
