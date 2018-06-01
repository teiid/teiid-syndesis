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
package org.komodo.rest.relational.connection;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.ws.rs.core.MediaType;

import org.komodo.rest.KRestEntity;

public class RestSchemaNode implements KRestEntity {

    /**
     * Label for connection name
     */
    public static final String CONNECTION_NAME_LABEL = "connectionName";

    /**
     * Label for name
     */
    public static final String NAME_LABEL = "name";

    /**
     * Label for type
     */
    public static final String TYPE_LABEL = "type";

    /**
     * Label for queryable
     */
    public static final String QUERYABLE_LABEL = "queryable";

    /**
     * Label for parent
     */
    public static final String CHILDREN_LABEL = "children";

    private List<RestSchemaNode> children = new ArrayList<RestSchemaNode>();
    
    private String name;

    private String connectionName;

    private String type;
    
    private boolean queryable = false;

    /**
     * Constructor for use when deserializing
     */
    public RestSchemaNode() {
        super();
    }

    public RestSchemaNode(String connName, String name, String type) {
        this.connectionName = connName;
        this.name = name;
        this.type = type;
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    public String getConnectionName() {
        return connectionName;
    }

    public void setConnectionName(String connName) {
        this.connectionName = connName;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public boolean isQueryable() {
        return queryable;
    }

    public void setQueryable(boolean queryable) {
        this.queryable = queryable;
    }

    public RestSchemaNode[] getChildren() {
        return children.toArray(new RestSchemaNode[0]);
    }

    public void setChildren(RestSchemaNode[] children) {
        if (children == null || children.length == 0)
            this.children = Collections.emptyList();

        this.children = new ArrayList<>();
        for (RestSchemaNode child : children) {
            this.children.add(child);
        }
    }

    public void addChild(RestSchemaNode child) {
        if ( !hasChild(child) ) {
        	this.children.add(child);
        }
    }

    private boolean hasChild(RestSchemaNode child) {
    	boolean hasChild = false;
    	for( RestSchemaNode node : this.children ) {
    		if( node.getName().equals(child.getName()) &&
    			node.getType().equals(child.getType()) ) {
    			hasChild = true;
    			break;
    		}
    	}
    	return hasChild;
    }
}
