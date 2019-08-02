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
package org.komodo.rest.relational.connection;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * Used to build a JSON representation for a schema node
 */
@JsonSerialize(as = RestSchemaNode.class)
@JsonInclude(Include.NON_NULL)
public class RestSchemaNode {

    private List<RestSchemaNode> children = new ArrayList<RestSchemaNode>();
    
    private String name;

    private String connectionName;

    private String type;
    
    private String path;
    
    private boolean queryable = false;

    /**
     * Constructor for use when deserializing
     */
    public RestSchemaNode() {
        super();
    }

    /**
     * Constructor
     * @param connName source name
     * @param name node name
     * @param type type
     */
    public RestSchemaNode(String connName, String name, String type) {
        this.connectionName = connName;
        this.name = name;
        this.type = type;
    }

    /**
     * Get connection name
     * @return the connection name
     */
    public String getConnectionName() {
        return connectionName;
    }

    /**
     * Set connection name
     * @param connName connection name
     */
    public void setConnectionName(String connName) {
        this.connectionName = connName;
    }

    /**
     * Get node name
     * @return the node name
     */
    public String getName() {
        return name;
    }

    /**
     * Set node name
     * @param name the node name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Get node type
     * @return the node type
     */
    public String getType() {
        return type;
    }

    /**
     * Set node type
     * @param type the node type
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Get node path
     * @return the node path
     */
    public String getPath() {
        return path;
    }

    /**
     * Set node path
     * @param path the node path
     */
    public void setPath(String path) {
        this.path = path;
    }

    /**
     * Get isQueryable state
     * @return 'true' if is queryable
     */
    public boolean isQueryable() {
        return queryable;
    }

    /**
     * Set queryable state
     * @param queryable 'true' if queryable
     */
    public void setQueryable(boolean queryable) {
        this.queryable = queryable;
    }

    /**
     * Get node children
     * @return the node children
     */
    public RestSchemaNode[] getChildren() {
        return children.toArray(new RestSchemaNode[0]);
    }

    /**
     * Set node children
     * @param children the node children
     */
    public void setChildren(RestSchemaNode[] children) {
        if (children == null || children.length == 0)
            this.children = Collections.emptyList();

        this.children = new ArrayList<>();
        for (RestSchemaNode child : children) {
            this.children.add(child);
        }
    }

    /**
     * Add node child
     * @param child the child node
     */
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
