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
     * Label for type
     */
    public static final String PATH_LABEL = "path";

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
    
    private String path;
    
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

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
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
