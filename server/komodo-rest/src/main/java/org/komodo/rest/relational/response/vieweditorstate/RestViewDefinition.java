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
package org.komodo.rest.relational.response.vieweditorstate;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.rest.RestBasicEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class RestViewDefinition extends RestBasicEntity {
	
//	private boolean isComplete;
	
    /*
     * The contents of the view editor state
     */
    private RestSqlComposition[] compositions = new RestSqlComposition[0];
    
    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestViewDefinition() {
        // nothing to do
    }
    
    public RestViewDefinition(final RestViewDefinition viewDef) throws KException {
    	super(viewDef.getBaseUri());
    	
    	String name = viewDef.getViewName();
        if( name != null ) {
        	tuples.put(RestViewEditorState.ID_VIEW_NAME, name);
        }
        if( viewDef.getDescription() != null ) {
        	tuples.put(RestViewEditorState.DESCRIPTION, viewDef.getDescription());
        }
        
        setComplete(viewDef.isComplete());
        
        String[] paths = viewDef.getSourcePaths();
        if( paths != null && paths.length > 0 ) {
        	tuples.put(RestViewEditorState.SOURCE_PATHS, paths);
        }

        List<RestSqlComposition> compList = new ArrayList<>();
        for (RestSqlComposition comp : viewDef.getSqlCompositions()) {
        	RestSqlComposition restComp = new RestSqlComposition(comp);
        	compList.add(restComp);
        }
        this.compositions = compList.toArray(new RestSqlComposition[0]);
    }

    /**
     * Constructor for those connections needing more control over what basic properties
     * should be set
     *
     * @param baseUri
     * @throws KException
     */
    public RestViewDefinition(URI baseUri, ViewDefinition viewDef, UnitOfWork transaction) throws KException {
        super(baseUri);
        
        
        String name = viewDef.getViewName(transaction);
        if( name != null ) {
        	tuples.put(RestViewEditorState.ID_VIEW_NAME, name);
        }
        if( viewDef.getDescription(transaction) != null ) {
        	tuples.put(RestViewEditorState.DESCRIPTION, viewDef.getDescription(transaction));
        }
        
        setComplete(viewDef.isComplete(transaction));
        
        String[] paths = viewDef.getSourcePaths(transaction);
        if( paths != null && paths.length > 0 ) {
        	tuples.put(RestViewEditorState.SOURCE_PATHS, paths);
        }

        List<RestSqlComposition> compList = new ArrayList<>();
        for (SqlComposition comp : viewDef.getSqlCompositions(transaction)) {
        	RestSqlComposition restComp = new RestSqlComposition(baseUri, comp, transaction);
        	compList.add(restComp);
        }
        this.compositions = compList.toArray(new RestSqlComposition[0]);
    }

    /**
     * @return the view definition name (can be empty)
     */
    public String getViewName() {
        Object viewName = tuples.get(RestViewEditorState.ID_VIEW_NAME);
        return viewName != null ? viewName.toString() : null;
    }

    /**
     * @param viewName
     *        the new viewName name (can be empty)
     */
    public void setViewName(final String viewName) {
        tuples.put(RestViewEditorState.ID_VIEW_NAME, viewName);
    }
    
    /**
     * @return the view definition name (can be empty)
     */
    public String getDescription() {
        Object result = tuples.get(RestViewEditorState.DESCRIPTION);
        return result != null ? result.toString() : null;
    }

    /**
     * @param description
     *        the new description (can be empty)
     */
    public void setDescription(final String description) {
        tuples.put(RestViewEditorState.DESCRIPTION, description);
    }
    
    /**
     * @return the view definition isComplete value (can be empty)
     */
    public boolean isComplete() {
        Object result = tuples.get(RestViewEditorState.IS_COMPLETE);
        return result != null ? Boolean.parseBoolean(result.toString()) : null;
    }

    /**
     * @param isComplete value
     *        the new description (can be empty)
     */
    public void setComplete(final boolean complete) {
        tuples.put(RestViewEditorState.IS_COMPLETE, Boolean.toString(complete));
    }
    
    /**
     * @return the string array of source table paths(can be empty)
     */
    public String[] getSourcePaths() {
    	Object[] pathsRaw = (Object[])tuples.get(RestViewEditorState.SOURCE_PATHS);
    	String[] pathsString = new String[pathsRaw.length];
    	int i = 0;
    	for( Object obj : pathsRaw ) {
    		pathsString[i++] = (String)obj;
    	}
        return pathsString;
    }
    
    /**
     * @param sourcePaths
     *        the source table paths
     */
    public void setSourcePaths(String[] sourcePaths) {
    	Object[] pathsRaw = (Object[])tuples.get(RestViewEditorState.SOURCE_PATHS);
    	if( pathsRaw != null ) {
    		tuples.remove(RestViewEditorState.SOURCE_PATHS);
    	}
    	tuples.put(RestViewEditorState.SOURCE_PATHS, sourcePaths);
    }
    
    /**
     * @return the string array of sql compositions(can be empty)
     */
    public RestSqlComposition[] getSqlCompositions() {
    	return compositions;
    }
    
    public void setSqlCompositions(RestSqlComposition[] compositions) {
        this.compositions = compositions;
    }
}
