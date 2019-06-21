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
package org.komodo.rest.relational.response.vieweditorstate;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.rest.RestBasicEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Rest ViewDefinition
 */
public class RestViewDefinition extends RestBasicEntity {
	
    /*
     * The view compositions
     */
    private RestSqlComposition[] compositions = new RestSqlComposition[0];
    /*
     * The view projected columns
     */
    private RestSqlProjectedColumn[] projectedColumns = new RestSqlProjectedColumn[0];
    
    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestViewDefinition() {
        // nothing to do
    }
    
    /**
     * Constructor
     * @param viewDef the view definition
     * @throws KException the exception
     */
    public RestViewDefinition(final RestViewDefinition viewDef) throws KException {
    	super(viewDef.getBaseUri());
    	
    	String name = viewDef.getViewName();
        if( name != null ) {
        	tuples.put(RestViewEditorState.ID_VIEW_NAME, name);
        }
        if( viewDef.getDescription() != null ) {
        	tuples.put(RestViewEditorState.DESCRIPTION, viewDef.getDescription());
        }
        if( viewDef.getDdl() != null ) {
        	tuples.put(RestViewEditorState.DDL, viewDef.getDdl());
        }
        
        setComplete(viewDef.isComplete());
        setUserDefined(viewDef.isUserDefined());
        
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
        
        List<RestSqlProjectedColumn> columnList = new ArrayList<>();
        for (RestSqlProjectedColumn col : viewDef.getProjectedColumns()) {
            RestSqlProjectedColumn restCol = new RestSqlProjectedColumn(col);
            columnList.add(restCol);
        }
        this.projectedColumns = columnList.toArray(new RestSqlProjectedColumn[0]);
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
        if( viewDef.getDdl(transaction) != null ) {
        	tuples.put(RestViewEditorState.DDL, viewDef.getDdl(transaction));
        }
        
        setComplete(viewDef.isComplete(transaction));
        setUserDefined(viewDef.isUserDefined(transaction));
        
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
        
        List<RestSqlProjectedColumn> columnList = new ArrayList<>();
        for (SqlProjectedColumn col : viewDef.getProjectedColumns(transaction)) {
            RestSqlProjectedColumn restCol = new RestSqlProjectedColumn(baseUri, col, transaction);
            columnList.add(restCol);
        }
        this.projectedColumns = columnList.toArray(new RestSqlProjectedColumn[0]);
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
     * @return the view DDL (can be empty)
     */
    public String getDdl() {
        Object result = tuples.get(RestViewEditorState.DDL);
        return result != null ? result.toString() : null;
    }

    /**
     * @param ddl
     *        the new DDL (can be empty)
     */
    public void setDdl(final String ddl) {
        tuples.put(RestViewEditorState.DDL, ddl);
    }
    
    /**
     * @return the view definition isComplete status
     */
    public boolean isComplete() {
        Object hasIsComplete = tuples.get(RestViewEditorState.IS_COMPLETE);
        return hasIsComplete != null ? Boolean.parseBoolean(hasIsComplete.toString()) : false;
    }

    /**
     * @param complete 
     *        the complete status
     */
    public void setComplete(final boolean complete) {
        tuples.put(RestViewEditorState.IS_COMPLETE, complete);
    }

    /**
     * @return the view definition isUserDefined status
     */
    public boolean isUserDefined() {
        Object hasIsUserDefined = tuples.get(RestViewEditorState.IS_USER_DEFINED);
        return hasIsUserDefined != null ? Boolean.parseBoolean(hasIsUserDefined.toString()) : false;
    }

    /**
     * @param userDefined 
     *        the userDefined status
     */
    public void setUserDefined(final boolean userDefined) {
        tuples.put(RestViewEditorState.IS_USER_DEFINED, userDefined);
    }
    
    /**
     * @return the string array of source table paths(can be empty)
     */
    public String[] getSourcePaths() {
    	Object[] pathsRaw = (Object[])tuples.get(RestViewEditorState.SOURCE_PATHS);
    	int pathsLength = (pathsRaw != null) ? pathsRaw.length : 0;
    	String[] pathsString = new String[pathsLength];
    	if (pathsLength > 0) {
        	int i = 0;
        	for( Object obj : pathsRaw ) {
        		pathsString[i++] = (String)obj;
        	}
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
    
    /**
     * Set the compositions
     * @param compositions the compositions
     */
    public void setSqlCompositions(RestSqlComposition[] compositions) {
        this.compositions = compositions;
    }

    /**
     * @return the projected columns
     */
    public RestSqlProjectedColumn[] getProjectedColumns() {
        return projectedColumns;
    }
    
    /**
     * Set the projected columns
     * @param projCols the projected columns
     */
    public void setProjectedColumns(RestSqlProjectedColumn[] projCols) {
        this.projectedColumns = projCols;
    }
}
