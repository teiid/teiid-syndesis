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

import org.komodo.relational.dataservice.SqlComposition;
import org.komodo.rest.RestBasicEntity;
import org.komodo.spi.KException;

public class RestSqlComposition extends RestBasicEntity {
	
    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestSqlComposition() {
    	
    }
    
    public RestSqlComposition(final RestSqlComposition sqlComp) {
    	String value = sqlComp.getId();
        if( value != null ) {
        	tuples.put(RestViewEditorState.ID_NAME, value);
        }
        
        value = sqlComp.getDescription();
        if( value != null ) {
        	tuples.put(RestViewEditorState.DESCRIPTION, value);
        }
        
        value = sqlComp.getRightSourcePath();
        if( value != null ) {
        	tuples.put(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL, value);
        }
        
        value = sqlComp.getLeftSourcePath();
        if( value != null ) {
        	tuples.put(RestViewEditorState.LEFT_SOURCE_PATH_LABEL, value);
        }
        
        value = sqlComp.getRightCriteriaColumn();
        if( value != null ) {
        	tuples.put(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL, value);
        }
        
        value = sqlComp.getLeftCriteriaColumn();
        if( value != null ) {
        	tuples.put(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL, value);
        }
        
        value = sqlComp.getType();
        if( value != null ) {
        	tuples.put(RestViewEditorState.TYPE_LABEL, value);
        }
        
        value = sqlComp.getOperator();
        if( value != null ) {
        	tuples.put(RestViewEditorState.OPERATOR_LABEL, value);
        }
    }
    
    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestSqlComposition(String name,
    						  String description,
    						  String leftSrc,
    						  String rightSrc,
    						  String leftColumn,
    						  String rightColumn,
    						  String type,
    						  String operator) {
        if( name != null ) {
        	tuples.put(RestViewEditorState.ID_NAME, name);
        }
        if( description != null ) {
        	tuples.put(RestViewEditorState.DESCRIPTION, description);
        }
        if( rightSrc != null ) {
        	tuples.put(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL, rightSrc);
        }
        if( leftSrc != null ) {
        	tuples.put(RestViewEditorState.LEFT_SOURCE_PATH_LABEL, leftSrc);
        }
        if( rightColumn != null ) {
        	tuples.put(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL, rightColumn);
        }
        if( leftColumn != null ) {
        	tuples.put(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL, leftColumn);
        }
        if( type != null ) {
        	tuples.put(RestViewEditorState.TYPE_LABEL, type);
        }
        if( operator != null ) {
        	tuples.put(RestViewEditorState.OPERATOR_LABEL, operator);
        }
    }

    /**
     * Constructor for those connections needing more control over what basic properties
     * should be set
     *
     * @param baseUri
     * @throws KException
     */
    public RestSqlComposition(URI baseUri, SqlComposition sqlCompostion) throws KException {
        super(baseUri);
        
        String value = sqlCompostion.getName();
        if( value != null ) {
        	tuples.put(RestViewEditorState.ID_NAME, value);
        }
        
        value = sqlCompostion.getDescription();
        if( value != null ) {
        	tuples.put(RestViewEditorState.DESCRIPTION, value);
        }
        
        value = sqlCompostion.getRightSourcePath();
        if( value != null ) {
        	tuples.put(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL, value);
        }
        
        value = sqlCompostion.getLeftSourcePath();
        if( value != null ) {
        	tuples.put(RestViewEditorState.LEFT_SOURCE_PATH_LABEL, value);
        }
        
        value = sqlCompostion.getRightCriteriaColumn();
        if( value != null ) {
        	tuples.put(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL, value);
        }
        
        value = sqlCompostion.getLeftCriteriaColumn();
        if( value != null ) {
        	tuples.put(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL, value);
        }
        
        value = sqlCompostion.getType();
        if( value != null ) {
        	tuples.put(RestViewEditorState.TYPE_LABEL, value);
        }
        
        value = sqlCompostion.getOperator();
        if( value != null ) {
        	tuples.put(RestViewEditorState.OPERATOR_LABEL, value);
        }
    }
    
    
    /**
     * @param name value
     *        the new name (can be empty)
     */
    public void setName(final String name) {
        tuples.put(RestViewEditorState.ID, name);
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        Object value = tuples.get(RestViewEditorState.ID);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @param descr value
     *        the new description (can be empty)
     */
    public void setDescription(final String descr) {
        tuples.put(RestViewEditorState.DESCRIPTION, descr);
    }

    /**
     * @return the left node path (can be empty)
     */
    public String getDescription() {
        Object value = tuples.get(RestViewEditorState.DESCRIPTION);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @param path value
     *        the new left source path (can be empty)
     */
    public void setLeftSourcePath(final String path) {
        tuples.put(RestViewEditorState.LEFT_SOURCE_PATH_LABEL, path);
    }

    /**
     * @return the left source path (can be empty)
     */
    public String getLeftSourcePath() {
        Object value = tuples.get(RestViewEditorState.LEFT_SOURCE_PATH_LABEL);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @param path value
     *        the new right source path (can be empty)
     */
    public void setRightSourcePath(final String path) {
        tuples.put(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL, path);
    }

    /**
     * @return the right source path (can be empty)
     */
    public String getRightSourcePath() {
        Object value = tuples.get(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param column value
     *        the new right criteria column (can be empty)
     */
    public void setRightCriteriaColumn(final String column) {
        tuples.put(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL, column);
    }

    /**
     * @return the right node path (can be empty)
     */
    public String getRightCriteriaColumn() {
        Object value = tuples.get(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @param column value
     *        the new left criteria column (can be empty)
     */
    public void setLeftCriteriaColumn(final String column) {
        tuples.put(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL, column);
    }

    /**
     * @return the left node path (can be empty)
     */
    public String getLeftCriteriaColumn() {
        Object value = tuples.get(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param type value
     *        the new type (can be empty)
     */
    public void setType(final String type) {
        tuples.put(RestViewEditorState.TYPE_LABEL, type);
    }

    /**
     * @return the composition type (can be empty)
     */
    public String getType() {
        Object value = tuples.get(RestViewEditorState.TYPE_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param operator value
     *        the new operator (can be empty)
     */
    public void setOperator(final String operator) {
        tuples.put(RestViewEditorState.OPERATOR_LABEL, operator);
    }

    /**
     * @return the operator type (can be empty)
     */
    public String getOperator() {
        Object value = tuples.get(RestViewEditorState.OPERATOR_LABEL);
        return value != null ? value.toString() : null;
    }
}