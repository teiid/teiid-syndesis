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

import org.komodo.relational.profile.SqlComposition;
import org.komodo.rest.RestBasicEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

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
    public RestSqlComposition(URI baseUri, SqlComposition sqlCompostion, UnitOfWork uow) throws KException {
        super(baseUri);
        
        String value = sqlCompostion.getName(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.ID_NAME, value);
        }
        
        value = sqlCompostion.getDescription(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.DESCRIPTION, value);
        }
        
        value = sqlCompostion.getRightSourcePath(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL, value);
        }
        
        value = sqlCompostion.getLeftSourcePath(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.LEFT_SOURCE_PATH_LABEL, value);
        }
        
        value = sqlCompostion.getRightCriteriaColumn(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL, value);
        }
        
        value = sqlCompostion.getLeftCriteriaColumn(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL, value);
        }
        
        value = sqlCompostion.getType(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.TYPE_LABEL, value);
        }
        
        value = sqlCompostion.getOperator(uow);
        if( value != null ) {
        	tuples.put(RestViewEditorState.OPERATOR_LABEL, value);
        }
    }
    
    
    /**
     * @return the left node path (can be empty)
     */
    public String getDescription() {
        Object value = tuples.get(RestViewEditorState.DESCRIPTION);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @return the left source path (can be empty)
     */
    public String getLeftSourcePath() {
        Object value = tuples.get(RestViewEditorState.LEFT_SOURCE_PATH_LABEL);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @return the right source path (can be empty)
     */
    public String getRightSourcePath() {
        Object value = tuples.get(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @return the right node path (can be empty)
     */
    public String getRightCriteriaColumn() {
        Object value = tuples.get(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @return the left node path (can be empty)
     */
    public String getLeftCriteriaColumn() {
        Object value = tuples.get(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @return the composition type (can be empty)
     */
    public String getType() {
        Object value = tuples.get(RestViewEditorState.TYPE_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @return the operator type (can be empty)
     */
    public String getOperator() {
        Object value = tuples.get(RestViewEditorState.OPERATOR_LABEL);
        return value != null ? value.toString() : null;
    }
}