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

import org.komodo.relational.dataservice.SqlProjectedColumn;
import org.komodo.rest.RestBasicEntity;
import org.komodo.spi.KException;

/**
 * Rest object for a projected column
 */
public class RestSqlProjectedColumn extends RestBasicEntity {
	
    /**
     * Label used for column name
     */
    public static final String NAME_LABEL = "name"; //$NON-NLS-1$
    
    /**
     * label used for column type
     */
    public static final String TYPE_LABEL = "type"; //$NON-NLS-1$

    /**
     * label used for column selected
     */
    public static final String SELECTED_LABEL = "selected"; //$NON-NLS-1$

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestSqlProjectedColumn() {
    	
    }
    
    /**
     * Constructor
     * @param projCol the column
     */
    public RestSqlProjectedColumn(final RestSqlProjectedColumn projCol) {
        this.setName(projCol.getName());
        this.setType(projCol.getType());
        this.setSelected(projCol.isSelected());
    }
    
    /**
     * Constructor
     * @param baseUri base uri
     * @param sqlProjectedColumn the projected column
     * @throws KException if error
     */
    public RestSqlProjectedColumn(URI baseUri, SqlProjectedColumn sqlProjectedColumn) throws KException {
        super(baseUri);
        
        String value = sqlProjectedColumn.getName();
        this.setName(value);
        
        value = sqlProjectedColumn.getType();
        this.setType(value);
        
        boolean bVal = sqlProjectedColumn.isSelected();
        this.setSelected(bVal);
    }

    /**
     * Constructor
     * @param name the column name
     * @param type the column type
     * @param selected the column selection state
     */
    public RestSqlProjectedColumn(String name,
    						      String type,
      						      boolean selected) {
        this.setName(name);
        this.setType(type);
        this.setSelected(selected);
    }

    /**
     * @param name value
     *        the new name (can be empty)
     */
    public void setName(final String name) {
        tuples.put(NAME_LABEL, name);
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        Object value = tuples.get(NAME_LABEL);
        return value != null ? value.toString() : null;
    }
    
    /**
     * @param type value
     *        the new type (can be empty)
     */
    public void setType(final String type) {
        tuples.put(TYPE_LABEL, type);
    }

    /**
     * @return the column type (can be empty)
     */
    public String getType() {
        Object value = tuples.get(TYPE_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @return selected flag
     */
    public boolean isSelected() {
        Object value = tuples.get(SELECTED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param selected 'true' if selected
     */
    public void setSelected(boolean selected) {
        tuples.put(SELECTED_LABEL, selected);
    }

}