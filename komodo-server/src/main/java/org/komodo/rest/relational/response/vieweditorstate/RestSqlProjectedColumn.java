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
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.rest.RestBasicEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

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
     * @param uow transaction
     * @throws KException if error
     */
    public RestSqlProjectedColumn(URI baseUri, SqlProjectedColumn sqlProjectedColumn, UnitOfWork uow) throws KException {
        super(baseUri);
        
        String value = sqlProjectedColumn.getName(uow);
        this.setName(value);
        
        value = sqlProjectedColumn.getType(uow);
        this.setType(value);
        
        boolean bVal = sqlProjectedColumn.isSelected(uow);
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