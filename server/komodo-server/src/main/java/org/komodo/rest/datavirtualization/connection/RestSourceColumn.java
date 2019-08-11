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
package org.komodo.rest.datavirtualization.connection;

import org.teiid.metadata.Column;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * Represents the configuration for the schema table's column info 
 */
@JsonSerialize(as = RestSourceColumn.class)
@JsonInclude(Include.NON_NULL)
public class RestSourceColumn {
	
	/*
	 * The column name
	 */
	private String columnName;
	
	/*
	 * The column datatype
	 */
	private String datatype;

    /**
     * Constructor for use when deserializing
     */
    public RestSourceColumn(Column column) {
        super();
        this.columnName = column.getName();
        this.datatype = column.getRuntimeType();
    }

    public String getColumName() {
    	return this.columnName;
    }
    
    public String getDatatype() {
    	return this.datatype;
    }
}
