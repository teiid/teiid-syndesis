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
import java.util.List;

import org.teiid.metadata.Schema;
import org.teiid.metadata.Table;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * Represents the configuration for a source schema
 */
@JsonSerialize(as = RestSourceSchema.class)
@JsonInclude(Include.NON_NULL)
public class RestSourceSchema {
	
    /*
     * The schema name
     */
    private String schemaName;
    
    /*
     * The source path to the tables in this schema
     */
    private String sourcePath;

    /*
     * The source table objects for this schema
     */
    private RestSourceTable[] sourceTables;

    /**
     * Constructor for use when deserializing
     */
    public RestSourceSchema(String sourcePath, Schema schema) {
        super();
        this.sourcePath = sourcePath;
    	List<RestSourceTable> tables = new ArrayList<RestSourceTable>();
    	for( String key : schema.getTables().keySet()) {
    		Table nextTable = schema.getTables().get(key);
    		tables.add(new RestSourceTable(this.sourcePath, nextTable));
    	}
    	this.sourceTables = tables.toArray(new RestSourceTable[0]);
    }

    public String getSchemaName() {
    	return this.schemaName;
    }
    
    public RestSourceTable[] getSourceTables() {
    	return this.sourceTables;
    }
    
}
