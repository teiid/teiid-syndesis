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
package org.komodo.openshift;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.komodo.StringConstants;

/**
 * Enum of all the Komodo types
 */
public enum KomodoType {

    /**
     * Dataservice
     */
    DATASERVICE("dataservice"), //$NON-NLS-1$

    /**
     * Connection
     */
    CONNECTION("connection"), //$NON-NLS-1$

    /**
     * Template
     */
    TEMPLATE("template"), //$NON-NLS-1$

    /**
     * Template Entry
     */
    TEMPLATE_ENTRY("templateEntry"), //$NON-NLS-1$

    /**
     * Schema
     */
    SCHEMA,

    /**
     * Vdb
     */
    VDB("vdb"), //$NON-NLS-1$

    /**
     * Vdb Entry
     */
    VDB_ENTRY("entry"), //$NON-NLS-1$

    /**
     * Vdb Import
     */
    VDB_IMPORT("import-vdb", "importVdb"), //$NON-NLS-1$ //$NON-NLS-2$

    /**
     * Vdb Model Source
     */
    VDB_MODEL_SOURCE("source"), //$NON-NLS-1$

    /**
     * Vdb Translator
     */
    VDB_TRANSLATOR("translator"), //$NON-NLS-1$

    /**
     * Vdb Condition
     */
    VDB_CONDITION("condition"), //$NON-NLS-1$

    /**
     * Vdb Data Role
     */
    VDB_DATA_ROLE("data-role", "dataRole"), //$NON-NLS-1$ //$NON-NLS-2$

    /**
     * Vdb Make
     */
    VDB_MASK("mask"), //$NON-NLS-1$

    /**
     * Vdb Permission
     */
    VDB_PERMISSION("permission"), //$NON-NLS-1$

    /**
     * Access Pattern
     */
    ACCESS_PATTERN,

    /**
     * Column
     */
    COLUMN,

    /**
     * Foreign Key
     */
    FOREIGN_KEY,

    /**
     * Pushdown Function
     */
    PUSHDOWN_FUNCTION,

    /**
     * UDF
     */
    USER_DEFINED_FUNCTION,

    /**
     * Index
     */
    INDEX,

    /**
     * Model
     */
    MODEL("model"), //$NON-NLS-1$

    /**
     * Paremeter
     */
    PARAMETER,

    /**
     * Primary Key
     */
    PRIMARY_KEY,

    /**
     * Profile
     */
    PROFILE,

    /**
     * Git Repository
     */
    GIT_REPOSITORY,

    /**
     * View Editor State
     */
    VIEW_EDITOR_STATE,

    /**
     * View Definition
     */
    VIEW_DEFINITION,
    
    /**
     * Sql Composition
     */
    SQL_COMPOSITION,
    
    /**
     * Sql Compositions
     */
    SQL_COMPOSITIONS,
    
    /**
     * Sql ProjectedColumn
     */
    SQL_PROJECTED_COLUMN,

    /**
     * View Editor State Command Aggregate
     */
    STATE_COMMAND_AGGREGATE,

    /**
     * View Editor State Command
     */
    STATE_COMMAND,

    /**
     * Stored Procedure
     */
    STORED_PROCEDURE,

    /**
     * Virtual Procedure
     */
    VIRTUAL_PROCEDURE,

    /**
     * Data type Result Set
     */
    DATA_TYPE_RESULT_SET,

    /**
     * Tabular Result Set Column
     */
    RESULT_SET_COLUMN,

    /**
     * Tabular Result Set
     */
    TABULAR_RESULT_SET,

    /**
     * Statement Option
     */
    STATEMENT_OPTION,

    /**
     * Table
     */
    TABLE,

    /**
     * Unique Constraint
     */
    UNIQUE_CONSTRAINT,

    /**
     * View
     */
    VIEW,

    /**
     * Type from the DDL schema
     */
    DDL_SCHEMA,

    /**
     * Type from the Teiid SQL schema
     */
    TSQL_SCHEMA,

    /**
     * Type from the VDB schema
     */
    VDB_SCHEMA,

    /**
     * Workspace type
     */
    WORKSPACE,

    /**
     * Server manager type
     */
    SERVER_MANAGER,

    /**
     * Unknown Type
     */
    UNKNOWN,

    /**
     * Type for a data service VDB entry.
     */
    VDB_DATA_SERVICE_ENTRY,

    /**
     * Type for a data service connection entry.
     */
    CONNECTION_ENTRY,

    /**
     * Data Source from Syndesis
     */
    SYNDESIS_DATA_SOURCE,

    /**
     * Build Status
     */
    BUILD_STATUS;

    private Collection<String> aliases;

    /**
     * Default constructor
     */
    private KomodoType() {
    }

    private KomodoType(String... aliases) {
        this();
        if (aliases != null) {
            this.aliases = new ArrayList<>(aliases.length);
            for (String alias : aliases) {
                this.aliases.add(alias);
            }
        }
    }

    /**
     * @return actual type
     */
    public String getType() {
        StringBuffer sb = new StringBuffer();
        for (String s : name().split(StringConstants.UNDERSCORE)) {
            sb.append(Character.toUpperCase(s.charAt(0)));
            if (s.length() > 1) {
                sb.append(s.substring(1, s.length()).toLowerCase());
            }
        }

        return sb.toString();
    }

    /**
     * @return the aliases
     */
    public Collection<String> getAliases() {
        if (this.aliases == null)
            return Collections.emptyList();

        return this.aliases;
    }

    @Override
    public String toString() {
        return getType();
    }

    /**
     * An empty array of types.
     */
    public static final KomodoType[] NO_TYPES = new KomodoType[0];

    /**
     * @param kType the string definition of a type
     * @return the {@link KomodoType} of the given string definition
     */
    public static KomodoType getKomodoType(String kType) {
        if (kType == null)
            return KomodoType.UNKNOWN;

        for (KomodoType value : values()) {
            if (value.getType().equalsIgnoreCase(kType))
                return value;

            if (value.getAliases().contains(kType))
                return value;
        }

        return KomodoType.UNKNOWN;
    }

    /**
     * @return all the string definitions of the types
     */
    public static List<String> getTypes() {
        List<String> names = new ArrayList<>();
        for (KomodoType kType : values()) {
            names.add(kType.getType());
        }

        return names;
    }
}
