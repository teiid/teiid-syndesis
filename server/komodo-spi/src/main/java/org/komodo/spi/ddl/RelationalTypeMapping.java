/*************************************************************************************
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
package org.komodo.spi.ddl;




/**
 * This class provides a mapping between the built-in types and the JDBC types.
 */
public interface RelationalTypeMapping {

    @SuppressWarnings("javadoc")
	public static class SQL_TYPE_NAMES {
        public static final String ARRAY = "ARRAY"; //$NON-NLS-1$ // NO_UCD
        public static final String BIGINT = "BIGINT"; //$NON-NLS-1$
        public static final String BINARY = "BINARY"; //$NON-NLS-1$
        public static final String BIT = "BIT"; //$NON-NLS-1$
        public static final String BLOB = "BLOB"; //$NON-NLS-1$
        public static final String CHAR = "CHAR"; //$NON-NLS-1$
        public static final String CLOB = "CLOB"; //$NON-NLS-1$
        public static final String DATE = "DATE"; //$NON-NLS-1$
        public static final String DECIMAL = "DECIMAL"; //$NON-NLS-1$
        public static final String DISTINCT = "DISTINCT"; //$NON-NLS-1$ // NO_UCD
        public static final String DOUBLE = "DOUBLE"; //$NON-NLS-1$
        public static final String FLOAT = "FLOAT"; //$NON-NLS-1$
        public static final String INTEGER = "INTEGER"; //$NON-NLS-1$
        public static final String JAVA_OBJECT = "JAVA_OBJECT"; //$NON-NLS-1$ // NO_UCD
        public static final String LONGVARBINARY = "LONGVARBINARY"; //$NON-NLS-1$
        public static final String LONGVARCHAR = "LONGVARCHAR"; //$NON-NLS-1$
        public static final String NCHAR = "NCHAR"; //$NON-NLS-1$
        public static final String NVARCHAR = "NVARCHAR"; //$NON-NLS-1$
        public static final String NTEXT = "NTEXT"; //$NON-NLS-1$
        public static final String NULL = "NULL"; //$NON-NLS-1$ // NO_UCD
        public static final String NUMERIC = "NUMERIC"; //$NON-NLS-1$
        public static final String OTHER = "OTHER"; //$NON-NLS-1$
        public static final String REAL = "REAL"; //$NON-NLS-1$
        public static final String REF = "REF"; //$NON-NLS-1$
        public static final String SMALLINT = "SMALLINT"; //$NON-NLS-1$
        public static final String SQLXML = "SQLXML"; //$NON-NLS-1$
        public static final String STRUCT = "STRUCT"; //$NON-NLS-1$
        public static final String TIME = "TIME"; //$NON-NLS-1$
        public static final String TIMESTAMP = "TIMESTAMP"; //$NON-NLS-1$
        public static final String TINYINT = "TINYINT"; //$NON-NLS-1$
        public static final String VARBINARY = "VARBINARY"; //$NON-NLS-1$
        public static final String VARCHAR = "VARCHAR"; //$NON-NLS-1$
    }

}
