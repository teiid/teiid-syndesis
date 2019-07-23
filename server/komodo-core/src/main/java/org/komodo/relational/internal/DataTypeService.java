package org.komodo.relational.internal;

import java.util.Set;

import org.komodo.spi.StringConstants;
import org.teiid.core.types.DataTypeManager;

/**
 *
 */
public class DataTypeService implements StringConstants {

    /**
     * The suffix designating an array
     */
    String ARRAY_SUFFIX = "[]"; //$NON-NLS-1$

    /**
     * Enumerator of data type names supported by the
     * teiid DataTypeManager
     */
    public enum DataTypeName {
        STRING,                         STRING_ARRAY,
        BOOLEAN,                     BOOLEAN_ARRAY,
        BYTE,                             BYTE_ARRAY,
        SHORT,                         SHORT_ARRAY,
        CHAR,                           CHAR_ARRAY,
        INTEGER,                      INTEGER_ARRAY,
        LONG,                           LONG_ARRAY,
        BIGINTEGER,              BIGINTEGER_ARRAY,
        FLOAT,                          FLOAT_ARRAY,
        DOUBLE,                       DOUBLE_ARRAY,
        BIGDECIMAL,              BIGDECIMAL_ARRAY,
        DATE,                            DATE_ARRAY,
        TIME,                            TIME_ARRAY,
        TIMESTAMP,                 TIMESTAMP_ARRAY,
        OBJECT,                         OBJECT_ARRAY,
        NULL,                            NULL_ARRAY,
        BLOB,                            BLOB_ARRAY,
        CLOB,                            CLOB_ARRAY,
        XML,                              XML_ARRAY,
        VARCHAR,                     VARCHAR_ARRAY,
        TINYINT,                       TINYINT_ARRAY,
        SMALLINT,                    SMALLINT_ARRAY,
        BIGINT,                         BIGINT_ARRAY,
        REAL,                             REAL_ARRAY,
        DECIMAL,                      DECIMAL_ARRAY,
        VARBINARY,                  VARBINARY_ARRAY;

        private static final String ARRAY_SUFFIX = "_ARRAY"; //$NON-NLS-1$

        /**
         * @return whether this is an array type
         */
        public boolean isArrayType() {
            return name().endsWith(ARRAY_SUFFIX);
        }

        /**
         * @return component type of this DataTypeName
         */
        public DataTypeName getComponentType() {
            if (isArrayType()) {
                String rootName = name().substring(0, name().indexOf(ARRAY_SUFFIX));
                return findDataTypeName(rootName);
            }

            return this;
        }

        /**
         * @return array type of this DataTypeName
         */
        public DataTypeName getArrayType() {
            if (isArrayType())
                return this;

            String arrayTypeName = name() + ARRAY_SUFFIX;
            return findDataTypeName(arrayTypeName);
        }

        /**
         * @param name
         * @return DataTypeName for the given name
         */
        public static DataTypeName findDataTypeName(String name) {
            if (name == null)
                return DataTypeName.NULL;

            name = correctBigUnderscores(name);
            name = name.toUpperCase();

            for (DataTypeName dtn : values()) {
                if (dtn.name().equals(name))
                    return dtn;
            }

            return null;
        }

        public String getId() {
            return name().toLowerCase();
        }

        public static String correctBigUnderscores(String name) {
            if (name == null)
                return null;

            //
            // At some point, its possible that existing data types have been
            // stored using underscores for BIG_DECIMAL and BIG_INTEGER.
            // These will no longer be found so need to mitigate.
            //
            return name.toLowerCase().replace("big_", "big");
        }
    }

    public enum DataTypeAliases {
        VARCHAR("varchar"), //$NON-NLS-1$

        TINYINT("tinyint"), //$NON-NLS-1$

        SMALLINT("smallint"), //$NON-NLS-1$

        BIGINT("bigint"), //$NON-NLS-1$

        REAL("real"), //$NON-NLS-1$

        DECIMAL("decimal"); //$NON-NLS-1$
        
        private String id;

        /**
         * 
         */
        private DataTypeAliases(String id) {
            this.id = id;
        }

        /**
         * @return the id
         */
        public String getId() {
            return this.id;
        }
    }

    /**
     * Types of data source supported by teiid instances
     */
    public enum DataSourceTypes {
        JDBC("connector-jdbc"), //$NON-NLS-1$

        SALESFORCE("salesforce"), //$NON-NLS-1$

        LDAP("ldap"), //$NON-NLS-1$ 

        FILE("file"), //$NON-NLS-1$ 

        MONGODB("mongodb"), //$NON-NLS-1$ 

        JDBC_XA("connector-jdbc-xa"), //$NON-NLS-1$

        WS("webservice"), //$NON-NLS-1$

        UNKNOWN("connector-unknown"); //$NON-NLS-1$

        private String id;

        DataSourceTypes(String id) {
            this.id = id;
        }

        public String id() {
            return this.id;
        }
    }

    /**
     * Get the teiid instance specific name of the data source type
     *  
     * @param dataSourceType
     * 
     * @return data source type name
     */
    public String getDataSourceType(DataSourceTypes dataSourceType) {
        if (dataSourceType == null)
            return DataSourceTypes.UNKNOWN.id();

        return dataSourceType.id();
    }

    /**
     * Get the data type class with the given name.
     * 
     * @param name
     *      Data type name
     *      
     * @return Data type class
     */
    public Class<?> getDataTypeClass(String name) {
        return DataTypeManager.getDataTypeClass(name);
    }

    /**
     * Get the runtime data type name for the given data type id
     *
     * @param dataTypeId
     *
     * @return runtime type name or null
     */
    public DataTypeName getDataTypeName(String dataTypeId) {        
        if (dataTypeId == null)
            return DataTypeName.NULL;

        dataTypeId = DataTypeName.correctBigUnderscores(dataTypeId);

        // Should eliminate any aliases
        Class<?> dataTypeClass = getDataTypeClass(dataTypeId);
        dataTypeId = DataTypeManager.getDataTypeName(dataTypeClass);

        boolean isArray = isArrayType(dataTypeId);

        if (isArray)
            dataTypeId = getComponentType(dataTypeId);

        DataTypeName dataType = DataTypeName.findDataTypeName(dataTypeId);
        if (dataType == null)
            dataType = DataTypeName.OBJECT;

        if (isArray)
            return dataType.getArrayType();
        else
            return dataType;
    }

    /**
     * Get the runtime type for the given class as a String
     *  
     * @param typeClass
     * 
     * @return runtime type
     */
    public String getDataTypeName(Class<?> typeClass) {
        return DataTypeManager.getDataTypeName(typeClass);
    }

    /**
     * Retrieve the runtime type for the given class as a {@link DataTypeName}
     *  
     * @param typeClass
     * 
     * @return runtime type as a {@link DataTypeName}
     */
    public DataTypeName retrieveDataTypeName(Class<?> typeClass) {
        String typeName = getDataTypeName(typeClass);
        return DataTypeName.findDataTypeName(typeName);
    }

    /**
     * Get a set of all data type names.
     * 
     * @return Set of data type names (String)
     */
    public Set<String> getAllDataTypeNames() {
    	return DataTypeManager.getAllDataTypeNames();
    }
    
    /**
     * Get the default data class represented by the 
     * given {@link DataTypeName} enumerator
     * 
     * @param dataTypeName
     * 
     * @return class of data type or will throw a runtime exception
     *                if there is no data type.
     */
    public Class<?> getDefaultDataClass(DataTypeName dataTypeName) {
        if (dataTypeName == null)
            return getDataTypeClass(null);

        return getDataTypeClass(dataTypeName.name());
    }
    
    /**
     * @param type
     * @return whether this is a LOB-based class
     */
    public boolean isLOB(Class<?> type) {
        return DataTypeManager.isLOB(type);
    }
    
    protected boolean isArrayType(String name) {
        return name.endsWith(ARRAY_SUFFIX);
    }

    protected String getComponentType(String name) {
        return name.substring(0, name.lastIndexOf(ARRAY_SUFFIX));
    }

}
