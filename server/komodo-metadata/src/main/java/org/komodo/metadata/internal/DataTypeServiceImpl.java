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
package org.komodo.metadata.internal;

import java.util.Set;
import org.komodo.spi.runtime.version.MetadataVersion;
import org.komodo.spi.type.DataTypeService;
import org.teiid.core.types.DataTypeManager;

public class DataTypeServiceImpl implements DataTypeService {

    private final MetadataVersion teiidVersion;

    public DataTypeServiceImpl(MetadataVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
    }

    /**
     * @return version
     */
    public MetadataVersion getVersion() {
        return teiidVersion;
    }

    protected boolean isArrayType(String name) {
        return name.endsWith(ARRAY_SUFFIX);
    }

    protected String getComponentType(String name) {
        return name.substring(0, name.lastIndexOf(ARRAY_SUFFIX));
    }

    @Override
    public String getDataSourceType(DataSourceTypes dataSourceType) {
        if (dataSourceType == null)
            return DataSourceTypes.UNKNOWN.id();

        return dataSourceType.id();
    }

    @Override
    public <T> T transformValue(Object value, DataTypeName dataTypeName) throws Exception {
        Class<DataTypeName> typeClass = dataTypeName.getDeclaringClass();
        return transformValue(value, typeClass);
    }

    @Override
    public Class<?> getDataTypeClass(String name) {
        return DataTypeManager.getDataTypeClass(name);
    }

    @Override
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

    @Override
    public String getDataTypeName(Class<?> typeClass) {
        return DataTypeManager.getDataTypeName(typeClass);
    }

    @Override
    public DataTypeName retrieveDataTypeName(Class<?> typeClass) {
        String typeName = getDataTypeName(typeClass);
        return DataTypeName.findDataTypeName(typeName);
    }

    @Override
    public Set<String> getAllDataTypeNames() {
        return DataTypeManager.getAllDataTypeNames();
    }

    @Override
    public Class<?> getDefaultDataClass(DataTypeName dataTypeName) {
        if (dataTypeName == null)
            return getDataTypeClass(null);

        return getDataTypeClass(dataTypeName.name());
    }

    @Override
    public boolean isExplicitConversion(String sourceTypeName, String targetTypeName) {
        return DataTypeManager.isExplicitConversion(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isImplicitConversion(String sourceTypeName, String targetTypeName) {
        return DataTypeManager.isImplicitConversion(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isTransformable(String sourceTypeName, String targetTypeName) {
        return DataTypeManager.isTransformable(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isLOB(Class<?> type) {
        return DataTypeManager.isLOB(type);
    }

    @SuppressWarnings( "unchecked" )
    @Override
    public <T> T transformValue(Object value, Class<?> typeClass) throws Exception {
        return (T) DataTypeManager.transformValue(value, typeClass);
    }
}
