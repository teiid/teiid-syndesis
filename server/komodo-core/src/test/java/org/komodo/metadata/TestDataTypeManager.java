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
package org.komodo.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Set;

import org.junit.Test;
import org.komodo.relational.internal.DataTypeService;
import org.komodo.relational.internal.DataTypeService.DataSourceTypes;
import org.komodo.relational.internal.DataTypeService.DataTypeName;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestDataTypeManager {

    private DataTypeService manager = new DataTypeService();

    @Test
    public void testGetDataType() throws Exception {
        assertEquals(DataTypeName.NULL, manager.getDataTypeName((String)null));

        assertEquals(DataTypeName.STRING, manager.getDataTypeName("string"));
        assertEquals(DataTypeName.STRING, manager.getDataTypeName("STRING"));
        assertEquals(DataTypeName.STRING_ARRAY, manager.getDataTypeName("String[]"));

        assertEquals(DataTypeName.BIGDECIMAL, manager.getDataTypeName("bigdecimal"));
        assertEquals(DataTypeName.BIGDECIMAL, manager.getDataTypeName("BIG_DECIMAL"));
        assertEquals(DataTypeName.BIGDECIMAL_ARRAY, manager.getDataTypeName("BIG_DECIMAL[]"));

        assertEquals(DataTypeName.OBJECT, manager.getDataTypeName("NoSuchObject"));
    }

    @Test
    public void testGetDataTypeClass() throws Exception {
        assertEquals("NullType", manager.getDataTypeClass((String)null).getSimpleName());

        assertSame(String.class, manager.getDataTypeClass("string"));
        assertSame(String.class, manager.getDataTypeClass("STRING"));
        assertSame(String[].class, manager.getDataTypeClass("String[]"));

        assertSame(Object.class, manager.getDataTypeClass("NoSuchObject"));
    }

    @Test
    public void testGetDefaultDataTypeClass() throws Exception {
        assertEquals("NullType", manager.getDefaultDataClass(null).getSimpleName());
        assertSame(String.class, manager.getDefaultDataClass(DataTypeName.STRING));
        assertSame(Object.class, manager.getDefaultDataClass(DataTypeName.OBJECT));
    }

    @Test
    public void testGetDataType4Class() throws Exception {
        assertEquals(DataTypeName.LONG.getId(), manager.getDataTypeName(Long.class));
        assertEquals(DataTypeName.STRING.getId(), manager.getDataTypeName(String.class));
    }

    @Test
    public void testGetDataSourceType() throws Exception {
        assertSame(DataSourceTypes.JDBC.id(), manager.getDataSourceType(DataSourceTypes.JDBC));
        assertSame(DataSourceTypes.UNKNOWN.id(), manager.getDataSourceType(DataSourceTypes.UNKNOWN));
    }

    @Test
    public void testGetDataType1() throws Exception {
        assertSame(DataTypeName.VARBINARY, manager.getDataTypeName("varbinary"));
    }

    @Test
    public void testGetDataTypeClass1() throws Exception {
        assertEquals("BinaryType", manager.getDataTypeClass("varbinary").getSimpleName());
    }

    @Test
    public void testGetDefaultDataTypeClass1() throws Exception {
        assertEquals("BinaryType", manager.getDefaultDataClass(DataTypeName.VARBINARY).getSimpleName());
    }

    @Test
    public void testGetAllDataTypeNames() throws Exception {
        Set<String> names = manager.getAllDataTypeNames();
        assertTrue(!names.isEmpty());
        assertTrue(names.contains(DataTypeName.BIGDECIMAL.getId()));
        assertTrue(names.contains(DataTypeName.STRING.getId()));
        assertTrue(names.contains(DataTypeName.VARBINARY.getId()));
    }

    @Test
    public void testGetDataSourceType1() throws Exception {
        assertEquals(DataSourceTypes.SALESFORCE.id(), manager.getDataSourceType(DataSourceTypes.SALESFORCE));
        assertEquals(DataSourceTypes.LDAP.id(), manager.getDataSourceType(DataSourceTypes.LDAP));
        assertEquals(DataSourceTypes.FILE.id(), manager.getDataSourceType(DataSourceTypes.FILE));
        assertEquals(DataSourceTypes.WS.id(), manager.getDataSourceType(DataSourceTypes.WS));
    }
}
