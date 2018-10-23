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
 * 021101301 USA.
 */
package org.komodo.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.komodo.spi.type.DataTypeService;
import org.komodo.spi.type.DataTypeService.DataSourceTypes;
import org.komodo.spi.type.DataTypeService.DataTypeName;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestDataTypeManager extends AbstractMetadataInstanceTests {

    private DataTypeService manager;
    @Before
    public void setup() throws Exception {
        manager = getMetadataInstance().getDataTypeService();
    }

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
