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
package org.komodo.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultMetadataVersion;
import org.komodo.test.utils.TestUtilities;
import org.teiid.adminapi.AdminProcessingException;
import org.teiid.core.util.ApplicationInfo;

@Ignore
@RunWith( Arquillian.class )
public class TestMetadataServer {

	private MetadataInstance getMetadataInstance() {
		// TODO:
		return null;
	}
	
    @Test
    public void testVersion() throws Exception {
        ApplicationInfo info = ApplicationInfo.getInstance();
        assertEquals(new DefaultMetadataVersion(info.getReleaseNumber()), getMetadataInstance().getVersion());
    }

    @Test
    public void testCreateDataSource() throws Exception {
        String displayName = "h2-connector";
        String type = "h2";
        String dsName = "accounts-ds";
        String jndiName = "java:/accounts-ds";
        String connUrl = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1";

        if (getMetadataInstance().dataSourceExists(dsName)) {
            getMetadataInstance().deleteDataSource(dsName);
        }

        Properties properties = new Properties();
        properties.setProperty(TeiidDataSource.DATASOURCE_JNDINAME, jndiName);
        properties.setProperty(TeiidDataSource.DATASOURCE_CONNECTION_URL, connUrl);

        TeiidDataSource accountsDS = getMetadataInstance().getOrCreateDataSource(displayName, dsName, type, properties);
        assertNotNull(accountsDS);

        assertEquals(dsName, accountsDS.getName());
        assertEquals(type, accountsDS.getType());
        assertEquals(jndiName, accountsDS.getPropertyValue(TeiidDataSource.DATASOURCE_JNDINAME));
        assertEquals(connUrl, accountsDS.getPropertyValue(TeiidDataSource.DATASOURCE_CONNECTION_URL));

        getMetadataInstance().deleteDataSource(dsName);
        assertFalse(getMetadataInstance().dataSourceExists(dsName));
    }

    @Test
    public void testDeployment() throws Exception {
        getMetadataInstance().deployDynamicVdb(TestUtilities.SAMPLE_VDB_FILE, TestUtilities.sampleExample());
        Thread.sleep(2000);

        Collection<TeiidVdb> vdbs = getMetadataInstance().getVdbs();
        assertEquals(1, vdbs.size());

        TeiidVdb vdb = getMetadataInstance().getVdb(TestUtilities.SAMPLE_VDB_NAME);
        assertNotNull(vdb);
        assertEquals(0, vdb.getValidityErrors().size());

        assertTrue(vdb.isActive());

        getMetadataInstance().undeployDynamicVdb(TestUtilities.SAMPLE_VDB_NAME);
        vdbs = getMetadataInstance().getVdbs();
        assertEquals(0, vdbs.size());
    }

    @Test
    public void testGetSchema() throws Exception {
        try {
            getMetadataInstance().getSchema("blah", "1.0", "model");
        } catch (Exception ex) {
            //
            // Should throw this exception since blah does not exist but should not
            // throw a NumberFormatException or NoSuchMethodException
            //
            Throwable cause = ex.getCause();
            assertNotNull(cause);
            assertTrue(cause instanceof AdminProcessingException);
            assertTrue(cause.getMessage().contains("does not exist or is not ACTIVE"));
        }
    }

    @Test
    public void testDataSourceDrivers() throws Exception {
        Collection<ConnectionDriver> dataSourceDrivers = getMetadataInstance().getDataSourceDrivers();
        assertTrue(dataSourceDrivers.size() > 0);

        String[] driverNamesArr = {"h2", "teiid-local", "teiid"};
        List<String> driverNames = Arrays.asList(driverNamesArr);

        Iterator<ConnectionDriver> iter = dataSourceDrivers.iterator();
        int found = 0;
        while (iter.hasNext()) {
            ConnectionDriver driver = iter.next();
            if (driverNames.contains(driver.getName()))
                found++;
        }

        assertEquals(driverNamesArr.length, found);
    }

}
