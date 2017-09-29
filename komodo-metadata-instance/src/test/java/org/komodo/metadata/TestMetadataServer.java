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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.Properties;
import org.junit.Test;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultMetadataVersion;
import org.komodo.test.utils.TestUtilities;
import org.teiid.core.util.ApplicationInfo;

public class TestMetadataServer extends AbstractMetadataInstanceTests {

    @Test
    public void shouldGetVersion() throws Exception {
        ApplicationInfo info = ApplicationInfo.getInstance();
        assertEquals(new DefaultMetadataVersion(info.getReleaseNumber()), getMetadataInstance().getVersion());
    }

//
// TODO
// FIXME
//
//    @Test
//    public void testCreateDataSource() throws Exception {
//        String displayName = "h2-connector";
//        String type = "h2";
//        String dsName = "accounts-ds";
//        String jndiName = "java:/accounts-ds";
//        String connUrl = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1";
//
//        if (teiidInstance.dataSourceExists(dsName))
//            teiidInstance.deleteDataSource(dsName);
//
//        Properties properties = new Properties();
//        properties.setProperty(TeiidInstance.DATASOURCE_JNDINAME, jndiName);
//        properties.setProperty(TeiidInstance.DATASOURCE_CONNECTION_URL, connUrl);
//
//        TeiidDataSource accountsDS = teiidInstance.getOrCreateDataSource(displayName, dsName, type, properties);
//        assertNotNull(accountsDS);
//
//        assertEquals(dsName, accountsDS.getName());
//        assertEquals(type, accountsDS.getType());
//        assertEquals(jndiName, accountsDS.getPropertyValue(TeiidInstance.DATASOURCE_JNDINAME));
//        assertEquals(connUrl, accountsDS.getPropertyValue(TeiidInstance.DATASOURCE_CONNECTION_URL));
//
//        teiidInstance.deleteDataSource(dsName);
//        assertFalse(teiidInstance.dataSourceExists(dsName));
//    }

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

    private void wait(int seconds) {
        try {
            Thread.sleep(seconds * 1000);
        } catch (Exception ex) {
            // Nothing required
        }
    }

    private TeiidDataSource deployDataSource() throws Exception {
        /*
             <jdbc-connection name="MySqlPool">
                <jndi-name>java:/MySqlDS</jndi-name>
                <driver-name>mysql-connector-java-5.1.39-bin.jarcom.mysql.jdbc.Driver_5_1</driver-name>
                <property name="track-statements">NOWARN</property>
                <property name="connection-url">jdbc:mysql://db4free.net:3306/usstates</property>
                <property name="share-prepared-statements">false</property>
                <property name="statistics-enabled">false</property>
                <property name="validate-on-match">false</property>
                <property name="allow-multiple-users">false</property>
                <property name="user-name">komodo</property>
                <property name="password">XUMz4vBKuA2v</property>
                <property name="use-fast-fail">false</property>
                <property name="set-tx-query-timeout">false</property>
                <property name="spy">false</property>
            </jdbc-connection>
         */

        String displayName = "MySqlPool";
        String jndiName = "java:/MySqlDS";
        String typeName = "mysql-connector_com.mysql.jdbc.Driver_5_1";
        Properties properties = new Properties();
        properties.setProperty("password", "XUMz4vBKuA2v");
        properties.setProperty("user-name", "komodo");
        properties.setProperty("validate-on-match", "false");
        properties.setProperty("connection-url", "jdbc:mysql://db4free.net:3306/usstates");
        properties.setProperty("jndi-name", "java:/MySqlDS");
        properties.setProperty("driver-name", "mysql-connector-java-5.1.39-bin.jar_com.mysql.jdbc.Driver_5_1");

        TeiidDataSource teiidDataSource = getMetadataInstance().getOrCreateDataSource(displayName, jndiName, typeName, properties);
        assertNotNull(teiidDataSource);
        return teiidDataSource;
    }

//    @Test
//    @Ignore( "Test demonstrates TEIID-4592 / 3834. Question when it will be fixed for Teiid 9.1+" )
//    public void testDataSourceOperations() throws Exception {
//        getTeiidInstance().connect();
//        assertTrue(getTeiidInstance().isConnected());
//
//        String MYSQL_DRIVER = "mysql-connector";
//
//        InputStream driverStream = TestUtilities.mySqlDriver();
//        assertNotNull(driverStream);
//
//        File driverFile = TestUtilities.createTempFile(MYSQL_DRIVER, ".jar");
//        driverFile.deleteOnExit();
//        FileUtils.write(driverStream, driverFile);
//        getTeiidInstance().deployDriver(MYSQL_DRIVER, driverFile);
//
//        wait(2);
//
//        TeiidDataSource teiidDataSource = deployDataSource();
//        assertTrue(getTeiidInstance().dataSourceExists(teiidDataSource.getName()));
//
//        String dsName = teiidDataSource.getName();
//        getTeiidInstance().deleteDataSource(dsName);
//
//        wait(4);
//
//        getTeiidInstance().reconnect();
//
//        assertFalse(getTeiidInstance().dataSourceExists(dsName));
//
//        /*
//         * Try redeploying
//         */
//        try {
//            teiidDataSource = deployDataSource();
//            wait(4);
//            getTeiidInstance().reconnect();
//
//            assertTrue(getTeiidInstance().dataSourceExists(dsName));
//        } catch (Exception ex) {
//            throw ex;
//        } finally {
//            if (getTeiidInstance().dataSourceExists(dsName)) {
//                // Ensure everything tidied up at the end
//                getTeiidInstance().deleteDataSource(dsName);
//                wait(4);
//            }
//        }
//    }
//
//    @Test
//    public void testDataSourceDrivers() throws Exception {
//        getTeiidInstance().connect();
//        assertTrue(getTeiidInstance().isConnected());
//        Collection<ConnectionDriver> dataSourceDrivers = getTeiidInstance().getDrivers();
//        assertTrue(dataSourceDrivers.size() > 0);
//
//        String[] driverNamesArr = {"h2", "teiid-local", "teiid"};
//        List<String> driverNames = Arrays.asList(driverNamesArr);
//        String[] classNamesArr = {"org.teiid.jdbc.TeiidDriver", "org.h2.Driver"};
//        List<String> classNames = Arrays.asList(classNamesArr);
//
//        Iterator<ConnectionDriver> iter = dataSourceDrivers.iterator();
//        while (iter.hasNext()) {
//            ConnectionDriver driver = iter.next();
//            assertTrue(driverNames.contains(driver.getName()));
//            assertTrue(classNames.contains(driver.getClassName()));
//        }
//    }
//
//    @Test
//    public void testGetSchema() throws Exception {
//        getTeiidInstance().connect();
//        assertTrue(getTeiidInstance().isConnected());
//
//        try {
//            getTeiidInstance().getSchema("blah", "1.0", "model");
//        } catch (InvocationTargetException ex) {
//            //
//            // Should throw this exception since blah does not exist but should not
//            // throw a NumberFormatException or NoSuchMethodException
//            //
//            Throwable cause = ex.getCause();
//            assertTrue(cause instanceof AdminProcessingException);
//            assertTrue(cause.getMessage().contains("does not exist or is not ACTIVE"));
//        }
//    }
}
