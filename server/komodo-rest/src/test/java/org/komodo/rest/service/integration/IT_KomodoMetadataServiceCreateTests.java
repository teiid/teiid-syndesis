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
package org.komodo.rest.service.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.util.Collection;
import java.util.Properties;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.rest.TeiidSwarmConnectionProvider;
import org.komodo.rest.TeiidSwarmMetadataInstance;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.test.utils.TestUtilities;

@RunWith( Arquillian.class )
public class IT_KomodoMetadataServiceCreateTests {

    @Deployment(testable=false)
    public static WebArchive createDeployment() throws Exception {
        return ShrinkWrap.createFromZipFile(WebArchive.class, new File("target/vdb-builder.war"));
    }

    private TeiidSwarmMetadataInstance instance;

    private MetadataInstance getMetadataInstance() throws Exception {
        if (instance == null) {
            TeiidSwarmConnectionProvider connectionProvider = new TeiidSwarmConnectionProvider();
            instance = new TeiidSwarmMetadataInstance(connectionProvider);
        }

        return instance;
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
    @Ignore("Requires configuration of loopback translator")
    public void testDeployment() throws Exception {
        getMetadataInstance().deployDynamicVdb(TestUtilities.SAMPLE_VDB_FILE, TestUtilities.sampleExample());
        Thread.sleep(2000);

        Collection<TeiidVdb> vdbs = getMetadataInstance().getVdbs();
        assertEquals(1, vdbs.size());

        TeiidVdb vdb;
        long timeout = System.currentTimeMillis() + 120000;
        do {
            vdb = getMetadataInstance().getVdb(TestUtilities.SAMPLE_VDB_NAME);
            assertNotNull(vdb);
            assertEquals(0, vdb.getValidityErrors().size());

            Thread.sleep(3000);
        } while (vdb.isLoading() && System.currentTimeMillis() < timeout);

        assertFalse(vdb.isLoading());
        assertTrue(vdb.isActive());

        getMetadataInstance().undeployDynamicVdb(TestUtilities.SAMPLE_VDB_NAME);
        vdbs = getMetadataInstance().getVdbs();
        assertEquals(0, vdbs.size());
    }

}
