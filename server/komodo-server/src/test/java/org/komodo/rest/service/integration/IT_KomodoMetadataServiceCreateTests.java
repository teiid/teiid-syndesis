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
package org.komodo.rest.service.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Properties;

import org.junit.Ignore;
import org.junit.Test;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.test.utils.TestUtilities;

public class IT_KomodoMetadataServiceCreateTests extends AbstractKomodoMetadataServiceTest {

    @Override
    protected int getTestTotalInClass() {
        return 1;
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
