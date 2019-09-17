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

package org.komodo.rest.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.KException;
import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.metadata.TeiidVdb;
import org.komodo.metadata.internal.DefaultMetadataInstance;
import org.komodo.metadata.internal.TeiidDataSourceImpl;
import org.komodo.repository.KomodoRepositoryConfiguration;
import org.komodo.repository.WorkspaceManagerImpl;
import org.komodo.rest.KomodoJsonMarshaller;
import org.komodo.rest.datavirtualization.KomodoQueryAttribute;
import org.komodo.rest.datavirtualization.connection.RestSchemaNode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.server.ResponseStatusException;
import org.teiid.adminapi.impl.VDBMetaData;

@SuppressWarnings({ "javadoc", "nls" })
@RunWith(SpringRunner.class)
@DataJpaTest
@ContextConfiguration(classes = {KomodoRepositoryConfiguration.class, ServiceTestConfiguration.class})
@DirtiesContext
public class KomodoMetadataServiceTest {
    @Autowired
    private WorkspaceManagerImpl workspaceManagerImpl;

    @Autowired
    private KomodoMetadataService komodoMetadataService;

    @Autowired
    private DefaultMetadataInstance metadataInstance;

    @Test
    public void testSourceVdbGeneration() throws Exception {
//        Map<String, String> properties = new LinkedHashMap<String, String>();
//        properties.put(TeiidDataSource.DATASOURCE_JNDINAME, "something");
//        properties.put(TeiidDataSource.DATASOURCE_DRIVERNAME, "type");
//        properties.put(TeiidOpenShiftClient.ID, "source id");

        DefaultSyndesisDataSource sds = KomodoDataserviceServiceTest.createH2DataSource("source");
        metadataInstance.registerDataSource("source", (TeiidDataSourceImpl)sds.createDataSource("source"));

        TeiidDataSourceImpl tds = metadataInstance.getDataSource("source");

        VDBMetaData vdb = KomodoMetadataService.generateSourceVdb(tds, "vdb", null);

        String s = new String(DefaultMetadataInstance.toBytes(vdb).toByteArray(), "UTF-8");
        assertEquals(
                "<?xml version=\"1.0\" ?><vdb name=\"vdb\" version=\"1\"><description>Vdb for source Data Source:	source\n"
                        + "Type: \t\th2</description><connection-type>BY_VERSION</connection-type>"
                        + "<property name=\"id\" value=\"someid\"></property><property name=\"async-load\" value=\"true\"></property>"
                        + "<model name=\"source\" type=\"PHYSICAL\" visible=\"true\">"
                        + "<property name=\"importer.TableTypes\" value=\"TABLE,VIEW\"></property>"
                        + "<property name=\"importer.UseFullSchemaName\" value=\"false\"></property>"
                        + "<property name=\"importer.UseQualifiedName\" value=\"true\"></property>"
                        + "<property name=\"importer.UseCatalogName\" value=\"false\"></property>"
                        + "<source name=\"source\" translator-name=\"h2\" connection-jndi-name=\"source\"></source></model></vdb>",
                s);

        //with ddl passed in
        vdb = KomodoMetadataService.generateSourceVdb(tds, "vdb", "create something...");

        s = new String(DefaultMetadataInstance.toBytes(vdb).toByteArray(), "UTF-8");
        assertEquals(
                "<?xml version=\"1.0\" ?><vdb name=\"vdb\" version=\"1\"><description>Vdb for source Data Source:	source\n"
                        + "Type: \t\th2</description><connection-type>BY_VERSION</connection-type>"
                        + "<property name=\"id\" value=\"someid\"></property>"
                        + "<model name=\"source\" type=\"PHYSICAL\" visible=\"false\">"
                        + "<property name=\"importer.TableTypes\" value=\"TABLE,VIEW\"></property>"
                        + "<property name=\"importer.UseFullSchemaName\" value=\"false\"></property>"
                        + "<property name=\"importer.UseQualifiedName\" value=\"true\"></property>"
                        + "<property name=\"importer.UseCatalogName\" value=\"false\"></property>"
                        + "<source name=\"source\" translator-name=\"h2\" connection-jndi-name=\"source\"></source>"
                        + "<metadata type=\"DDLDB\"><![CDATA[someid]]></metadata></model></vdb>",
                s);

    }

    @Test
    public void testGetSchema() throws Exception {
        List<RestSchemaNode> nodes = null;
        try {
            nodes = komodoMetadataService.getSchema("source2");
            fail();
        } catch (ResponseStatusException e) {
            //no source yet
        }

        DefaultSyndesisDataSource sds = KomodoDataserviceServiceTest.createH2DataSource("source2");
        metadataInstance.registerDataSource("source2", (TeiidDataSourceImpl)sds.createDataSource("source2"));

        workspaceManagerImpl.createSchema("someid", "source",
                "create foreign table tbl (col string) options (\"teiid_rel:fqn\" 'schema=s%20x/t%20bl=bar');"
                + "create foreign table tbl1 (col string) options (\"teiid_rel:fqn\" 'schema=s%20x/t%20bl=bar1');");

        nodes = komodoMetadataService.getSchema("source");
        assertEquals("[ {\n" +
                "  \"children\" : [ {\n" +
                "    \"children\" : [ ],\n" +
                "    \"name\" : \"bar\",\n" +
                "    \"teiidName\" : \"tbl\",\n" +
                "    \"connectionName\" : \"source\",\n" +
                "    \"type\" : \"t bl\",\n" +
                "    \"queryable\" : true\n" +
                "  }, {\n" +
                "    \"children\" : [ ],\n" +
                "    \"name\" : \"bar1\",\n" +
                "    \"teiidName\" : \"tbl1\",\n" +
                "    \"connectionName\" : \"source\",\n" +
                "    \"type\" : \"t bl\",\n" +
                "    \"queryable\" : true\n" +
                "  } ],\n" +
                "  \"name\" : \"s x\",\n" +
                "  \"connectionName\" : \"source\",\n" +
                "  \"type\" : \"schema\",\n" +
                "  \"queryable\" : false\n" +
                "} ]", KomodoJsonMarshaller.marshall(nodes));
    }

    @Test
    public void testGetSchemaSingleLevel() throws Exception {
        List<RestSchemaNode> nodes = null;
        try {
            nodes = komodoMetadataService.getSchema("source3");
            fail();
        } catch (ResponseStatusException e) {
            //no source yet
        }

        //add the data source, and schema
        DefaultSyndesisDataSource sds = KomodoDataserviceServiceTest.createH2DataSource("source3");
        metadataInstance.registerDataSource("source3", (TeiidDataSourceImpl)sds.createDataSource("source3"));


        workspaceManagerImpl.createSchema("someid", "source3",
                "create foreign table tbl (col string) options (\"teiid_rel:fqn\" 'collection=bar');"
                + "create foreign table tbl1 (col string) options (\"teiid_rel:fqn\" 'collection=bar1');");

        nodes = komodoMetadataService.getSchema("source3");
        assertEquals("[ {\n" +
                "  \"children\" : [ ],\n" +
                "  \"name\" : \"bar\",\n" +
                "  \"teiidName\" : \"tbl\",\n" +
                "  \"connectionName\" : \"source3\",\n" +
                "  \"type\" : \"collection\",\n" +
                "  \"queryable\" : true\n" +
                "}, {\n" +
                "  \"children\" : [ ],\n" +
                "  \"name\" : \"bar1\",\n" +
                "  \"teiidName\" : \"tbl1\",\n" +
                "  \"connectionName\" : \"source3\",\n" +
                "  \"type\" : \"collection\",\n" +
                "  \"queryable\" : true\n" +
                "} ]", KomodoJsonMarshaller.marshall(nodes));
    }

    @Test
    public void testPreviewQuery() throws Exception {
        KomodoQueryAttribute kqa = new KomodoQueryAttribute();
        kqa.setQuery("select * from myview");
        kqa.setTarget("dv1");

        workspaceManagerImpl.createDataVirtualization("dv1");

        //get rid of the default preview vdb
        metadataInstance.undeployDynamicVdb(KomodoUtilService.PREVIEW_VDB);

        try {
            komodoMetadataService.updatePreviewVdb("dv1");
            fail();
        } catch (KException e) {
            //preveiw vdb does not exist
        }

        metadataInstance.deploy(KomodoUtilServiceTest.dummyPreviewVdb());

        //even with no views, we should still succeed
        TeiidVdb vdb = komodoMetadataService.updatePreviewVdb("dv1");

        //there will be a validity error from being empty
        assertTrue(!vdb.getValidityErrors().isEmpty());

        metadataInstance.query(vdb.getName(), "select * from v", DefaultMetadataInstance.NO_OFFSET, DefaultMetadataInstance.NO_LIMIT);
    }
}