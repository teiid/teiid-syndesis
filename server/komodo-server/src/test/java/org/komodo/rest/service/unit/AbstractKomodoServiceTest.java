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
package org.komodo.rest.service.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.InputStream;
import java.net.URI;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.komodo.core.KEngine;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.rest.service.ServiceTestUtilities;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.TestUtilities;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.embedded.LocalServerPort;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public abstract class AbstractKomodoServiceTest extends AbstractServiceTest {

    @Autowired
    TestRestTemplate web;

    @Autowired
    KEngine engine;

    @LocalServerPort
    private int port;

    protected ServiceTestUtilities serviceTestUtilities;

    protected String PORTFOLIO_DATA_PATH;


    @Before
    public void beforeEachTest() throws Exception {
        serviceTestUtilities = new ServiceTestUtilities(this.engine);
        PORTFOLIO_DATA_PATH = serviceTestUtilities.getWorkspace(USER_NAME) + FORWARD_SLASH + TestUtilities.PORTFOLIO_VDB_NAME;
    }

    @After
    public void afterEachTest() throws Exception {
        serviceTestUtilities.deleteLogged(USER_NAME);
    }

    protected URI appUri() {
        return URI.create("http://localhost:"+port+"/vdb-builder/v1");
    }

    protected KomodoRestUriBuilder uriBuilder() {
        return new KomodoRestUriBuilder(appUri());
    }

    protected void logObjectPath(String objectPath) {
        serviceTestUtilities.logObjectPath(objectPath);
    }

    protected void logDataservice(Dataservice dataservice) throws Exception {
        if (dataservice == null)
            return;

        logObjectPath(dataservice.getAbsolutePath());

        UnitOfWork uow = serviceTestUtilities.createReadTransaction(USER_NAME);
        Vdb serviceVdb = dataservice.getServiceVdb(uow);
        String absPath = serviceVdb.getAbsolutePath();
        logObjectPath(absPath);

        Vdb[] vdbs = dataservice.getVdbs(uow);
        for (Vdb vdb : vdbs)
            logObjectPath(vdb.getAbsolutePath());

        Connection[] connections = dataservice.getConnections(uow);
        for (Connection connection : connections)
            logObjectPath(connection.getAbsolutePath());


    }

    protected void loadStatesDataService() throws Exception {
        removeStatesDataService();

        serviceTestUtilities.importDataservice(TestUtilities.usStatesDataserviceExample(), USER_NAME);

        Dataservice dataservice = serviceTestUtilities.getDataservice(USER_NAME, TestUtilities.US_STATES_DATA_SERVICE_NAME);
        Assert.assertNotNull(dataservice);

        logDataservice(dataservice);
    }

    protected void removeStatesDataService() throws Exception {
        Dataservice dataservice = serviceTestUtilities.getDataservice(USER_NAME, TestUtilities.US_STATES_DATA_SERVICE_NAME);
        if (dataservice != null)
            serviceTestUtilities.deleteObject(dataservice.getAbsolutePath(), USER_NAME);

        Vdb vdb = serviceTestUtilities.getVdb(USER_NAME, TestUtilities.US_STATES_VDB_NAME);
        if (vdb != null)
            serviceTestUtilities.deleteObject(vdb.getAbsolutePath(), USER_NAME);
    }

    protected void removeSampleService() throws Exception {
        Dataservice dataservice = serviceTestUtilities.getDataservice(USER_NAME, TestUtilities.SAMPLE_DATA_SERVICE_NAME);
        if (dataservice != null)
            serviceTestUtilities.deleteObject(dataservice.getAbsolutePath(), USER_NAME);

        //
        // TODO
        // The sample ds is a little odd in that not all of its vdbs being retrieved
        //
        String[] vdbNames = {"BooksExample",
                                                 TestUtilities.PORTFOLIO_VDB_NAME,
                                                 TestUtilities.TWEET_EXAMPLE_VDB_NAME};

        for (String vdbName : vdbNames) {
            Vdb vdb = serviceTestUtilities.getVdb(USER_NAME, vdbName);
            if (vdb == null)
                continue;

            serviceTestUtilities.deleteObject(vdb.getAbsolutePath(), USER_NAME);
        }
    }

    protected void loadStatesServiceSourceVdb() throws Exception {
        ImportMessages msgs = importVdb(TestUtilities.usStatesSourceExample(), USER_NAME);
        Assert.assertTrue(msgs.getErrorMessages().isEmpty());

        Vdb vdb = serviceTestUtilities.getVdb(USER_NAME, TestUtilities.USSTATES_SOURCE_VDB_NAME);
        Assert.assertNotNull(vdb);
        logObjectPath(vdb.getAbsolutePath());
    }

    protected void loadDsbSingleSourceDataService() throws Exception {
        serviceTestUtilities.importDataservice(TestUtilities.dsbDataserviceSingleSourceParts(), USER_NAME);
        Dataservice dataservice = serviceTestUtilities.getDataservice(USER_NAME, TestUtilities.PARTS_SINGLE_SOURCE_SERVICE_NAME);
        Assert.assertNotNull(dataservice);
        logObjectPath(dataservice.getAbsolutePath());
    }

    protected void loadDsbJoinDifferentTableNamesDataService() throws Exception {
        serviceTestUtilities.importDataservice(TestUtilities.dsbDataserviceJoinDifferentTableNames(), USER_NAME);
        Dataservice dataservice = serviceTestUtilities.getDataservice(USER_NAME, TestUtilities.JOIN_DIFFERENT_TABLE_NAMES_SERVICE_NAME);
        Assert.assertNotNull(dataservice);
        logObjectPath(dataservice.getAbsolutePath());
        logObjectPath(serviceTestUtilities.getWorkspace(USER_NAME) + FORWARD_SLASH + TestUtilities.JOIN_DIFFERENT_TABLE_NAMES_SERVICE_NAME + "VDB");
    }

    protected void loadDsbJoinSameTableNamesDataService() throws Exception {
        serviceTestUtilities.importDataservice(TestUtilities.dsbDataserviceJoinSameTableNames(), USER_NAME);
        Dataservice dataservice = serviceTestUtilities.getDataservice(USER_NAME, TestUtilities.JOIN_SAME_TABLE_NAMES_SERVICE_NAME);
        Assert.assertNotNull(dataservice);
        logObjectPath(dataservice.getAbsolutePath());
        logObjectPath(serviceTestUtilities.getWorkspace(USER_NAME) + FORWARD_SLASH + TestUtilities.JOIN_SAME_TABLE_NAMES_SERVICE_NAME + "VDB");
    }

    protected void createDataservice( String serviceName ) throws Exception {
        serviceTestUtilities.createDataservice(serviceName, false, USER_NAME);
        Assert.assertNotNull(serviceTestUtilities.getDataservice(USER_NAME, serviceName));
    }

    protected void createVdb( String vdbName ) throws Exception {
        serviceTestUtilities.createVdb(vdbName, USER_NAME);
        Assert.assertNotNull(serviceTestUtilities.getVdb(USER_NAME, vdbName));
    }

    protected void createVdbModel( String vdbName, String modelName ) throws Exception {
        serviceTestUtilities.createVdbModel(vdbName, modelName, USER_NAME);
        Assert.assertNotNull(serviceTestUtilities.getVdbModel(USER_NAME, vdbName, modelName));
    }

    protected void createVdbModelView( String vdbName, String modelName, String viewName ) throws Exception {
        serviceTestUtilities.createVdbModelView(vdbName, modelName, viewName, USER_NAME);
        Assert.assertNotNull(serviceTestUtilities.getVdbModelView(USER_NAME, vdbName, modelName, viewName));
    }

    protected void assertPortfolio(RestVdb vdb) throws Exception {
        assertEquals(PORTFOLIO_DATA_PATH, vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(TestUtilities.PORTFOLIO_VDB_NAME, vdb.getName());
        assertEquals("The Portfolio Dynamic VDB", vdb.getDescription());
        assertFalse(vdb.isPreview());
        assertEquals(1, vdb.getVersion());

        List<RestProperty> properties = vdb.getProperties();
        assertEquals(1, properties.size());
        RestProperty property = properties.iterator().next();
        assertEquals("UseConnectorMetadata", property.getName());
        assertEquals("true", property.getValue());

        Collection<RestLink> links = vdb.getLinks();
        assertEquals(7, links.size());

        int linkCounter = 0;
        for (RestLink link : links) {
            String href = link.getHref().toString();

            if (link.getRel().equals(LinkType.SELF)) {
                linkCounter++;
                assertTrue(href.startsWith(appUri().toString() + "/workspace/vdbs"));
                assertTrue(href.endsWith(TestUtilities.PORTFOLIO_VDB_NAME));
            } else if (link.getRel().equals(LinkType.PARENT)) {
                linkCounter++;
                assertTrue(href.startsWith(appUri().toString() + "/workspace/vdbs"));
            } else if (link.getRel().equals(LinkType.CHILDREN)) {
                linkCounter++;
                assertTrue(href.startsWith(appUri().toString() + "/workspace/search"));
            } else {
                assertTrue(href.startsWith(appUri().toString() + "/workspace/vdbs"));

                String suffixPrefix = TestUtilities.PORTFOLIO_VDB_NAME + FORWARD_SLASH;

                if (link.getRel().equals(LinkType.IMPORTS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.IMPORTS.uriName()));
                } else if (link.getRel().equals(LinkType.MODELS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.MODELS.uriName()));
                } else if (link.getRel().equals(LinkType.TRANSLATORS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.TRANSLATORS.uriName()));
                } else if (link.getRel().equals(LinkType.DATA_ROLES)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.DATA_ROLES.uriName()));
                }
            }
        }

        assertEquals(7, linkCounter);
    }

    public ImportMessages importVdb(InputStream vdbStream, String user) throws Exception {
        Repository repository = this.engine.getDefaultRepository();

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Import Vdb", false, callback, user); //$NON-NLS-1$

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();

        KomodoObject workspace = repository.komodoWorkspace(uow);
        VdbImporter importer = new VdbImporter(repository);
        importer.importVdb(uow, vdbStream, workspace, importOptions, importMessages);
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);

        return importMessages;
    }

    void loadVdbs() {
        try {
            ImportMessages msgs = importVdb(TestUtilities.allElementsExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());

            msgs = importVdb(TestUtilities.portfolioExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());

            msgs = importVdb(TestUtilities.partsWithKeysExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());

            msgs = importVdb(TestUtilities.tweetExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }
}
