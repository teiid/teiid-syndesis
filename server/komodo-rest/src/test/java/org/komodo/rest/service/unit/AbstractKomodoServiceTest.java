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
package org.komodo.rest.service.unit;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.komodo.core.internal.repository.search.ComparisonOperator;
import org.komodo.core.internal.repository.search.ObjectSearcher;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.importer.ImportMessages;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.rest.service.ServiceTestUtilities;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.TestUtilities;

/**
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public abstract class AbstractKomodoServiceTest extends AbstractServiceTest {

    //
    // With this rule placed here and in the suite ensures that UnitServiceResources
    // will be correctly instantiated whether running in the suite or as a single test class
    //
    @ClassRule
    public static UnitServiceResources unitServiceResources = UnitServiceResources.getInstance();

    protected final ServiceTestUtilities serviceTestUtilities;

    protected final String PORTFOLIO_DATA_PATH;

    protected static KomodoRestV1Application restApp() {
        return unitServiceResources.restApp();
    }

    @After
    public void afterEachTest() throws Exception {
        serviceTestUtilities.deleteLogged(USER_NAME);
    }

    protected static URI appUri() {
        return unitServiceResources.appUri();
    }

    protected static KomodoRestUriBuilder uriBuilder() {
        return unitServiceResources.uriBuilder();
    }

    public AbstractKomodoServiceTest() throws Exception {
        super();
        serviceTestUtilities = new ServiceTestUtilities(restApp().getEngine());
        PORTFOLIO_DATA_PATH = serviceTestUtilities.getWorkspace(USER_NAME) + FORWARD_SLASH + TestUtilities.PORTFOLIO_VDB_NAME;
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

        Driver[] drivers = dataservice.getDrivers(uow);
        for (Driver driver : drivers)
            logObjectPath(driver.getAbsolutePath());

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

        Driver driver = serviceTestUtilities.getDriver(USER_NAME, TestUtilities.US_STATES_DRIVER_NAME);
        if (driver != null)
            serviceTestUtilities.deleteObject(driver.getAbsolutePath(), USER_NAME);
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

        Driver driver = serviceTestUtilities.getDriver(USER_NAME, TestUtilities.US_STATES_DRIVER_NAME);
        if (driver != null)
            serviceTestUtilities.deleteObject(driver.getAbsolutePath(), USER_NAME);
    }

    protected void loadStatesServiceSourceVdb() throws Exception {
        ImportMessages msgs = restApp().importVdb(TestUtilities.usStatesSourceExample(), USER_NAME);
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

    protected void createConnection( String connectionName ) throws Exception {
        serviceTestUtilities.createConnection(connectionName, USER_NAME);
        Assert.assertNotNull(serviceTestUtilities.getConnection(USER_NAME, connectionName));
    }

    protected void createDriver( String driverName ) throws Exception {
        serviceTestUtilities.createDriver(driverName);
        Assert.assertNotNull(serviceTestUtilities.getDriver(USER_NAME, driverName));
    }

    protected void createVdb( String vdbName ) throws Exception {
        serviceTestUtilities.createVdb(vdbName, USER_NAME);
        Assert.assertNotNull(serviceTestUtilities.getVdb(USER_NAME, vdbName));
    }

    protected void createVdbModel( String vdbName, String modelName ) throws Exception {
        serviceTestUtilities.createVdbModel(vdbName, modelName, USER_NAME);
        Assert.assertNotNull(serviceTestUtilities.getVdbModel(USER_NAME, vdbName, modelName));
    }

    protected List<String> loadSampleSearches() throws Exception {
        List<String> searchNames = new ArrayList<>();
        Repository repository = restApp().getDefaultRepository();

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "SaveSearchInWorkspace" + COLON + System.currentTimeMillis(),
                                                      false, callback);

        ObjectSearcher vdbsSearch = new ObjectSearcher(repository);
        vdbsSearch.setFromType(VdbLexicon.Vdb.VIRTUAL_DATABASE, "vdbs");
        String vdbSearchName = "Vdbs Search";
        vdbsSearch.write(uow, vdbSearchName);

        ObjectSearcher columnsSearch = new ObjectSearcher(repository);
        columnsSearch.setFromType(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT, "c");
        String columnSearchName = "Columns Search";
        columnsSearch.write(uow, columnSearchName);

        ObjectSearcher columnsWithParamSearch = new ObjectSearcher(repository);
        columnsWithParamSearch.setFromType(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT, "c");
        columnsWithParamSearch.addWhereCompareClause(null, "c", "mode:localName", ComparisonOperator.LIKE, "{valueParam}");
        String columnsWithParamSearchName = "Columns Search With Where Parameter";
        columnsWithParamSearch.write(uow, columnsWithParamSearchName);

        ObjectSearcher fromParameterSearch = new ObjectSearcher(repository);
        fromParameterSearch.setFromType("{fromTypeParam}", "c");
        String fromParamSearchName = "From Parameter Search";
        fromParameterSearch.write(uow, fromParamSearchName);

        uow.commit();

        if (!callback.await(3, TimeUnit.MINUTES)) {
            throw new Exception("Timed out while loading saved searches");
        }

        if (callback.error() != null)
            throw new Exception(callback.error());

        searchNames.add(vdbSearchName);
        searchNames.add(columnSearchName);
        searchNames.add(columnsWithParamSearchName);
        searchNames.add(fromParamSearchName);

        return searchNames;
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

}
