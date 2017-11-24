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
package org.komodo.rest.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.core.UriBuilder;
import org.jboss.resteasy.plugins.server.tjws.TJWSEmbeddedJaxrsServer;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.komodo.core.internal.repository.search.ComparisonOperator;
import org.komodo.core.internal.repository.search.ObjectSearcher;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.response.RestVdb;
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
public abstract class AbstractKomodoServiceTest extends AbstractFrameworkTest {

    protected static KomodoRestV1Application _restApp;
    protected static TJWSEmbeddedJaxrsServer _server;
    private static URI _appUri;

    @BeforeClass
    public static void beforeAll() throws Exception {
        _restApp = new KomodoRestV1Application();

        _server = new TJWSEmbeddedJaxrsServer();
        _server.setPort(TEST_PORT);

        _server.getDeployment().setApplication(_restApp);
        _server.start();

        System.setProperty("org.jboss.resteasy.port", Integer.toString(TEST_PORT));
        URI baseUri = URI.create("http://localhost:8080");
        //
        // Note this lacks the /v1 context since the embedded server does not
        // seem to detect context from the application
        //
        _appUri = UriBuilder.fromUri(baseUri).scheme("http").build();
        _uriBuilder = new KomodoRestUriBuilder(_appUri);
    }

    @AfterClass
    public static void afterAll() throws Exception {
        if (_server != null)
            _server.stop();

        if (_restApp != null)
            _restApp.stop();
    }

    /**
     *
     */
    public AbstractKomodoServiceTest() {
        super();
    }

    @After
    public void afterEach() throws Exception {
        _restApp.clearRepository();
    }

    @Before
    public void beforeEach() {
    }

    protected KomodoRestV1Application getRestApp() {
        return _restApp;
    }

    protected void loadVdbs() throws Exception {
        _restApp.importVdb(TestUtilities.allElementsExample(), USER_NAME);
        _restApp.importVdb(TestUtilities.portfolioExample(), USER_NAME);
        _restApp.importVdb(TestUtilities.partsWithKeysExample(), USER_NAME);
        _restApp.importVdb(TestUtilities.tweetExample(), USER_NAME);

        Assert.assertEquals(4, _restApp.getVdbs(USER_NAME).length);
    }

    protected void loadStatesDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.usStatesDataserviceExample(), USER_NAME);
    }

    protected void loadServiceSourceVdb() throws Exception {
        _restApp.importVdb(TestUtilities.usStatesSourceExample(), USER_NAME);

        Assert.assertEquals(1, _restApp.getVdbs(USER_NAME).length);
    }

    protected void loadDsbSingleSourceDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.dsbDataserviceSingleSourceParts(), USER_NAME);
    }

    protected void loadDsbJoinDifferentTableNamesDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.dsbDataserviceJoinDifferentTableNames(), USER_NAME);
    }

    protected void loadDsbJoinSameTableNamesDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.dsbDataserviceJoinSameTableNames(), USER_NAME);
    }

    protected void createDataservice( String serviceName ) throws Exception {
        _restApp.createDataservice(serviceName, false, USER_NAME);

        Assert.assertEquals(1, _restApp.getDataservices(USER_NAME).length);
    }

    protected void createConnection( String connectionName ) throws Exception {
        _restApp.createConnection(connectionName, USER_NAME);

        Assert.assertEquals(1, _restApp.getConnections(USER_NAME).length);
    }

    protected void createDriver( String driverName ) throws Exception {
        _restApp.createDriver(driverName);

        Assert.assertEquals(1, _restApp.getDrivers().length);
    }

    protected void createVdb( String vdbName ) throws Exception {
        _restApp.createVdb(vdbName, USER_NAME);

        Assert.assertEquals(1, _restApp.getVdbs(USER_NAME).length);
    }

    protected void createVdbModel( String vdbName, String modelName ) throws Exception {
        _restApp.createVdbModel(vdbName, modelName, USER_NAME);

        Assert.assertEquals(1, _restApp.getVdbs(USER_NAME).length);
    }

    protected List<String> loadSampleSearches() throws Exception {
        List<String> searchNames = new ArrayList<>();
        Repository repository = _restApp.getDefaultRepository();

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

    protected void assertPortfolio(RestVdb vdb) {
        String expectedPath = "/tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio";
        assertEquals(expectedPath, vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(TestUtilities.PORTFOLIO_VDB_NAME, vdb.getName());
        assertEquals("The Portfolio Dynamic VDB", vdb.getDescription());
        assertEquals(expectedPath, vdb.getOriginalFilePath());
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
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/vdbs"));
                assertTrue(href.endsWith(TestUtilities.PORTFOLIO_VDB_NAME));
            } else if (link.getRel().equals(LinkType.PARENT)) {
                linkCounter++;
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/vdbs"));
            } else if (link.getRel().equals(LinkType.CHILDREN)) {
                linkCounter++;
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/search"));
            } else {
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/vdbs"));

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
