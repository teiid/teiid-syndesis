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
import java.io.InputStream;
import java.net.URI;
import java.util.Base64;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.core.UriBuilder;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.TeiidSwarmConnectionProvider;
import org.komodo.rest.TeiidSwarmMetadataInstance;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.response.ImportExportStatus;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.test.utils.TestUtilities;
import org.komodo.test.utils.UsStatesTestDB;
import org.komodo.utils.FileUtils;

@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractKomodoMetadataServiceTest extends AbstractServiceTest {

    protected static final String MYSQL_DRIVER = "mysql-connector";

    protected static KomodoRestUriBuilder _uriBuilder;

    private int testIndex = 0;

    private TeiidSwarmMetadataInstance instance;

    private static UsStatesTestDB testDB;

    @Deployment( testable = false )
    public static WebArchive createRestDeployment() {
        return ShrinkWrap.createFromZipFile(WebArchive.class, new File("target/vdb-builder.war"));
    }

    @BeforeClass
    public static void beforeAllSetupBaseUri() throws Exception {
        System.setProperty("org.jboss.resteasy.port", Integer.toString(TEST_PORT));
        URI baseUri = URI.create("http://localhost" + COLON + TEST_PORT);
        baseUri = UriBuilder.fromUri(baseUri).scheme("http").path("/vdb-builder/v1").build();
        _uriBuilder = new KomodoRestUriBuilder(baseUri);

        testDB = new UsStatesTestDB();
    }

    @AfterClass
    public static void afterAllTests() throws Exception {
        if (testDB == null)
            return;

        testDB.dispose();
    }

    protected void assertNoMysqlDriver() throws Exception {
        wait(2);
    
        Collection<ConnectionDriver> drivers = getMetadataInstance().getDataSourceDrivers();
        for (ConnectionDriver driver : drivers) {
            assertFalse(driver.getName().startsWith(MYSQL_DRIVER));
        }
    }

    protected void assertMysqlDriver() throws Exception {
        boolean found = false;
        for (int i = 0; i < 10 && !found; i++) {
            wait(3);
            Collection<ConnectionDriver> drivers = getMetadataInstance().getDataSourceDrivers();
            for (ConnectionDriver driver : drivers) {
                // Use startswith rather than equals since the
                // mysql connector gives up 2 drivers rather than just 1
                if (driver.getName().startsWith(MYSQL_DRIVER)) {
                    found = true;
                    break;
                }
            }
        }
        assertTrue("Cannot find deployed driver", found);
    }

    protected MetadataInstance getMetadataInstance() throws Exception {
        if (instance == null) {
            TeiidSwarmConnectionProvider connectionProvider = new TeiidSwarmConnectionProvider();
            instance = new TeiidSwarmMetadataInstance(connectionProvider);
        }

        return instance;
    }

    protected void waitForVdb() throws Exception {
        Thread.sleep(20);
    }

    protected void wait(int seconds) {
        try {
            Thread.sleep(seconds * 1000);
        } catch (Exception ex) {
            // Nothing required
        }
    }

    protected void undeployDrivers() throws Exception {
        Set<String> undeployDrivers = new HashSet<String>();
        Collection<ConnectionDriver> drivers = getMetadataInstance().getDataSourceDrivers();
        for (ConnectionDriver driver : drivers) {
            if (driver.getName().startsWith(MYSQL_DRIVER)) {
                String driverName = driver.getName();
                //
                // MySQL has 2 drivers so concatenates the class name
                // to the end of the driver names but means that the driver
                // cannot be undeployed unless the class name is removed
                //
                int endsWithClass = driverName.lastIndexOf(JAR + UNDERSCORE);
                if (endsWithClass > -1)
                    driverName = driverName.substring(0, endsWithClass + JAR.length());
    
                undeployDrivers.add(driverName);
            }
        }
    
        for (String driver : undeployDrivers) {
            try {
                getMetadataInstance().undeployDataSourceDriver(driver);
            } catch (Exception ex) {
                // Flag as a warning that something in the test is going awry
                ex.printStackTrace();
            }
        }
    }

    private void undeployVdbs() throws Exception {
        deleteSample();
    }

    private void undeployDataSources() throws Exception {
        MetadataInstance teiidInstance = getMetadataInstance();
    
        for (TeiidDataSource ds : teiidInstance.getDataSources()) {
            if (ds.getName().contains("Example"))
                continue; // Leave the exampleDS in situ

            getMetadataInstance().deleteDataSource(ds.getName());
        }
    }

    protected void deleteSample() throws Exception {
        //
        // REMOVE SAMPLE IF IT EXISTS
        //
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.WORKSPACE_SEGMENT)
                                            .path(V1Constants.VDBS_SEGMENT)
                                            .path(TestUtilities.SAMPLE_VDB_NAME).build();

        HttpUriRequest request = jsonRequest(uri, RequestType.DELETE);
        addJsonConsumeContentType(request);

        execute(request);
    }

    protected void loadSample() throws Exception {
        deleteSample();

        //
        // IMPORT SAMPLE INTO WORKSPACE
        //
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                            .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType(StorageConnector.Types.FILE.id());
        storageAttr.setDocumentType(DocumentType.VDB_XML);
        storageAttr.setParameter(StorageConnector.IMPORT_OVERWRITE_PROPERTY, ExistingNodeOptions.OVERWRITE.name());

        String sampleCnt = FileUtils.streamToString(TestUtilities.sampleExample());
        String content = Base64.getEncoder().encodeToString(sampleCnt.getBytes());
        storageAttr.setContent(content);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);

        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(VDB_DEPLOYMENT_SUFFIX, status.getType());

        //
        // DEPLOY SAMPLE TO METADATA SERVER
        //
        String samplePath = "/tko:komodo/tko:workspace/" + USER_NAME + "/sample";
        KomodoPathAttribute pathAttribute = new KomodoPathAttribute();
        pathAttribute.setPath(samplePath);

        uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                    .path(V1Constants.METADATA_SEGMENT)
                                    .path(V1Constants.VDB_SEGMENT).build();

        request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, pathAttribute);

        response = executeOk(request);
    }

    @Before
    public void beforeEach() throws Exception {
        testIndex++;
    
        // Deploy sample vdb for services to find
        loadSample();

       waitForVdb();
    }

    protected abstract int getTestTotalInClass();

    @After
    public void afterEach() throws Exception {
        //
        // Do not undeploy between tests due to TEIID-4592 / 3834.
        // (Question when it will be fixed for Teiid 9.1+)
        //
        // Ensure tests are layered in their own classes so that the
        // server is restarted inbetween.
        //
        if (testIndex  == getTestTotalInClass()) {
            try {
                undeployVdbs();
                undeployDataSources();
                undeployDrivers();
                wait(2);
            } catch (Exception ex) {
                ex.printStackTrace(); // show in console but avoid failing the test
            }
        }
    }

    protected void importDataService() throws Exception {
        //
        // Import the data service into the workspace
        //
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                          .path(V1Constants.IMPORT)
                                          .build();
    
        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType(StorageConnector.Types.FILE.id());
        storageAttr.setDocumentType(DocumentType.ZIP);

        InputStream usStatesDSStream = TestUtilities.usStatesDataserviceExample();
        byte[] sampleBytes = TestUtilities.streamToBytes(usStatesDSStream);
        String content = Base64.getEncoder().encodeToString(sampleBytes);
        storageAttr.setContent(content);
    
        HttpPost request = jsonRequest(uri, RequestType.POST);
        addBody(request, storageAttr);

        executeOk(request);
    }

    protected void deployDataService() throws Exception {
        KomodoPathAttribute pathAttr = new KomodoPathAttribute();
        String path = RepositoryImpl.komodoWorkspacePath(null) + FORWARD_SLASH +
                                        USER_NAME + FORWARD_SLASH + "UsStatesService";
        pathAttr.setPath(path);
    
        //
        // Deploy the data service
        //
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                    .path(V1Constants.METADATA_SEGMENT)
                                    .path(V1Constants.DATA_SERVICE_SEGMENT)
                                    .build();
    
        HttpPost request = jsonRequest(uri, RequestType.POST);
        addBody(request, pathAttr);
        HttpResponse response = executeOk(request);
    
        String entity = extractResponse(response);
        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);
    
        Map<String, String> attributes = status.getAttributes();
        for (Map.Entry<String, String> attribute : attributes.entrySet()) {
            assertFalse("Error occurred in deployment: " + attribute.getValue(),
                        attribute.getKey().startsWith("ErrorMessage"));
        }
    }

    protected void queryDataService(KomodoQueryAttribute queryAttr, int expRowCount, String firstCellValue) throws Exception {
        URI uri;
        String entity;
        //
        // Query the deployed vdb
        //
        uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                    .path(V1Constants.METADATA_SEGMENT)
                                    .path(V1Constants.QUERY_SEGMENT)
                                    .build();
    
        HttpPost request = jsonRequest(uri, RequestType.POST);
        addBody(request, queryAttr);
        HttpResponse response = executeOk(request);

        entity = extractResponse(response);
        RestQueryResult result = KomodoJsonMarshaller.unmarshall(entity, RestQueryResult.class);
        assertNotNull(result);
        assertEquals(expRowCount, result.getRows().length);
    
        RestQueryRow firstRow = result.getRows()[0];
        String value = firstRow.getValues()[0];
        assertEquals(firstCellValue, value);
    }
}
