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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.security.GeneralSecurityException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.Base64;
import java.util.Map;
import javax.net.ssl.SSLContext;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.apache.http.auth.Credentials;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLContextBuilder;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.conn.BasicHttpClientConnectionManager;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.jboss.resteasy.client.core.executors.ApacheHttpClient4Executor;
import org.jboss.resteasy.test.TestPortProvider;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.repository.RepositoryImpl;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.response.ImportExportStatus;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;

@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractKomodoTeiidServiceTest implements StringConstants {

    protected static final String USER_NAME = "user";
    private static final String PASSWORD = "user";
    private static final String TEST_PORT = "8443";
    protected static final String MYSQL_DRIVER = "mysql-connector";
    private static Path _kengineDataDir;
    protected static KomodoRestUriBuilder _uriBuilder;
    protected static URI BASE_URI;

    private int testIndex = 0;

    @Deployment( testable = false )
    public static WebArchive createRestDeployment() {
        return ShrinkWrap.createFromZipFile(WebArchive.class, new File("target/vdb-builder.war"));
    }

    @BeforeClass
    public static void beforeAll() throws Exception {
        _kengineDataDir = Files.createTempDirectory(null, new FileAttribute[0]);
        System.setProperty(SystemConstants.ENGINE_DATA_DIR, _kengineDataDir.toString());

        System.setProperty("org.jboss.resteasy.port", TEST_PORT);
        final URI baseUri = URI.create(TestPortProvider.generateBaseUrl());
        BASE_URI = UriBuilder.fromUri(baseUri).scheme("https").path("/vdb-builder/v1").build();
        _uriBuilder = new KomodoRestUriBuilder(BASE_URI);
    }

    @AfterClass
    public static void afterAll() throws Exception {
        if (_kengineDataDir != null) {
            FileUtils.removeDirectoryAndChildren(_kengineDataDir.toFile());
        }
    }

    protected void addHeader(ClientRequest request, String name, Object value) {
        request.getHeadersAsObjects().add(name, value);
    }

    /**
     * Builds an {@link ApacheHttpClient4Executor} which does NOT verify ssl certificates so allows for the
     * self-signed certificates used in the integration testing.
     *
     * @return client executor with no ssl certificate verification
     *
     * @throws GeneralSecurityException
     */
    private ApacheHttpClient4Executor createSSLTrustingClientExecutor() throws GeneralSecurityException {
        RegistryBuilder<ConnectionSocketFactory> registryBuilder = RegistryBuilder.<ConnectionSocketFactory>create()
            .register("http", PlainConnectionSocketFactory.getSocketFactory());
    
        TrustStrategy trustStrategy = new TrustStrategy() {
            @Override
            public boolean isTrusted(X509Certificate[] chain, String authType) throws CertificateException {
                return true;
            }
        };
    
        SSLContextBuilder sslctxBuilder = new SSLContextBuilder();
        sslctxBuilder.loadTrustMaterial(null, trustStrategy);
        SSLContext sslContext = sslctxBuilder.build();
    
        SSLConnectionSocketFactory sslSocketFactory = new SSLConnectionSocketFactory(sslContext,
                                                                                                 SSLConnectionSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);
        registryBuilder.register("https", sslSocketFactory);
    
        BasicHttpClientConnectionManager mgr = new BasicHttpClientConnectionManager(registryBuilder.build());
    
        HttpClientBuilder clientBuilder = HttpClientBuilder.create();
        clientBuilder.setConnectionManager(mgr);
    
        Credentials credentials = new UsernamePasswordCredentials(USER_NAME, PASSWORD);
        CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(org.apache.http.auth.AuthScope.ANY, credentials);
        clientBuilder.setDefaultCredentialsProvider(credentialsProvider);
    
        return new ApacheHttpClient4Executor(clientBuilder.build());
    }

    protected ClientRequest request(final URI uri, MediaType type) throws Exception {
        ClientRequest request = new ClientRequest(uri.toString(), createSSLTrustingClientExecutor());
        if (type != null)
            request.accept(type);
    
        return request;
    }

//    protected void assertNoMysqlDriver() throws Exception {
//        wait(2);
//    
//        Collection<ConnectionDriver> drivers = helperInstance.getDrivers();
//        for (ConnectionDriver driver : drivers) {
//            assertFalse(driver.getName().startsWith(MYSQL_DRIVER));
//        }
//    }
//
//    protected void assertMysqlDriver() throws Exception {
//        boolean found = false;
//        for (int i = 0; i < 10 && !found; i++) {
//            wait(3);
//            Collection<ConnectionDriver> drivers = helperInstance.getDrivers();
//            for (ConnectionDriver driver : drivers) {
//                // Use startswith rather than equals since the
//                // mysql connector gives up 2 drivers rather than just 1
//                if (driver.getName().startsWith(MYSQL_DRIVER)) {
//                    found = true;
//                    break;
//                }
//            }
//        }
//        assertTrue("Cannot find deployed driver", found);
//    }

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

    protected void checkResponse(ClientResponse<String> response) {
        assertNotNull(response);
        String entity = response.getEntity();
        if (Response.Status.OK.getStatusCode() != response.getStatus()) {
            fail(response.getStatus() + COLON + SPACE + entity.toString());
        }
    }

//    protected void undeployDrivers() throws Exception {
//        Set<String> undeployDrivers = new HashSet<String>();
//        Collection<ConnectionDriver> drivers = helperInstance.getDrivers();
//        for (ConnectionDriver driver : drivers) {
//            if (driver.getName().startsWith(MYSQL_DRIVER)) {
//                String driverName = driver.getName();
//                //
//                // MySQL has 2 drivers so concatenates the class name
//                // to the end of the driver names but means that the driver
//                // cannot be undeployed unless the class name is removed
//                //
//                int endsWithClass = driverName.lastIndexOf(UNDERSCORE + driver.getClassName());
//                if (endsWithClass == -1)
//                    endsWithClass = driverName.lastIndexOf(driver.getClassName());
//    
//                if (endsWithClass > -1)
//                    driverName = driverName.substring(0, endsWithClass);
//    
//                undeployDrivers.add(driverName);
//            }
//        }
//    
//        for (String driver : undeployDrivers) {
//            try {
//                helperInstance.undeployDriver(driver);
//            } catch (Exception ex) {
//                // Flag as a warning that something in the test is going awry
//                ex.printStackTrace();
//            }
//        }
//    }

    private void undeployVdbs() throws Exception {
        deleteSample();
    }

//    private void undeployDataSources() throws Exception {
//        TeiidInstance teiidInstance = getTeiidInstance();
//        teiidInstance.connect();
//    
//        for (TeiidDataSource ds : teiidInstance.getDataSources()) {
//            if (ds.getName().contains("Example"))
//                continue; // Leave the exampleDS in situ
//
//            helperInstance.deleteDataSource(ds.getName());
//        }
//    }

    protected void deleteSample() throws Exception {
        //
        // REMOVE SAMPLE IF IT EXISTS
        //
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.WORKSPACE_SEGMENT)
                                            .path(V1Constants.VDBS_SEGMENT)
                                            .path(TestUtilities.SAMPLE_VDB_NAME).build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.header("Content-Type", MediaType.APPLICATION_JSON);
        ClientResponse<String> response = request.delete(String.class);
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
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.VDB_XML);
        storageAttr.setParameter(StorageConnector.IMPORT_OVERWRITE_PROPERTY, ExistingNodeOptions.OVERWRITE.name());

        String sampleCnt = FileUtils.streamToString(TestUtilities.sampleExample());
        String content = Base64.getEncoder().encodeToString(sampleCnt.getBytes());
        storageAttr.setContent(content);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.header("Content-Type", MediaType.APPLICATION_JSON);
        request.body(MediaType.APPLICATION_JSON_TYPE, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        String entity = response.getEntity();

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(VDB_DEPLOYMENT_SUFFIX, status.getType());

        //
        // DEPLOY SAMPLE TO METADATA SERVER
        //
        String samplePath = "/tko:komodo/tko:workspace/user/sample";
        KomodoPathAttribute pathAttribute = new KomodoPathAttribute();
        pathAttribute.setPath(samplePath);

        uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                    .path(V1Constants.METADATA_SEGMENT)
                                    .path(V1Constants.VDB_SEGMENT).build();

        request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.header("Content-Type", MediaType.APPLICATION_JSON);
        request.body(MediaType.APPLICATION_JSON_TYPE, pathAttribute);
        response = request.post(String.class);

        checkResponse(response);
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
//                undeployDataSources();
//                undeployDrivers();
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
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.ZIP);
    
        InputStream usStatesDSStream = TestUtilities.usStatesDataserviceExample();
        byte[] sampleBytes = TestUtilities.streamToBytes(usStatesDSStream);
        String content = Base64.getEncoder().encodeToString(sampleBytes);
        storageAttr.setContent(content);
    
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, storageAttr);
        ClientResponse<String> response = request.post(String.class);
    
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
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
    
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, pathAttr);
        ClientResponse<String> response = request.post(String.class);
        checkResponse(response);
    
        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(response.getEntity(), KomodoStatusObject.class);
        assertNotNull(status);
    
        Map<String, String> attributes = status.getAttributes();
        for (Map.Entry<String, String> attribute : attributes.entrySet()) {
            assertFalse("Error occurred in deployment: " + attribute.getValue(),
                        attribute.getKey().startsWith("ErrorMessage"));
        }
    }

    protected void queryDataService(KomodoQueryAttribute queryAttr, int expRowCount, int firstCellValue) throws Exception {
        URI uri;
        String entity;
        //
        // Query the deployed vdb
        //
        uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                    .path(V1Constants.METADATA_SEGMENT)
                                    .path(V1Constants.QUERY_SEGMENT)
                                    .build();
    
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, queryAttr);
        ClientResponse<String> response = request.post(String.class);
        entity = response.getEntity();
    
        checkResponse(response);
    
        RestQueryResult result = KomodoJsonMarshaller.unmarshall(entity, RestQueryResult.class);
        assertNotNull(result);
        assertEquals(expRowCount, result.getRows().length);
    
        RestQueryRow firstRow = result.getRows()[0];
        String value = firstRow.getValues()[0];
        assertEquals(new Integer(firstCellValue).toString(), value);
    }
}
