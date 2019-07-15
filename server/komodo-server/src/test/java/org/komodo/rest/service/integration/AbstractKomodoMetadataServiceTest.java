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
import static org.junit.Assert.assertNotNull;

import java.net.URI;

import javax.ws.rs.core.UriBuilder;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.runner.RunWith;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.runtime.TeiidDataSource;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.test.utils.TestUtilities;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.embedded.LocalServerPort;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractKomodoMetadataServiceTest extends AbstractServiceTest {

    @Autowired
    TestRestTemplate web;

    @LocalServerPort
    private int port;

    @Autowired
    private MetadataInstance instance;

    protected static final String MYSQL_DRIVER = "mysql-connector";

    private int testIndex = 0;

    public KomodoRestUriBuilder getUriBuilder() throws Exception {
        //System.setProperty("org.jboss.resteasy.port", Integer.toString(TEST_PORT));
        URI baseUri = URI.create("http://localhost:" + port);
        baseUri = UriBuilder.fromUri(baseUri).scheme("http").path("/vdb-builder/v1").build();
        return new KomodoRestUriBuilder(baseUri);
    }

    @AfterClass
    public static void afterAllTests() throws Exception {

    }

    protected MetadataInstance getMetadataInstance() throws Exception {
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
        URI uri = UriBuilder.fromUri(getUriBuilder().baseUri())
                                            .path(V1Constants.WORKSPACE_SEGMENT)
                                            .path(V1Constants.VDBS_SEGMENT)
                                            .path(TestUtilities.SAMPLE_VDB_NAME).build();

        HttpUriRequest request = jsonRequest(uri, RequestType.DELETE);
        addJsonConsumeContentType(request);

        execute(request);
    }

    protected void loadSample() throws Exception {
        deleteSample();

        //TODO
        
        //
        // DEPLOY SAMPLE TO METADATA SERVER
        //
        /*String samplePath = "/tko:komodo/tko:workspace/" + USER_NAME + "/sample";
        KomodoPathAttribute pathAttribute = new KomodoPathAttribute();
        pathAttribute.setPath(samplePath);

        URI uri = UriBuilder.fromUri(getUriBuilder().baseUri())
                                    .path(V1Constants.METADATA_SEGMENT)
                                    .path(V1Constants.VDB_SEGMENT).build();

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, pathAttribute);

        executeOk(request);*/
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
                wait(2);
            } catch (Exception ex) {
                ex.printStackTrace(); // show in console but avoid failing the test
            }
        }
    }

    protected void queryDataService(KomodoQueryAttribute queryAttr, int expRowCount, String firstCellValue) throws Exception {
        URI uri;
        String entity;
        //
        // Query the deployed vdb
        //
        uri = UriBuilder.fromUri(getUriBuilder().baseUri())
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
