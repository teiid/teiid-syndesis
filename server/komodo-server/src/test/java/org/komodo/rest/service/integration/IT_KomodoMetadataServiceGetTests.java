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
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.core.UriBuilder;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.cors.CorsHeaders;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.metadata.RestMetadataConnection;
import org.komodo.rest.relational.response.metadata.RestMetadataStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplate;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplateEntry;
import org.komodo.rest.relational.response.metadata.RestMetadataVdb;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatusVdb;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbTranslator;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.version.DefaultMetadataVersion;
import org.komodo.test.utils.TestUtilities;
import org.teiid.adminapi.AdminProcessingException;
import org.teiid.core.util.ApplicationInfo;

@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public class IT_KomodoMetadataServiceGetTests extends AbstractKomodoMetadataServiceTest {

    private void testTranslators(RestMetadataStatus status) throws Exception {
        assertEquals(getMetadataInstance().getTranslators().size(), status.getTranslatorSize());
    }

    @Override
    protected int getTestTotalInClass() {
        return 14;
    }

    @Test
    public void testVersion() throws Exception {
        ApplicationInfo info = ApplicationInfo.getInstance();
        assertEquals(new DefaultMetadataVersion(info.getReleaseNumber()), getMetadataInstance().getVersion());
    }

    @Test
    public void shouldLoadSampleVdb() throws Exception {
        loadSample();
    }

    @Test
    public void shouldReturnSwaggerSpec() throws Exception {

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path("swagger.json").build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        assertTrue(entity.contains("\"swagger\""));
        assertTrue(entity.contains("\"/service/about\""));
        assertTrue(entity.contains("\"/service/samples\""));
        assertTrue(entity.contains("\"/service/schema\""));
        assertTrue(entity.contains("\"/workspace/vdbs\""));
        assertTrue(entity.contains("\"/workspace/vdbs/{vdbName}\""));
        assertTrue(entity.contains("\"keng__id\""));
        assertTrue(entity.contains("\"keng__kType\""));
    }

    @Test
    public void shouldGetTeiidStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataStatus status = KomodoJsonMarshaller.unmarshall(entity, RestMetadataStatus.class);
        assertNotNull(status);

        assertTrue(status.isAvailable());
        assertEquals(getMetadataInstance().getDataSources().size(), status.getDataSourceSize());
        assertEquals(getMetadataInstance().getDataSourceDrivers().size(), status.getDataSourceDriverSize());

        testTranslators(status);

        assertEquals(getMetadataInstance().getVdbs().size(), status.getVdbSize());
    }

    @Test
    public void shouldGetTeiidVdbStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataVdbStatus status = KomodoJsonMarshaller.unmarshall(entity, RestMetadataVdbStatus.class);
        assertNotNull(status);

        List<RestMetadataVdbStatusVdb> vdbProperties = status.getVdbProperties();
        assertTrue(vdbProperties.size() > 0);

        RestMetadataVdbStatusVdb vdb = vdbProperties.get(0);
        assertNotNull(vdb);
        
        assertEquals("sample", vdb.getName());
        assertEquals("1", vdb.getVersion());
        //
        // TODO
        // sample is not currently active due to no loopback translator being installed
        // once this has been fixed then this can be uncommented
        //
//        assertTrue(vdb.isActive());
//        assertFalse(vdb.isLoading());
//        assertFalse(vdb.isFailed());
//        assertEquals(0, vdb.getErrors().size());
    }

    @SuppressWarnings( "incomplete-switch" )
    @Test
    public void shouldGetVdbs() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestMetadataVdb[].class);
        assertFalse(vdbs.length == 0);

        RestMetadataVdb vdb = vdbs[0];
        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH +
                                             V1Constants.METADATA_SEGMENT + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT + FORWARD_SLASH +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH +
                                             V1Constants.METADATA_SEGMENT + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT,
                                             link.getHref().toString());
            }
        }
    }

    @SuppressWarnings( "incomplete-switch" )
    @Test
    public void shouldGetVdb() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .path(TestUtilities.SAMPLE_VDB_NAME)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataVdb vdb = KomodoJsonMarshaller.unmarshall(entity, RestMetadataVdb.class);
        assertNotNull(vdb);

        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH +
                                             V1Constants.METADATA_SEGMENT + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT + FORWARD_SLASH +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH +
                                             V1Constants.METADATA_SEGMENT + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT,
                                             link.getHref().toString());
            }
        }
    }

    @Test
    public void shouldGetTeiidStatusMultiQueries() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .build();

        int iterations = 3;
        CountDownLatch latch = new CountDownLatch(iterations);
        List<Throwable> assertionFailures = new ArrayList<Throwable>();

        for (int i = 0; i < iterations; ++i) {
            Runnable runnable = new Runnable() {

                @Override
                public void run() {
                    try {
                        HttpGet request = jsonRequest(uri, RequestType.GET);
                        HttpResponse response = execute(request);

                        Thread.yield();

                        okResponse(response);
                        //
                        // Don't want the thread dying since the latch will never
                        // countdown and the test will be stuck for 3 minutes
                        // waiting to timeout.
                        // Better to add the assertion errors into a bucket and once
                        // the countdown has been completed, check the bucket for
                        // errors. Don't really care if there is more than one, just that
                        // there is one, the test should fail
                        //
                        String entity = extractResponse(response);
                        RestMetadataStatus status = KomodoJsonMarshaller.unmarshall(entity, RestMetadataStatus.class);
                        assertNotNull(status);

                        assertTrue(status.isAvailable());
                        assertEquals(getMetadataInstance().getDataSources().size(), status.getDataSourceSize());
                        assertEquals(getMetadataInstance().getDataSourceDrivers().size(), status.getDataSourceDriverSize());
                        testTranslators(status);
                        assertEquals(getMetadataInstance().getVdbs().size(), status.getVdbSize());
                    } catch (Throwable ex) {
                        assertionFailures.add(ex);
                    } finally {
                        latch.countDown();
                    }
                }
            };

            Thread thread = new Thread(runnable);
            thread.start();
        }

        assertTrue(latch.await(3, TimeUnit.MINUTES));
        for (Throwable t : assertionFailures) {
            // Give a clue as to what failed
            t.printStackTrace();
        }
        assertTrue(assertionFailures.isEmpty());
    }

    @Test
    public void shouldGetTranslators() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TRANSLATORS_SEGMENT)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestMetadataVdbTranslator[].class);
        assertTrue(translators.length > 0);

        for (RestMetadataVdbTranslator translator : translators) {
            assertNotNull(translator.getId());
            assertEquals(2, translator.getLinks().size());
        }
    }

    @Test
    public void shouldGetConnections() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.CONNECTIONS_SEGMENT)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entity, RestMetadataConnection[].class);
        //
        // TODO
        // No connections currently deployed.
        // If this becomes a major issue then may need to deploy one specially
        // just to ensure something tangible is returned
        //
        assertNotNull(connections);
    }

    @Test
    public void shouldGetConnectionTemplates() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataTemplate[] templates = KomodoJsonMarshaller.unmarshallArray(entity, RestMetadataTemplate[].class);
        assertTrue(templates.length > 0);

        for (RestMetadataTemplate template : templates) {
            assertNotNull(template.getId());
            assertFalse(template.getEntries().isEmpty());
            assertEquals(3, template.getLinks().size());
        }
    }

    @Test
    public void shouldGetConnectionTemplate() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .path("teiid")
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataTemplate template = KomodoJsonMarshaller.unmarshall(entity, RestMetadataTemplate.class);
        assertNotNull(template);

        assertNotNull(template.getId());
        assertFalse(template.getEntries().isEmpty());
        assertEquals(3, template.getLinks().size());
    }

    @Test
    public void shouldGetConnectionTemplateEntries() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .path("teiid")
                                          .path(V1Constants.TEMPLATE_ENTRIES_SEGMENT)
                                          .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        RestMetadataTemplateEntry[] templateEntries = KomodoJsonMarshaller.unmarshallArray(entity, RestMetadataTemplateEntry[].class);
        assertTrue(templateEntries.length > 0);

        for (RestMetadataTemplateEntry entry : templateEntries) {
            assertNotNull(entry.getId());
            assertEquals(2, entry.getLinks().size());
        }
    }

    @Test
    public void shouldGetDataSourceDrivers() throws Exception {
        Collection<ConnectionDriver> dataSourceDrivers = getMetadataInstance().getDataSourceDrivers();
        assertTrue(dataSourceDrivers.size() > 0);

        String[] driverNamesArr = {"h2", "teiid"};
        List<String> driverNames = Arrays.asList(driverNamesArr);

        Iterator<ConnectionDriver> iter = dataSourceDrivers.iterator();
        int found = 0;
        while (iter.hasNext()) {
            ConnectionDriver driver = iter.next();
            if (driverNames.contains(driver.getName()))
                found++;
        }

        assertEquals(driverNamesArr.length, found);
    }

    @Test
    public void shouldFailToGetSchema() throws Exception {
        try {
            getMetadataInstance().getSchema("blah", "1.0", "model");
        } catch (Exception ex) {
            //
            // Should throw this exception since blah does not exist but should not
            // throw a NumberFormatException or NoSuchMethodException
            //
            Throwable cause = ex.getCause();
            assertNotNull(cause);
            assertTrue(cause instanceof AdminProcessingException);
            assertTrue(cause.getMessage().contains("does not exist or is not ACTIVE"));
        }
    }

    @Test
    public void shouldAbout() throws Exception {
        String[] EXPECTED = {
                "\"Information\": " +  OPEN_BRACE + NEW_LINE,
                "\"Repository Workspace\": \"komodoLocalWorkspace\"," + NEW_LINE,
                "\"Repository Configuration\"", // Configuration Url contains local file names so impossible to test
                "\"Repository Vdb Total\":",
            };

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.ABOUT).build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");

        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        okResponse(response);
        for (String expected : EXPECTED) {
            assertTrue(expected + " is not contained in " + entity, entity.contains(expected));
        }
    }
}
