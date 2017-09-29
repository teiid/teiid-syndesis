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
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.cors.CorsHeaders;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.connection.RestTemplate;
import org.komodo.rest.relational.connection.RestTemplateEntry;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.RestMetadataStatus;
import org.komodo.rest.relational.response.RestMetadataVdb;
import org.komodo.rest.relational.response.RestMetadataVdbStatus;
import org.komodo.rest.relational.response.RestMetadataVdbStatusVdb;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;
import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public final class IT_KomodoTeiidServiceGetTests extends AbstractKomodoTeiidServiceTest implements StringConstants {

    private void testTranslators(RestMetadataStatus status) {
        assertEquals(56, status.getTranslatorSize());
    }

    @Override
    protected int getTestTotalInClass() {
        return 12;
    }

    @Test
    public void shouldLoadSampleVdb() throws Exception {
        loadSample();
    }

    @Test
    @Ignore("Need to implement getDataSources in DefaultMetadataInstance")
    public void shouldGetTeiidStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        checkResponse(response);

        RestMetadataStatus status = KomodoJsonMarshaller.unmarshall(entity, RestMetadataStatus.class);
        assertNotNull(status);

        assertTrue(status.isAvailable());
        assertEquals(1, status.getDataSourceSize());
        assertEquals(3, status.getDataSourceDriverSize());

        testTranslators(status);

        assertEquals(1, status.getVdbSize());
    }

    @Test
    public void shouldGetTeiidVdbStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        checkResponse(response);

        RestMetadataVdbStatus status = KomodoJsonMarshaller.unmarshall(entity, RestMetadataVdbStatus.class);
        assertNotNull(status);

        List<RestMetadataVdbStatusVdb> vdbProperties = status.getVdbProperties();
        assertEquals(1, vdbProperties.size());

        RestMetadataVdbStatusVdb vdb = vdbProperties.get(0);
        assertNotNull(vdb);
        
        assertEquals("sample", vdb.getName());
        assertEquals("1", vdb.getVersion());
        assertTrue(vdb.isActive());
        assertFalse(vdb.isLoading());
        assertFalse(vdb.isFailed());
        assertEquals(0, vdb.getErrors().size());
    }

    @SuppressWarnings( "incomplete-switch" )
    @Test
    public void shouldGetVdbs() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        checkResponse(response);

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
                    assertEquals(BASE_URI + FORWARD_SLASH +
                                             V1Constants.METADATA_SEGMENT + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT + FORWARD_SLASH +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + FORWARD_SLASH +
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

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        checkResponse(response);
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
                    assertEquals(BASE_URI + FORWARD_SLASH +
                                             V1Constants.METADATA_SEGMENT + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT + FORWARD_SLASH +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + FORWARD_SLASH +
                                             V1Constants.METADATA_SEGMENT + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT,
                                             link.getHref().toString());
            }
        }
    }

    @Test
    @Ignore("Need to implement getDataSources in DefaultMetadataInstance")
    public void shouldGetTeiidStatusMultiQueries() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .build();

        int iterations = 3;
        final CountDownLatch latch = new CountDownLatch(iterations);
        final List<Throwable> assertionFailures = new ArrayList<Throwable>();

        for (int i = 0; i < iterations; ++i) {
            Runnable runnable = new Runnable() {

                @Override
                public void run() {
                    try {
                        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
                        ClientResponse<String> response = request.get(String.class);

                        Thread.yield();

                        String entity = response.getEntity();
                        checkResponse(response);
                        //
                        // Don't want the thread dying since the latch will never
                        // countdown and the test will be stuck for 3 minutes
                        // waiting to timeout.
                        // Better to add the assertion errors into a bucket and once
                        // the countdown has been completed, check the bucket for
                        // errors. Don't really care if there is more than one, just that
                        // there is one, the test should fail
                        //
                        RestMetadataStatus status = KomodoJsonMarshaller.unmarshall(entity, RestMetadataStatus.class);
                        assertNotNull(status);

                        assertTrue(status.isAvailable());
                        assertEquals(1, status.getDataSourceSize());
                        assertEquals(3, status.getDataSourceDriverSize());
                        testTranslators(status);
                        assertEquals(1, status.getVdbSize());
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
    @Ignore("Not implemented yet")
    public void shouldGetTranslators() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TRANSLATORS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        checkResponse(response);

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertTrue(translators.length > 0);

        for (RestVdbTranslator translator : translators) {
            assertNotNull(translator.getId());
            assertEquals(3, translator.getLinks().size());
        }
    }

    @Test
    @Ignore("Connections not implemented yet")
    public void shouldGetConnections() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.CONNECTIONS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        checkResponse(response);

        RestConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entity, RestConnection[].class);
        assertTrue(connections.length > 0);

        for (RestConnection connection : connections) {
            assertNotNull(connection.getId());
            assertEquals(3, connection.getLinks().size());
        }
    }

    @Test
    @Ignore("Connections not implemented yet")
    public void shouldGetConnectionTemplates() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        assertEquals(200, response.getStatus());

        RestTemplate[] templates = KomodoJsonMarshaller.unmarshallArray(entity, RestTemplate[].class);
        assertTrue(templates.length > 0);

        for (RestTemplate template : templates) {
            assertNotNull(template.getId());
            assertFalse(template.getEntries().isEmpty());
            assertEquals(4, template.getLinks().size());
        }
    }

    @Test
    @Ignore("Connections not implemented yet")
    public void shouldGetConnectionTemplate() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .path("webservice")
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        assertEquals(200, response.getStatus());

        RestTemplate template = KomodoJsonMarshaller.unmarshall(entity, RestTemplate.class);
        assertNotNull(template);

        assertNotNull(template.getId());
        assertFalse(template.getEntries().isEmpty());
        assertEquals(4, template.getLinks().size());
    }

    @Test
    @Ignore("Connections not implemented yet")
    public void shouldGetConnectionTemplateEntries() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.METADATA_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .path("webservice")
                                          .path(V1Constants.TEMPLATE_ENTRIES_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        assertEquals(200, response.getStatus());

        RestTemplateEntry[] templateEntries = KomodoJsonMarshaller.unmarshallArray(entity, RestTemplateEntry[].class);
        assertTrue(templateEntries.length > 0);

        for (RestTemplateEntry entry : templateEntries) {
            assertNotNull(entry.getId());
            assertEquals(2, entry.getLinks().size());
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

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");

        ClientResponse<String> response = request.get(String.class);
        assertNotNull(response.getEntity());
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        final String entity = response.getEntity();
        checkResponse(response);
        for (String expected : EXPECTED) {
            assertTrue(expected + " is not contained in " + entity, entity.contains(expected));
        }
    }
}
