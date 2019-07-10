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
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.ws.rs.core.UriBuilder;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.junit.Test;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.cors.CorsHeaders;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.version.DefaultMetadataVersion;
import org.teiid.adminapi.AdminProcessingException;
import org.teiid.core.util.ApplicationInfo;

@SuppressWarnings( {"javadoc", "nls"} )
public class IT_KomodoMetadataServiceGetTests extends AbstractKomodoMetadataServiceTest {

    @Override
    protected int getTestTotalInClass() {
        return 5;
    }

    @Test
    public void testVersion() throws Exception {
        ApplicationInfo info = ApplicationInfo.getInstance();
        assertEquals(new DefaultMetadataVersion(info.getReleaseNumber()), getMetadataInstance().getVersion());
    }

    @Test
    public void shouldReturnSwaggerSpec() throws Exception {

        // get
        URI uri = UriBuilder.fromUri(getUriBuilder().baseUri())
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
        URI uri = UriBuilder.fromUri(getUriBuilder().baseUri())
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
