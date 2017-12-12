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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import javax.ws.rs.core.MediaType;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.komodo.rest.KRestEntity;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.PersistenceType;
import org.komodo.utils.FileUtils;

public class AbstractFrameworkTest implements StringConstants, V1Constants {

    public static enum RequestType {
        GET,
        POST,
        PUT,
        DELETE
    }

    protected static final int TEST_PORT = 8080;

    protected static final String USER_NAME = KomodoService.KOMODO_USER;
    
    protected static final String PASSWORD = "user";

    protected static Path _kengineDataDir;

    protected static KomodoRestUriBuilder _uriBuilder;

    public AbstractFrameworkTest() {
        super();
    }

    @BeforeClass
    public static void beforeAllFramework() throws Exception {
        _kengineDataDir = Files.createTempDirectory(null, new FileAttribute[0]);
        System.setProperty(SystemConstants.ENGINE_DATA_DIR, _kengineDataDir.toString());

        //
        // Sets the persistence type to H2 for test purposes
        //
        System.setProperty(SystemConstants.REPOSITORY_PERSISTENCE_TYPE, PersistenceType.H2.name());
    }

    @AfterClass
    public static void afterAll() throws Exception {
        if (_kengineDataDir != null) {
            FileUtils.removeDirectoryAndChildren(_kengineDataDir.toFile());
        }

        try {
            Files.deleteIfExists(_kengineDataDir);
        } catch (Exception ex) {
            _kengineDataDir.toFile().deleteOnExit();
        }
    }

    protected HttpClient requestClient() {
        CloseableHttpClient httpClient = HttpClients.createDefault();
        return httpClient;
    }

    protected HttpResponse execute(HttpUriRequest request) throws Exception {
        return requestClient().execute(request);
    }

    protected HttpResponse executeOk(HttpUriRequest request) throws Exception {
        HttpResponse response = execute(request);
        okResponse(response);
        return response;
    }

    protected void addHeader(HttpUriRequest request, String name, Object value) {
        assertNotNull(name);
        assertNotNull(value);
        request.setHeader(name, value.toString());
    }

    protected void addJsonConsumeContentType(HttpUriRequest request) {
        //
        // Have to add this as the REST operation has a @Consumes annotation
        //
        addHeader(request, HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON);
    }

    protected void addXmlConsumeContentType(HttpUriRequest request) {
        //
        // Have to add this as the REST operation has a @Consumes annotation
        //
        addHeader(request, HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_XML);
    }

    protected void addBody(HttpEntityEnclosingRequestBase request, String bodyText) throws UnsupportedEncodingException {
        HttpEntity requestEntity = new StringEntity(bodyText, ContentType.APPLICATION_JSON);
        request.setEntity(requestEntity);
    }

    protected void addBody(HttpEntityEnclosingRequestBase request, KRestEntity bodyObject) throws UnsupportedEncodingException {
        String body = KomodoJsonMarshaller.marshall(bodyObject);
        HttpEntity requestEntity = new StringEntity(body, ContentType.APPLICATION_JSON);
        request.setEntity(requestEntity);
    }

    protected void addBody(HttpEntityEnclosingRequestBase request, KRestEntity[] bodyObject) throws UnsupportedEncodingException {
        String body = KomodoJsonMarshaller.marshallArray(bodyObject, false);
        HttpEntity requestEntity = new StringEntity(body, ContentType.APPLICATION_JSON);
        request.setEntity(requestEntity);
    }

    @SuppressWarnings( "unchecked" )
    protected <T extends HttpUriRequest> T request(URI uri, RequestType requestType, MediaType type) throws Exception {
        switch (requestType) {
            case GET: {
                HttpGet request = new HttpGet(uri);
                if (type != null) request.setHeader(HttpHeaders.ACCEPT, type.toString());
                return (T) request;
            }
            case POST: {
                HttpPost request = new HttpPost(uri);
                if (type != null) request.setHeader(HttpHeaders.ACCEPT, type.toString());
                return (T) request;
            }
            case PUT: {
                HttpPut request = new HttpPut(uri);
                if (type != null) request.setHeader(HttpHeaders.ACCEPT, type.toString());
                return (T) request;
            }
            case DELETE: {
                HttpDelete request = new HttpDelete(uri);
                if (type != null) request.setHeader(HttpHeaders.ACCEPT, type.toString());
                return (T) request;
            }
            default:
                fail("Request type " + requestType + " not supported");
                return null;
        }
    }

    protected <T extends HttpUriRequest> T jsonRequest(URI uri, RequestType requestType) throws Exception {
        return request(uri, requestType, MediaType.APPLICATION_JSON_TYPE);
    }

    protected String extractResponse(HttpResponse response) throws IOException {
        assertNotNull(response);
        HttpEntity entity = response.getEntity();
        assertNotNull(entity);
    
        String value = EntityUtils.toString(entity);
        assertNotNull(value);
        return value;
    }

    protected void assertResponse(HttpResponse response, int expectedStatusCode) throws Exception {
        assertNotNull(response);
        StatusLine status = response.getStatusLine();
    
        // Rather do this than AssertEquals since it will show the response body in the failure
        if (expectedStatusCode != status.getStatusCode()) {
            String value = EntityUtils.toString(response.getEntity());
            fail(status + COLON + SPACE + value);
        }
    }

    protected void okResponse(HttpResponse response) throws Exception {
        assertNotNull(response);
        StatusLine status = response.getStatusLine();
    
        if (HttpStatus.SC_OK != status.getStatusCode()) {
            String value = EntityUtils.toString(response.getEntity());
            fail(status + COLON + SPACE + value);
        }
    }

}
