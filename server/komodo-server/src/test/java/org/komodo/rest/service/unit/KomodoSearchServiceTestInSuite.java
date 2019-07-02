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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.net.URI;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.junit.Before;
import org.junit.Test;
import org.komodo.importer.ImportMessages;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RestEntityFactory;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoSearcherAttributes;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoSearchServiceTestInSuite extends AbstractKomodoServiceTest {

    @Before
    public void setup() throws Exception{
        ImportMessages msgs = importVdb(TestUtilities.portfolioExample(), AbstractServiceTest.USER_NAME);
        assertTrue(msgs.getErrorMessages().isEmpty());
    }

    @Test
    public void shouldFailNoParameters() throws Exception {
        // get
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
//        System.out.println("Response:\n" + entity);

        JsonElement jelement = new JsonParser().parse(entity);
        assertNotNull(jelement);
        JsonObject  jobject = jelement.getAsJsonObject();
        assertEquals("\"The search service requires at least one parameter\"", jobject.get("error").toString());
    }

    @Test
    public void shouldSearchForAnythingContainingView() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_CONTAINS_PARAMETER, "view");
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertNotEquals(KomodoType.UNKNOWN, e.getkType());
        }
    }

    @Test
    public void shouldSearchForAnyModelContainingView() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        properties.addProperty(SEARCH_CONTAINS_PARAMETER, "view");
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
//        assertEquals(5, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
        }
    }

    @Test
    public void shouldSearchForAnyModelUnderPortfolioContainingView() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        properties.addProperty(SEARCH_PARENT_PARAMETER, PORTFOLIO_DATA_PATH);
        properties.addProperty(SEARCH_CONTAINS_PARAMETER, "view");
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(2, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
            assertTrue(e.getDataPath().startsWith(PORTFOLIO_DATA_PATH));
        }
    }

    @Test
    public void shouldSearchByPath() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_PATH_PARAMETER, PORTFOLIO_DATA_PATH);
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(1, entities.length);

        RestBasicEntity basicEntity = entities[0];
        RestVdb vdb = RestEntityFactory.resolve(basicEntity, RestVdb.class);
        assertNotNull(vdb);

        assertPortfolio(vdb);
    }

    @Test
    public void shouldSearchByParent() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_PARENT_PARAMETER, PORTFOLIO_DATA_PATH);
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(5, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            RestVdbModel model = RestEntityFactory.resolve(basicEntity, RestVdbModel.class);
            assertNotNull(model);
        }
    }

    @Test
    public void shouldSearchByAncestor() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_ANCESTOR_PARAMETER, PORTFOLIO_DATA_PATH);
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(110, entities.length);
    }

    @Test
    public void shouldSearchByType() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldSearchByKType() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, KomodoType.COLUMN.getType());
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldSearchByKTypeAndLocalName() throws Exception {

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, KomodoType.COLUMN.getType());
        properties.addProperty(SEARCH_OBJECT_NAME_PARAMETER, "%ID%");
        URI uri = uriBuilder().searchUri(properties);

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
            assertTrue(basicEntity.getId().contains("ID"));
            // System.out.println(basicEntity.getDataPath());
        }
    }

    @Test
    public void shouldAdvancedSearchForAnythingContainingView() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setContains("view");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertNotEquals(KomodoType.UNKNOWN, e.getkType());
        }
    }

    @Test
    public void shouldAdvancedSearchForAnyModelContainingView() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        searchAttr.setContains("view");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
        }
    }

    @Test
    public void shouldAdvancedSearchForAnyModelUnderPortfolioContainingView() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        searchAttr.setParent(PORTFOLIO_DATA_PATH);
        searchAttr.setContains("view");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(2, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
            assertTrue(e.getDataPath().startsWith(PORTFOLIO_DATA_PATH));
        }
    }

    @Test
    public void shouldAdvancedSearchByPath() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setPath(PORTFOLIO_DATA_PATH);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
//        System.out.println("Response:\n" + entity);
        assertEquals(HttpStatus.SC_OK, response.getStatusLine().getStatusCode());

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(1, entities.length);

        RestBasicEntity basicEntity = entities[0];
        RestVdb vdb = RestEntityFactory.resolve(basicEntity, RestVdb.class);
        assertNotNull(vdb);

        assertPortfolio(vdb);
    }

    @Test
    public void shouldAdvancedSearchByParent() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setParent(PORTFOLIO_DATA_PATH);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(5, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            RestVdbModel model = RestEntityFactory.resolve(basicEntity, RestVdbModel.class);
            assertNotNull(model);
        }
    }

    @Test
    public void shouldAdvancedSearchByAncestor() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setAncestor(PORTFOLIO_DATA_PATH);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(110, entities.length);
    }

    @Test
    public void shouldAdvancedSearchByType() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
//        System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldAdvancedSearchByKType() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(KomodoType.COLUMN.getType());

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldAdvancedSearchByKTypeAndLocalName() throws Exception {

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = uriBuilder().searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(KomodoType.COLUMN.getType());
        searchAttr.setObjectName("%ID%");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertTrue(entities.length > 0);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
            assertTrue(basicEntity.getId().contains("ID"));
            // System.out.println(basicEntity.getDataPath());
        }
    }

}
