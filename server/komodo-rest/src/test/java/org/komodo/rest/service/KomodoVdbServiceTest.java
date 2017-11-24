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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.StringStartsWith.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.relational.model.Model.Type;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbCondition;
import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.rest.relational.response.RestVdbMask;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.relational.response.RestVdbModelSource;
import org.komodo.rest.relational.response.RestVdbModelTable;
import org.komodo.rest.relational.response.RestVdbModelTableColumn;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoVdbServiceTest extends AbstractKomodoServiceTest {

    @Rule
    public TestName testName = new TestName();

    @Test
    public void shouldGetVdbs() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.workspaceVdbsUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);

        // System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(4, vdbs.length);

        boolean foundPortfolio = false;
        for (RestVdb vdb : vdbs) {
            assertNotNull(vdb.getId());
            assertNotNull(vdb.getDataPath());
            assertNotNull(vdb.getkType());

            if (TestUtilities.PORTFOLIO_VDB_NAME.equals(vdb.getId())) {
                foundPortfolio = true;
                assertPortfolio(vdb);
            }
        }

        assertTrue(foundPortfolio);

    }

    @Test
    public void shouldNotGetVdbsXml() throws Exception {
        loadVdbs();

        URI uri = _uriBuilder.workspaceVdbsUri();
        HttpGet request = request(uri, RequestType.GET, MediaType.APPLICATION_XML_TYPE);
        HttpResponse response = execute(request);

        //
        // Internal server error since the server does not support
        // '/vdbs' url returning anything in xml
        //
        assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);

        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        assertTrue(entity.contains("No match for accept header"));
    }

    //    @Test
    //    public void shouldDeleteVdb() throws Exception {
    //        RestVdb restVdb = createVdbs( 1 )[ 0 ];
    //        response = request( _uriBuilder.buildVdbUri( LinkType.DELETE, restVdb.getName() ) ).delete();
    //        assertThat( response.getStatus(), is( Status.NO_CONTENT.getStatusCode() ) );
    //    }

    @Test
    public void shouldCreateModel() throws Exception {
        String vdbName = "blah";

        { // create VDB first
            Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
            _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
            URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

            RestVdb vdb = new RestVdb();
            vdb.setName(vdbName);
            vdb.setDescription("blah VDB description");
            vdb.setOriginalFilePath("/Users/elvis/vdbs/blah.vdb");
            vdb.setPreview(false);
            vdb.setConnectionType("connType");
            vdb.setVersion(1);
            vdb.setId(vdbName);
            vdb.setkType(KomodoType.VDB);
            vdb.setDataPath("/tko:komodo/tko:workspace/" + USER_NAME + "/blah");

            String json = KomodoJsonMarshaller.marshall(vdb);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);

            executeOk(request);
        }

        String modelName = "elvis";
        String dataPath = "/tko:komodo/tko:workspace/" + USER_NAME + "/blah/elvis";
        String ddl = "CREATE VIEW Tweet AS select * FROM twitterview.getTweets;";
        String description = "model description goes here";
        String id = modelName;
        KomodoType kType = KomodoType.MODEL;
        String metadataType = "DDL";
        Type modelType = Type.VIRTUAL;
        boolean visible = true;

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, modelName);
        URI uri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);

        { // create model
            RestVdbModel inModel = new RestVdbModel();
            inModel.setDataPath(dataPath);
            inModel.setDdl(ddl);
            inModel.setDescription(description);
            inModel.setId(id);
            inModel.setkType(kType);
            inModel.setMetadataType(metadataType);
            inModel.setModelType(modelType);
            inModel.setVisible(visible);

            String json = KomodoJsonMarshaller.marshall(inModel);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);

            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);
            assertThat(entity, is(notNullValue()));

            RestVdbModel outModel = KomodoJsonMarshaller.unmarshall(entity, RestVdbModel.class);
            assertNotNull(outModel);
            assertThat(outModel.getDataPath(), is(dataPath));
            assertThat(outModel.getDescription(), is(description));
            // can't check DDL because sequencer has not run yet
            assertThat(outModel.getId(), is(id));
            assertThat(outModel.getkType(), is(kType));
            assertThat(outModel.getMetadataType(), is(metadataType));
            assertThat(outModel.getModelType(), is(modelType));
            assertThat(outModel.isVisible(), is(visible));
        }

        { // get new model
            HttpGet request = jsonRequest(uri, RequestType.GET);
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);
            assertThat(entity, is(notNullValue()));

            RestVdbModel newModel = KomodoJsonMarshaller.unmarshall(entity, RestVdbModel.class);
            assertNotNull(newModel);
            assertThat(newModel.getDataPath(), is(dataPath));
            assertThat(newModel.getDescription(), is(description));
            assertThat(newModel.getId(), is(id));
            assertThat(newModel.getkType(), is(kType));
            assertThat(newModel.getMetadataType(), is(metadataType));
            assertThat(newModel.getModelType(), is(modelType));
            assertThat(newModel.isVisible(), is(visible));

            // remove any formatting before comparing
            String actual = newModel.getDdl().toLowerCase().replace('\n', ' ').trim();
            String expected = ddl.toLowerCase().replace('\n', ' ').trim();
            assertThat(actual, is(expected));
        }
    }

    @Test
    public void shouldCreateModelSource() throws Exception {
        String vdbName = "blah";

        { // create VDB
            Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
            _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
            URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

            RestVdb vdb = new RestVdb();
            vdb.setName(vdbName);
            vdb.setDescription("blah VDB description");
            vdb.setOriginalFilePath("/Users/elvis/vdbs/blah.vdb");
            vdb.setPreview(false);
            vdb.setConnectionType("connType");
            vdb.setVersion(1);
            vdb.setId(vdbName);
            vdb.setkType(KomodoType.VDB);
            vdb.setDataPath("/tko:komodo/tko:workspace/" + USER_NAME + "/blah");

            String json = KomodoJsonMarshaller.marshall(vdb);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);

            executeOk(request);
        }

        String modelName = "elvis";

        { // create model
            RestVdbModel model = new RestVdbModel();
            model.setDataPath("/tko:komodo/tko:workspace/" + USER_NAME + "/blah/elvis");
            model.setDdl("CREATE VIEW Tweet AS select * FROM twitterview.getTweets;");
            model.setDescription("model description goes here");
            model.setId(modelName);
            model.setkType(KomodoType.MODEL);
            model.setMetadataType("DDL");
            model.setModelType(Type.VIRTUAL);
            model.setVisible(true);

            Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
            _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
            _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, modelName);
            URI uri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);

            String json = KomodoJsonMarshaller.marshall(model);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);

            executeOk(request);
        }

        String modelSourceName = "sledge";
        String id = modelSourceName;
        String dataPath = "/tko:komodo/tko:workspace/" + USER_NAME + "/blah/elvis/vdb:sources/sledge";
        KomodoType kType = KomodoType.VDB_MODEL_SOURCE;
        String jndiName = "myJndiName";
        String translator = "myTranslator";

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, modelName);
        _uriBuilder.addSetting(settings, SettingNames.SOURCE_NAME, modelSourceName);
        URI uri = _uriBuilder.vdbModelSourceUri(LinkType.SELF, settings);

        { // create model source
            RestVdbModelSource inModelSource = new RestVdbModelSource();
            inModelSource.setDataPath(dataPath);
            inModelSource.setId(id);
            inModelSource.setkType(kType);
            inModelSource.setJndiName(jndiName);
            inModelSource.setTranslator(translator);

            String json = KomodoJsonMarshaller.marshall(inModelSource);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);

            HttpResponse response = executeOk(request);
            String entity = extractResponse(response);

            RestVdbModelSource outModelSource = KomodoJsonMarshaller.unmarshall(entity, RestVdbModelSource.class);
            assertNotNull(outModelSource);
            assertThat(outModelSource.getDataPath(), is(dataPath));
            assertThat(outModelSource.getId(), is(id));
            assertThat(outModelSource.getkType(), is(kType));
            assertThat(outModelSource.getJndiName(), is(jndiName));
            assertThat(outModelSource.getTranslator(), is(translator));
        }

        { // get new model source
            HttpGet request = jsonRequest(uri, RequestType.GET);
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);

            RestVdbModelSource newModelSource = KomodoJsonMarshaller.unmarshall(entity, RestVdbModelSource.class);
            assertNotNull(newModelSource);
            assertThat(newModelSource.getDataPath(), is(dataPath));
            assertThat(newModelSource.getId(), is(id));
            assertThat(newModelSource.getkType(), is(kType));
            assertThat(newModelSource.getJndiName(), is(jndiName));
            assertThat(newModelSource.getTranslator(), is(translator));
        }
    }

    @Test
    public void shouldCreateVdb() throws Exception {
        String vdbName = "blah";
        String description = "blah VDB description";
        String originalFilePath = "/Users/elvis/vdbs/blah.vdb";
        boolean preview = false;
        String connectionType = "connType";
        int version = 5;
        String id = vdbName;
        KomodoType kType = KomodoType.VDB;
        String dataPath = "/tko:komodo/tko:workspace/" + USER_NAME + "/blah";

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

        { // create
            RestVdb inVdb = new RestVdb();
            inVdb.setName(vdbName);
            inVdb.setDescription(description);
            inVdb.setOriginalFilePath(originalFilePath);
            inVdb.setPreview(preview);
            inVdb.setConnectionType(connectionType);
            inVdb.setVersion(version);
            inVdb.setId(id);
            inVdb.setkType(kType);
            inVdb.setDataPath(dataPath);

            String json = KomodoJsonMarshaller.marshall(inVdb);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);

            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);

            // verify response is new VDB
            RestVdb outVdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
            assertNotNull(outVdb);
            assertThat(outVdb.getName(), is(vdbName));
            assertThat(outVdb.getDescription(), is(description));
            assertThat(outVdb.getOriginalFilePath(), is(originalFilePath));
            assertThat(outVdb.isPreview(), is(preview));
            assertThat(outVdb.getConnectionType(), is(connectionType));
            assertThat(outVdb.getVersion(), is(version));
            assertThat(outVdb.getId(), is(id));
            assertThat(outVdb.getkType(), is(kType));
            assertThat(outVdb.getDataPath(), is(dataPath));
        }

        { // get new VDB
            HttpGet request = jsonRequest(uri, RequestType.GET);
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);

            RestVdb newVdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
            assertNotNull(newVdb);
            assertThat(newVdb.getName(), is(vdbName));
            assertThat(newVdb.getDescription(), is(description));
            assertThat(newVdb.getOriginalFilePath(), is(originalFilePath));
            assertThat(newVdb.isPreview(), is(preview));
            assertThat(newVdb.getConnectionType(), is(connectionType));
            assertThat(newVdb.getVersion(), is(version));
            assertThat(newVdb.getId(), is(id));
            assertThat(newVdb.getkType(), is(kType));
            assertThat(newVdb.getDataPath(), is(dataPath));
        }
    }

    @Test
    public void shouldGetVdb() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());

        URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdb vdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
        assertNotNull(vdb);

        assertPortfolio(vdb);
    }

    @Test
    public void shouldGetVdbXml() throws Exception {
        loadVdbs();

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

        HttpGet request = request(uri, RequestType.GET, MediaType.APPLICATION_XML_TYPE);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);

        System.out.println("Response:\n" + entity);
        assertTrue(entity.contains("<?xml version="));
        assertTrue(entity.contains("encoding="));
        assertTrue(entity.contains("<vdb name=\"Portfolio\" version=\"1\">"));
        assertTrue(entity.contains("</vdb>"));
    }

    @Test
    public void shouldGetVdbWhenPatternMatches() throws Exception {
        loadVdbs();

        String pattern = STAR + "P" + STAR;
        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri()).queryParam(KomodoVdbService.QueryParamKeys.PATTERN,
                                                                                pattern).build();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(2, vdbs.length);

        // Should return both MyPartsVDB_Dynamic and Portfoliio
        for (RestVdb vdb : vdbs) {
            if (TestUtilities.PARTS_VDB_NAME.equals(vdb.getName())) {
                // Not checking this one
            } else if (TestUtilities.PORTFOLIO_VDB_NAME.equals(vdb.getName()))
                assertPortfolio(vdb);
            else fail("Invalid VDB returned from search pattern " + pattern);
        }
    }

    @Test
    public void shouldLimitNumberOfVdbsReturnedWhenUsingSizeQueryParameter() throws Exception {
        loadVdbs();

        int resultSize = 3;

        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri()).queryParam(KomodoVdbService.QueryParamKeys.SIZE,
                                                                                resultSize).build();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(resultSize, vdbs.length);
    }

    @Test
    public void shouldLimitNumberOfVdbsReturnedWhenUsingStartQueryParameter() throws Exception {
        loadVdbs();

        int start = 3;

        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri()).queryParam(KomodoVdbService.QueryParamKeys.START,
                                                                                start).build();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(1, vdbs.length);
    }

    //
    //    @Test
    //    public void shouldNotDeleteVdb() throws Exception {
    //        response = request( _uriBuilder.buildVdbUri( LinkType.DELETE, "vdbDoesNotExist" ) ).delete();
    //        assertThat( response.getStatus(), is( Status.NOT_FOUND.getStatusCode() ) );
    //    }

    @Test
    public void shouldNotFindVdb() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri()).path("blah").build();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);
        assertResponse(response, HttpStatus.SC_NOT_FOUND);
    }

    @Test
    public void shouldReturnEmptyListWhenNoVdbsInWorkspace() throws Exception {
        URI uri = _uriBuilder.workspaceVdbsUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertNotNull(vdbs);
        assertEquals(0, vdbs.length);
    }

    @Test
    public void shouldGetVdbModels() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.MODELS);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbModel[] models = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbModel[].class);
        assertNotNull(models);
        assertEquals(5, models.length);
        for (RestVdbModel model : models) {
            assertEquals(KomodoType.MODEL, model.getkType());
            assertEquals(true, model.isVisible());

            if ("MarketData".equals(model.getId())) {
                assertEquals(Type.PHYSICAL, model.getModelType());
            } else if ("Accounts".equals(model.getId())) {
                assertEquals(Type.PHYSICAL, model.getModelType());
            } else if ("PersonalValuations".equals(model.getId())) {
                assertEquals(Type.PHYSICAL, model.getModelType());
            } else if ("Stocks".equals(model.getId())) {
                assertEquals(Type.VIRTUAL, model.getModelType());
            } else if ("StocksMatModel".equals(model.getId())) {
                assertEquals(Type.VIRTUAL, model.getModelType());
            } else fail("Model has invalid id");

            Collection<RestLink> links = model.getLinks();
            assertEquals(4, links.size());
        }
    }

    @Test
    public void shouldGetVdbModel() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        URI uri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        // System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbModel model = KomodoJsonMarshaller.unmarshall(entity, RestVdbModel.class);
        assertNotNull(model);

        assertEquals("PersonalValuations", model.getId());
        assertEquals(KomodoType.MODEL, model.getkType());
        assertEquals(true, model.isVisible());
        assertEquals(Type.PHYSICAL, model.getModelType());

        Collection<RestLink> links = model.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbModelSources() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        URI uri = _uriBuilder.vdbModelUri(LinkType.SOURCES, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entities);

        RestVdbModelSource[] sources = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelSource[].class);
        assertEquals(1, sources.length);

        RestVdbModelSource source = sources[0];

        assertEquals("excelconnector", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/excel-file", source.getJndiName());
        assertEquals("excel", source.getTranslator());

        Collection<RestLink> links = source.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbModelTables() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        URI modelUri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(LinkType.TABLES.uriName()).build();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);

        RestVdbModelTable[] tables = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelTable[].class);
        assertEquals(5, tables.length);

        List<String> tableNames = new ArrayList<String>();
        for (RestVdbModelTable table : tables) {
            tableNames.add(table.getId());
        }

        assertTrue(tableNames.contains("PARTS"));
        assertTrue(tableNames.contains("SHIP_VIA"));
        assertTrue(tableNames.contains("STATUS"));
        assertTrue(tableNames.contains("SUPPLIER"));
        assertTrue(tableNames.contains("SUPPLIER_PARTS"));
    }

    @Test
    public void shouldGetVdbModelTableColumns() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        _uriBuilder.addSetting(settings, SettingNames.TABLE_NAME, "SUPPLIER_PARTS");
        URI modelTableUri = _uriBuilder.vdbModelTableUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelTableUri).path(LinkType.COLUMNS.uriName()).build();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);

        RestVdbModelTableColumn[] columns = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelTableColumn[].class);
        assertEquals(4, columns.length);

        List<String> columnNames = new ArrayList<String>();
        Map<String, String> columnTypeMap = new HashMap<String, String>();
        for (RestVdbModelTableColumn column : columns) {
            columnNames.add(column.getId());
            columnTypeMap.put(column.getId(), column.getDatatypeName());
        }

        assertTrue(columnNames.contains("SUPPLIER_ID"));
        assertTrue(columnTypeMap.get("SUPPLIER_ID").equalsIgnoreCase("string"));
        assertTrue(columnNames.contains("PART_ID"));
        assertTrue(columnTypeMap.get("PART_ID").equalsIgnoreCase("string"));
        assertTrue(columnNames.contains("QUANTITY"));
        assertTrue(columnTypeMap.get("QUANTITY").equalsIgnoreCase("bigdecimal"));
        assertTrue(columnNames.contains("SHIPPER_ID"));
        assertTrue(columnTypeMap.get("SHIPPER_ID").equalsIgnoreCase("bigdecimal"));
    }

    @Test
    public void shouldGetVdbModelSourcesIncludeReferenceForTranslator() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.TWEET_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "twitter");
        URI uri = _uriBuilder.vdbModelUri(LinkType.SOURCES, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entities);

        RestVdbModelSource[] sources = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelSource[].class);
        assertEquals(1, sources.length);

        RestVdbModelSource source = sources[0];

        assertEquals("twitter", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/twitterDS", source.getJndiName());
        assertEquals("rest", source.getTranslator());

        Collection<RestLink> links = source.getLinks();
        assertEquals(4, links.size());

        RestLink refLink = null;
        for (RestLink link : links) {
            if (LinkType.REFERENCE.equals(link.getRel()))
                refLink = link;
        }
        assertNotNull(refLink);

        URI href = refLink.getHref();
        assertTrue(href.toString().endsWith("workspace/vdbs/twitter/VdbTranslators/rest"));
    }

    @Test
    public void shouldGetVdbModelSource() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        _uriBuilder.addSetting(settings, SettingNames.SOURCE_NAME, "excelconnector");
        URI uri = _uriBuilder.vdbModelSourceUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbModelSource source = KomodoJsonMarshaller.unmarshall(entity, RestVdbModelSource.class);
        assertNotNull(source);

        assertEquals("excelconnector", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/excel-file", source.getJndiName());
        assertEquals("excel", source.getTranslator());

        Collection<RestLink> links = source.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbTranslators() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.TWEET_EXAMPLE_VDB_NAME, LinkType.TRANSLATORS);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertNotNull(translators);
        assertEquals(1, translators.length);

        RestVdbTranslator translator = translators[0];

        assertEquals(KomodoType.VDB_TRANSLATOR, translator.getkType());
        assertEquals("ws", translator.getType());
        assertEquals("Rest Web Service translator", translator.getDescription());

        Collection<RestLink> links = translator.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbTranslatorsEmptyList() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.TRANSLATORS);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertNotNull(translators);
        assertEquals(0, translators.length);
    }

    @Test
    public void shouldGetVdbTranslator() throws Exception {
        loadVdbs();

        // get
        String vdbName = TestUtilities.TWEET_EXAMPLE_VDB_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);

        URI parentUri = _uriBuilder.vdbUri(_uriBuilder.workspaceVdbsUri(), vdbName);

        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, parentUri);
        _uriBuilder.addSetting(settings, SettingNames.TRANSLATOR_NAME, "rest");
        _uriBuilder.addSetting(settings, SettingNames.ADD_TRANSLATORS_SEGMENT, "true");
        URI uri = _uriBuilder.vdbTranslatorUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //        System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbTranslator translator = KomodoJsonMarshaller.unmarshall(entity, RestVdbTranslator.class);
        assertNotNull(translator);

        assertEquals(KomodoType.VDB_TRANSLATOR, translator.getkType());
        assertEquals("ws", translator.getType());
        assertEquals("Rest Web Service translator", translator.getDescription());

        Collection<RestLink> links = translator.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbImports() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME, LinkType.IMPORTS);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbImport[] imports = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbImport[].class);
        assertNotNull(imports);
        assertEquals(1, imports.length);

        RestVdbImport vdbImport = imports[0];

        assertEquals(KomodoType.VDB_IMPORT, vdbImport.getkType());
        assertEquals("x", vdbImport.getName());
        assertEquals(2, vdbImport.getVersion());
        assertEquals(false, vdbImport.isImportDataPolicies());

        Collection<RestLink> links = vdbImport.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbImportsEmptyList() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.IMPORTS);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbImport[] imports = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbImport[].class);
        assertNotNull(imports);
        assertEquals(0, imports.length);
    }

    @Test
    public void shouldGetVdbImport() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.IMPORT_NAME, "x");
        URI uri = _uriBuilder.vdbImportUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall(entity, RestVdbImport.class);
        assertNotNull(vdbImport);

        assertEquals(KomodoType.VDB_IMPORT, vdbImport.getkType());
        assertEquals("x", vdbImport.getName());
        assertEquals(2, vdbImport.getVersion());
        assertEquals(false, vdbImport.isImportDataPolicies());

        Collection<RestLink> links = vdbImport.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldCreateDataRole() throws Exception {
        loadVdbs();

        String dataRoleName = "MyDataRole";

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, dataRoleName);
        URI uri = _uriBuilder.vdbDataRoleUri(LinkType.SELF, settings);

        boolean allowCreateTempTables = true;
        boolean anyAuthenticated = true;
        String dataPath = "/tko:komodo/tko:workspace/" + USER_NAME + FORWARD_SLASH + TestUtilities.PORTFOLIO_VDB_NAME + FORWARD_SLASH
                                + VdbLexicon.Vdb.DATA_ROLES + FORWARD_SLASH + dataRoleName;
        String description = "My data role description";
        boolean grantAll = true;
        String[] mappedRoles = new String[] {"a", "b", "c", "d", "e"};
        KomodoType type = KomodoType.VDB_DATA_ROLE;
        String id = dataRoleName;

        String permissionName = "MyPermission";
        String permissionId = permissionName;
        KomodoType permissionType = KomodoType.VDB_PERMISSION;
        String permissionDataPath = dataPath + FORWARD_SLASH + VdbLexicon.DataRole.PERMISSIONS + FORWARD_SLASH + permissionName;
        boolean allowAlter = true;
        boolean allowCreate = false;
        boolean allowDelete = true;
        boolean allowExecute = false;
        boolean allowLanguage = true;
        boolean allowRead = false;
        boolean allowUpdate = true;

        { // create data role
            RestVdbDataRole inDataRole = new RestVdbDataRole();
            inDataRole.setAllowCreateTempTables(allowCreateTempTables);
            inDataRole.setAnyAuthenticated(anyAuthenticated);
            inDataRole.setDataPath(dataPath);
            inDataRole.setDescription(description);
            inDataRole.setGrantAll(grantAll);
            inDataRole.setId(id);
            inDataRole.setkType(type);
            inDataRole.setMappedRoles(mappedRoles);
            inDataRole.setName(dataRoleName);

            // add permission
            RestVdbPermission permission = new RestVdbPermission();
            permission.setName(permissionName);
            permission.setAllowAlter(allowAlter);
            permission.setAllowCreate(allowCreate);
            permission.setAllowDelete(allowDelete);
            permission.setAllowExecute(allowExecute);
            permission.setAllowLanguage(allowLanguage);
            permission.setAllowRead(allowRead);
            permission.setAllowUpdate(allowUpdate);
            permission.setId(permissionId);
            permission.setkType(permissionType);
            permission.setDataPath(permissionDataPath);
            inDataRole.setPermissions(new RestVdbPermission[] {permission});

            String json = KomodoJsonMarshaller.marshall(inDataRole);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);

            RestVdbDataRole outDataRole = KomodoJsonMarshaller.unmarshall(entity, RestVdbDataRole.class);
            assertNotNull(outDataRole);
            assertThat(outDataRole.isAllowCreateTempTables(), is(allowCreateTempTables));
            assertThat(outDataRole.isAnyAuthenticated(), is(anyAuthenticated));
            assertThat(outDataRole.getDataPath(), is(dataPath));
            assertThat(outDataRole.getDescription(), is(description));
            assertThat(outDataRole.isGrantAll(), is(grantAll));
            assertThat(outDataRole.getId(), is(id));
            assertThat(outDataRole.getkType(), is(type));
            assertThat(Arrays.equals(outDataRole.getMappedRoles(), mappedRoles), is(true));
            assertThat(outDataRole.getName(), is(dataRoleName));

            assertThat(outDataRole.getPermissions().length, is(1));
            RestVdbPermission outPermission = outDataRole.getPermissions()[0];
            assertThat(outPermission.getName(), is(permissionName));
            assertThat(outPermission.isAllowAlter(), is(allowAlter));
            assertThat(outPermission.isAllowCreate(), is(allowCreate));
            assertThat(outPermission.isAllowDelete(), is(allowDelete));
            assertThat(outPermission.isAllowExecute(), is(allowExecute));
            assertThat(outPermission.isAllowLanguage(), is(allowLanguage));
            assertThat(outPermission.isAllowRead(), is(allowRead));
            assertThat(outPermission.isAllowUpdate(), is(allowUpdate));
        }

        { // verify new data role exists
            HttpGet request = jsonRequest(uri, RequestType.GET);
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);

            RestVdbDataRole dataRole = KomodoJsonMarshaller.unmarshall(entity, RestVdbDataRole.class);
            assertNotNull(dataRole);

            assertThat(dataRole.isAllowCreateTempTables(), is(allowCreateTempTables));
            assertThat(dataRole.isAnyAuthenticated(), is(anyAuthenticated));
            assertThat(dataRole.getDataPath(), is(dataPath));
            assertThat(dataRole.getDescription(), is(description));
            assertThat(dataRole.isGrantAll(), is(grantAll));
            assertThat(dataRole.getId(), is(id));
            assertThat(dataRole.getkType(), is(type));
            assertThat(Arrays.equals(dataRole.getMappedRoles(), mappedRoles), is(true));
            assertThat(dataRole.getName(), is(dataRoleName));
            assertThat(dataRole.getPermissions().length, is(1));

            Collection<RestLink> links = dataRole.getLinks();
            assertEquals(4, links.size());
        }
    }

    @Test
    public void shouldDeleteDataRole() throws Exception {
        loadVdbs();

        String dataRoleName = "MyDataRole";

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, dataRoleName);
        URI uri = _uriBuilder.vdbDataRoleUri(LinkType.SELF, settings);

        { // create data role
            boolean allowCreateTempTables = true;
            boolean anyAuthenticated = true;
            String dataPath = "/tko:komodo/tko:workspace/" + USER_NAME + FORWARD_SLASH + TestUtilities.PORTFOLIO_VDB_NAME + FORWARD_SLASH
                                    + VdbLexicon.Vdb.DATA_ROLES + FORWARD_SLASH + dataRoleName;
            String description = "My data role description";
            boolean grantAll = true;
            String[] mappedRoles = new String[] {"a", "b", "c", "d", "e"};
            KomodoType type = KomodoType.VDB_DATA_ROLE;
            String id = dataRoleName;

            RestVdbDataRole dataRole = new RestVdbDataRole();
            dataRole.setAllowCreateTempTables(allowCreateTempTables);
            dataRole.setAnyAuthenticated(anyAuthenticated);
            dataRole.setDataPath(dataPath);
            dataRole.setDescription(description);
            dataRole.setGrantAll(grantAll);
            dataRole.setId(id);
            dataRole.setkType(type);
            dataRole.setMappedRoles(mappedRoles);
            dataRole.setName(dataRoleName);

            String json = KomodoJsonMarshaller.marshall(dataRole);
            HttpPost request = jsonRequest(uri, RequestType.POST);
            addBody(request, json);

            HttpResponse response = executeOk(request);
            extractResponse(response);
        }

        { // delete data role
            HttpDelete request = jsonRequest(uri, RequestType.DELETE);
            executeOk(request);
        }

        { // verify data role no longer exists
            HttpGet request = jsonRequest(uri, RequestType.GET);
            HttpResponse response = execute(request);
            assertResponse(response, HttpStatus.SC_NOT_FOUND);
        }
    }

    @Test
    public void shouldGetVdbDataRoles() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME, LinkType.DATA_ROLES);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbDataRole[] dataRoles = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbDataRole[].class);
        assertNotNull(dataRoles);
        assertEquals(1, dataRoles.length);

        RestVdbDataRole dataRole = dataRoles[0];

        assertEquals(KomodoType.VDB_DATA_ROLE, dataRole.getkType());
        assertEquals("roleOne", dataRole.getName());
        assertEquals("roleOne described", dataRole.getDescription());
        assertEquals(true, dataRole.isAllowCreateTempTables());
        assertEquals(false, dataRole.isAnyAuthenticated());
        assertEquals(true, dataRole.isGrantAll());
        assertEquals(2, dataRole.getMappedRoles().length);
        assertTrue(dataRole.getMappedRoles()[0].equals("ROLE1") || dataRole.getMappedRoles()[0].equals("ROLE2"));
        assertTrue(dataRole.getMappedRoles()[1].equals("ROLE1") || dataRole.getMappedRoles()[1].equals("ROLE2"));

        Collection<RestLink> links = dataRole.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbDataRolesEmptyList() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.DATA_ROLES);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestBasicEntity[] dataRoles = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertNotNull(dataRoles);
        assertEquals(0, dataRoles.length);
    }

    @Test
    public void shouldGetVdbDataRole() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        URI uri = _uriBuilder.vdbDataRoleUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbDataRole dataRole = KomodoJsonMarshaller.unmarshall(entity, RestVdbDataRole.class);
        assertNotNull(dataRole);

        assertEquals(KomodoType.VDB_DATA_ROLE, dataRole.getkType());
        assertEquals("roleOne", dataRole.getName());
        assertEquals("roleOne described", dataRole.getDescription());
        assertEquals(true, dataRole.isAllowCreateTempTables());
        assertEquals(false, dataRole.isAnyAuthenticated());
        assertEquals(true, dataRole.isGrantAll());
        assertEquals(2, dataRole.getMappedRoles().length);
        assertTrue(dataRole.getMappedRoles()[0].equals("ROLE1") || dataRole.getMappedRoles()[0].equals("ROLE2"));
        assertTrue(dataRole.getMappedRoles()[1].equals("ROLE1") || dataRole.getMappedRoles()[1].equals("ROLE2"));

        Collection<RestLink> links = dataRole.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbPermissions() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        URI uri = _uriBuilder.vdbDataRoleUri(LinkType.PERMISSIONS, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbPermission[] permissions = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbPermission[].class);
        assertNotNull(permissions);
        assertEquals(4, permissions.length);

        for (RestVdbPermission permission : permissions) {
            assertEquals(KomodoType.VDB_PERMISSION, permission.getkType());
            Collection<RestLink> links = permission.getLinks();
            assertEquals(5, links.size());

            if (permission.getId().equals("myTable.T1")) {
                assertFalse(permission.hasChildren());
                assertFalse(permission.isAllowAlter());
                assertFalse(permission.isAllowCreate());
                assertFalse(permission.isAllowDelete());
                assertFalse(permission.isAllowExecute());
                assertFalse(permission.isAllowLanguage());
                assertTrue(permission.isAllowRead());
                assertFalse(permission.isAllowUpdate());

            } else if (permission.getId().equals("myTable.T2")) {
                assertTrue(permission.hasChildren());
                assertTrue(permission.isAllowAlter());
                assertTrue(permission.isAllowCreate());
                assertTrue(permission.isAllowDelete());
                assertTrue(permission.isAllowExecute());
                assertFalse(permission.isAllowLanguage());
                assertFalse(permission.isAllowRead());
                assertTrue(permission.isAllowUpdate());

            } else if (permission.getId().equals("myTable.T2.col1")) {
                assertTrue(permission.hasChildren());
                assertFalse(permission.isAllowAlter());
                assertFalse(permission.isAllowCreate());
                assertFalse(permission.isAllowDelete());
                assertFalse(permission.isAllowExecute());
                assertFalse(permission.isAllowLanguage());
                assertFalse(permission.isAllowRead());
                assertFalse(permission.isAllowUpdate());

            } else if (permission.getId().equals("javascript")) {
                assertFalse(permission.hasChildren());
                assertFalse(permission.isAllowAlter());
                assertFalse(permission.isAllowCreate());
                assertFalse(permission.isAllowDelete());
                assertFalse(permission.isAllowExecute());
                assertTrue(permission.isAllowLanguage());
                assertFalse(permission.isAllowRead());
                assertFalse(permission.isAllowUpdate());
            }
        }
    }

    @Test
    public void shouldGetVdbPermission() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T1");
        URI uri = _uriBuilder.vdbPermissionUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbPermission permission = KomodoJsonMarshaller.unmarshall(entity, RestVdbPermission.class);
        assertNotNull(permission);

        assertEquals(KomodoType.VDB_PERMISSION, permission.getkType());
        assertEquals("myTable.T1", permission.getId());
        assertFalse(permission.hasChildren());
        assertFalse(permission.isAllowAlter());
        assertFalse(permission.isAllowCreate());
        assertFalse(permission.isAllowDelete());
        assertFalse(permission.isAllowExecute());
        assertFalse(permission.isAllowLanguage());
        assertTrue(permission.isAllowRead());
        assertFalse(permission.isAllowUpdate());

        Collection<RestLink> links = permission.getLinks();
        assertEquals(5, links.size());
    }

    @Test
    public void shouldGetVdbConditions() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2");
        URI uri = _uriBuilder.vdbPermissionUri(LinkType.CONDITIONS, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbCondition[] conditions = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbCondition[].class);
        assertNotNull(conditions);
        assertEquals(1, conditions.length);

        RestVdbCondition condition = conditions[0];
        assertEquals("col1 = user()", condition.getName());
        assertFalse(condition.isConstraint());

        Collection<RestLink> links = condition.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbCondition() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_TYPE, LinkType.CONDITIONS.uriName());
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_ID, "col1 = user()");
        URI uri = _uriBuilder.vdbPermissionChildUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbCondition condition = KomodoJsonMarshaller.unmarshall(entity, RestVdbCondition.class);
        assertNotNull(condition);

        assertEquals("col1 = user()", condition.getName());
        assertFalse(condition.isConstraint());

        Collection<RestLink> links = condition.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbMasks() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2.col1");
        URI uri = _uriBuilder.vdbPermissionUri(LinkType.MASKS, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbMask[] masks = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbMask[].class);
        assertNotNull(masks);
        assertEquals(1, masks.length);

        RestVdbMask mask = masks[0];
        assertEquals("col2", mask.getName());
        assertEquals("1", mask.getOrder());

        Collection<RestLink> links = mask.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbMask() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2.col1");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_TYPE, LinkType.MASKS.uriName());
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_ID, "col2");
        URI uri = _uriBuilder.vdbPermissionChildUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestVdbMask mask = KomodoJsonMarshaller.unmarshall(entity, RestVdbMask.class);
        assertNotNull(mask);

        assertEquals("col2", mask.getName());
        assertEquals("1", mask.getOrder());

        Collection<RestLink> links = mask.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldFailNameValidationWhenNameAlreadyExists() throws Exception {
        _restApp.importVdb(TestUtilities.partsExample(), USER_NAME);

        // try and validate the same name of an existing VDB
        URI vdbUri = _uriBuilder.workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("MyPartsVDB_Dynamic").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        extractResponse(response);
    }

    @Test
    public void shouldFailNameValidationWhenConnectionWithSameNameExists() throws Exception {
        String sourceName = "elvis";
        _restApp.createConnection(sourceName, USER_NAME);

        // try and validate the same name of an existing VDB
        URI vdbUri = _uriBuilder.workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path(sourceName).build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        extractResponse(response);
    }

    @Test
    public void shouldFailNameValidationWhenNameHasInvalidCharacters() throws Exception {
        URI vdbUri = _uriBuilder.workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("InvalidN@me").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        extractResponse(response);
    }

    @Test
    public void shouldFailNameValidationWhenNameIsEmpty() throws Exception {
        URI vdbUri = _uriBuilder.workspaceConnectionsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);
        String errorMsg = extractResponse(response);
        assertThat(errorMsg, startsWith("RESTEASY"));
    }

    @Test
    public void shouldFailNameValidationWhenNameHasSpaces() throws Exception {
        URI vdbUri = _uriBuilder.workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("a b c").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
    }

    @Test
    public void shouldFailNameValidationWhenMissingNameSegment() throws Exception {
        URI vdbUri = _uriBuilder.workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);
        String errorMsg = extractResponse(response);
        assertThat(errorMsg, startsWith("RESTEASY"));
    }

    @Test
    public void shouldValidateName() throws Exception {
        URI vdbUri = _uriBuilder.workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("ValidName").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is("")); // no error message since name was valid
    }
}
