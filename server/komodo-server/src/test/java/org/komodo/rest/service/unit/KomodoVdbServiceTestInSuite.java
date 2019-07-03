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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.junit.Before;
import org.junit.Test;
import org.komodo.importer.ImportMessages;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.rest.service.KomodoVdbService;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoVdbServiceTestInSuite extends AbstractKomodoServiceTest {

    @Before
    public void setup() throws Exception{
    }

    @Test
    public void shouldNotGetVdbsXml() throws Exception {

        URI uri = uriBuilder().workspaceVdbsUri();
        HttpGet request = request(uri, RequestType.GET, MediaType.APPLICATION_XML_TYPE);
        HttpResponse response = execute(request);

        //
        // Internal server error since the server does not support
        // '/vdbs' url returning anything in xml
        //
        assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);

        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));
    }

    //    @Test
    //    public void shouldDeleteVdb() throws Exception {
    //        RestVdb restVdb = createVdbs( 1 )[ 0 ];
    //        response = request( uriBuilder().buildVdbUri( LinkType.DELETE, restVdb.getName() ) ).delete();
    //        assertThat( response.getStatus(), is( Status.NO_CONTENT.getStatusCode() ) );
    //    }

    @Test
    public void shouldDeleteViewEditorState() throws Exception {
        final String vdbName = "MyVdb";
        final String modelName = "MyModel";
        final String viewName = "MyView";
        final String editorStateId = KomodoVdbService.getViewEditorStateId( vdbName, viewName );
        final String newName = "theNewName";
        final String oldName = "theOldName";

        final String undoId = "UpdateViewNameCommand";
        final Map< String, String > undoArgs = new HashMap<>();
        undoArgs.put( "newNameKey", newName );
        undoArgs.put( "oldNameKey", oldName );

        final String redoId = "UpdateViewNameCommand";
        final Map< String, String > redoArgs = new HashMap<>();
        undoArgs.put( "newNameKey", oldName );
        undoArgs.put( "oldNameKey", newName );

        // View Defn properties
        final String viewDescr = "viewName description";
        final String[] sourcePaths = new String[2];
        sourcePaths[0] = "connection=conn1/schema=public/table=customer";
        sourcePaths[1] = "connection=conn1/schema=public/table=account";
        final String compName = "left-right";
        final String compDescr = "composition description";
        final String compLeftSource = "connection=conn1/schema=public/table=customer";
        final String compRightSource = "connection=conn1/schema=public/table=account";
        final String leftColumn = "leftCol";
        final String rightColumn = "rightCol";
        final String type = "INNER_JOIN";
        final String operator = "EQ";

        // setup test by creating view and view editor state
        this.serviceTestUtilities.createVdbModelView( vdbName, modelName, viewName, USER_NAME );
        this.serviceTestUtilities.addViewEditorState(USER_NAME, editorStateId,
        		                                                undoId, undoArgs, redoId, redoArgs,
                                                                viewName, viewDescr, sourcePaths,
                                                                compName, compDescr, compLeftSource, compRightSource,
                                                                leftColumn, rightColumn, type, operator);

        assertThat( this.serviceTestUtilities.viewEditorStateExists( USER_NAME, editorStateId ), is( true ) );

        // now test by deleting the view
        final Properties settings = uriBuilder().createSettings( SettingNames.VDB_NAME, vdbName );
        uriBuilder().addSetting( settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri() );
        uriBuilder().addSetting( settings, SettingNames.MODEL_NAME, modelName );
        uriBuilder().addSetting( settings, SettingNames.VIEW_NAME, viewName );

        final URI uri = uriBuilder().vdbModelViewUri( LinkType.SELF, settings );
        final HttpDelete request = jsonRequest( uri, RequestType.DELETE );
        executeOk( request );
        assertThat( this.serviceTestUtilities.viewEditorStateExists( USER_NAME, editorStateId ), is( false ) );
    }

    @Test
    public void shouldGetVdbXml() throws Exception {
        ImportMessages msgs = importVdb(TestUtilities.portfolioExample(), AbstractServiceTest.USER_NAME);
        assertTrue(msgs.getErrorMessages().isEmpty());

        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        URI uri = uriBuilder().vdbUri(LinkType.SELF, settings);

        HttpGet request = request(uri, RequestType.GET, MediaType.APPLICATION_XML_TYPE);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);

        //        System.out.println("Response:\n" + entity);
        assertTrue(entity.contains("<?xml version="));
        assertTrue(entity.contains("encoding="));
        assertTrue(entity.contains("<vdb name=\"Portfolio\" version=\"1\">"));
        assertTrue(entity.contains("</vdb>"));
    }

    //
    //    @Test
    //    public void shouldNotDeleteVdb() throws Exception {
    //        response = request( uriBuilder().buildVdbUri( LinkType.DELETE, "vdbDoesNotExist" ) ).delete();
    //        assertThat( response.getStatus(), is( Status.NOT_FOUND.getStatusCode() ) );
    //    }

    @Test
    public void shouldNotFindVdb() throws Exception {
        URI uri = UriBuilder.fromUri(uriBuilder().workspaceVdbsUri()).path("shouldNotFindVdb").build();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);
        assertResponse(response, HttpStatus.SC_NOT_FOUND);
    }

    @Test
    public void shouldGetVdbDataRolesEmptyList() throws Exception {

        // get
        URI vdbBaseUri = uriBuilder().workspaceVdbsUri();
        URI uri = uriBuilder().vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.DATA_ROLES);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        //System.out.println("Response:\n" + entity);

        RestBasicEntity[] dataRoles = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertNotNull(dataRoles);
        assertEquals(0, dataRoles.length);
    }

    @Test
    public void shouldFailNameValidationWhenNameAlreadyExists() throws Exception {
        importVdb(TestUtilities.partsExample(), USER_NAME);

        // try and validate the same name of an existing VDB
        URI vdbUri = uriBuilder().workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("MyPartsVDB_Dynamic").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        extractResponse(response);
    }

    @Test
    public void shouldFailNameValidationWhenNameHasInvalidCharacters() throws Exception {
        URI vdbUri = uriBuilder().workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("InvalidN@me").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        extractResponse(response);
    }

    @Test
    public void shouldFailNameValidationWhenNameHasSpaces() throws Exception {
        URI vdbUri = uriBuilder().workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("a b c").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
    }

    @Test
    public void shouldFailNameValidationWhenMissingNameSegment() throws Exception {
        URI vdbUri = uriBuilder().workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);
    }

    @Test
    public void shouldValidateName() throws Exception {
        URI vdbUri = uriBuilder().workspaceVdbsUri();
        URI uri = UriBuilder.fromUri(vdbUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("ValidName").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is("")); // no error message since name was valid
    }

    @Test
    public void shouldFailViewNameValidationWhenNameAlreadyExists() throws Exception {
    	createVdbModelView("aVdb","aModel","aView");

        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, "aVdb");
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        uriBuilder().addSetting(settings, SettingNames.MODEL_NAME, "aModel");
        URI modelUri = uriBuilder().vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(V1Constants.VIEWS_SEGMENT).path(V1Constants.NAME_VALIDATION_SEGMENT).path("aView").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        extractResponse(response);
    }

    @Test
    public void shouldFailViewNameValidationWhenNameHasInvalidCharacters() throws Exception {
        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        uriBuilder().addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        URI modelUri = uriBuilder().vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(V1Constants.VIEWS_SEGMENT).path(V1Constants.NAME_VALIDATION_SEGMENT).path("InvalidN@me").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        extractResponse(response);
    }

    @Test
    public void shouldFailViewNameValidationWhenNameIsEmpty() throws Exception {
        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        uriBuilder().addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        URI modelUri = uriBuilder().vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(V1Constants.VIEWS_SEGMENT).path(V1Constants.NAME_VALIDATION_SEGMENT).path("").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);
    }

    @Test
    public void shouldFailViewNameValidationWhenNameHasSpaces() throws Exception {
        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        uriBuilder().addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        URI modelUri = uriBuilder().vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(V1Constants.VIEWS_SEGMENT).path(V1Constants.NAME_VALIDATION_SEGMENT).path("a b c").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
    }

    @Test
    public void shouldFailViewNameValidationWhenVdbNotFound() throws Exception {
        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, "VdbNotFound");
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        uriBuilder().addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        URI modelUri = uriBuilder().vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(V1Constants.VIEWS_SEGMENT).path(V1Constants.NAME_VALIDATION_SEGMENT).path("ValidName").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
    }

    @Test
    public void shouldFailViewNameValidationWhenVdbModelNotFound() throws Exception {
        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        uriBuilder().addSetting(settings, SettingNames.MODEL_NAME, "NotFoundModel");
        URI modelUri = uriBuilder().vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(V1Constants.VIEWS_SEGMENT).path(V1Constants.NAME_VALIDATION_SEGMENT).path("ValidName").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
    }

    @Test
    public void shouldValidateViewName() throws Exception {
        Properties settings = uriBuilder().createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        uriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, uriBuilder().workspaceVdbsUri());
        uriBuilder().addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        URI modelUri = uriBuilder().vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(V1Constants.VIEWS_SEGMENT).path(V1Constants.NAME_VALIDATION_SEGMENT).path("ValidName").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is("")); // no error message since name was valid
    }

}
