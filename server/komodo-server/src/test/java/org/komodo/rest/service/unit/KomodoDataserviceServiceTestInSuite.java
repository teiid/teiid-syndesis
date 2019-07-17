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
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.service.KomodoVdbService;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoDataserviceServiceTestInSuite extends AbstractKomodoServiceTest {

    @Test
    public void shouldGetDataservices() throws Exception {
        String dataserviceName = "shouldGetDataservices";
        createDataservice(dataserviceName);
        // get
        URI uri = uriBuilder().workspaceDataservicesUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entities = extractResponse(response);
        assertThat(entities, is(notNullValue()));

        // make sure the Dataservice JSON document is returned for each dataservice
        RestDataservice[] dataservices = KomodoJsonMarshaller.unmarshallArray(entities, RestDataservice[].class);

        assertTrue(dataservices.length > 0);
        RestDataservice myService = null;
        for (RestDataservice ds : dataservices) {
            if (dataserviceName.equals(ds.getId())) {
                myService = ds;
                break;
            }
        }
        assertNotNull(myService);
        assertNotNull(myService.getkType());

    }

    @Test
    public void shouldReturnEmptyListWhenNoDataservicesInWorkspace() throws Exception {
        URI uri = uriBuilder().workspaceDataservicesUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);

        assertThat(entity, is(notNullValue()));

        RestDataservice[] dataservices = KomodoJsonMarshaller.unmarshallArray(entity, RestDataservice[].class);
        assertNotNull(dataservices);
        assertEquals(0, dataservices.length);
    }

    @Test
    public void shouldGetDataservice() throws Exception {
        String dataserviceName = "shouldGetDataservice";
        createDataservice(dataserviceName);
        // get
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dataserviceName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestDataservice dataservice = KomodoJsonMarshaller.unmarshall(entity, RestDataservice.class);
        assertNotNull(dataservice);

        assertEquals(dataservice.getId(), dataserviceName);

    }

	    @Test
    public void shouldFailNameValidationWhenNameAlreadyExists() throws Exception {
        String dataserviceName = "shouldFailNameValidationWhenNameAlreadyExists";
        // create a data service first
        createDataservice(dataserviceName);
        // try and validate the same name of an existing data service
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path(dataserviceName).build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));

    }

    @Test
    public void shouldFailNameValidationWhenVdbWithSameNameExists() throws Exception {
        String dataserviceName = "shouldFailNameValidationWhenVdbWithSameNameExists";
        // create a data source first
        createVdb(dataserviceName);

        // try and validate the same name of an existing data service
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path(dataserviceName).build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
        assertThat(errorMsg.isEmpty(), is(false));
    }

    @Test
    public void shouldFailNameValidationWhenNameHasInvalidCharacters() throws Exception {
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("InvalidN@me").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
        assertThat(errorMsg.isEmpty(), is(false));
    }

    @Test
    public void shouldFailNameValidationWhenNameIsEmpty() throws Exception {
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);
        assertThat(response.getStatusLine().getStatusCode(), is(HttpStatus.SC_INTERNAL_SERVER_ERROR));
    }

    @Test
    public void shouldFailNameValidationWhenNameHasSpaces() throws Exception {
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("a b c").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
        assertThat(errorMsg.isEmpty(), is(false));
    }

    @Test
    @Ignore
    // invalid test any client should encode the URL before sending it over.
    public void shouldFailNameValidationWhenNameHasBackslash() throws Exception {
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("\\").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
        assertThat(errorMsg.isEmpty(), is(false));
    }

    @Test
    public void shouldFailNameValidationWhenNameHasSpecialChars() throws Exception {
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("a#").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is(notNullValue()));
        assertThat(errorMsg.isEmpty(), is(false));
    }

    @Test
    public void shouldFailNameValidationWhenMissingNameSegment() throws Exception {
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = execute(request);
        assertThat(response.getStatusLine().getStatusCode(), is(HttpStatus.SC_INTERNAL_SERVER_ERROR));
    }

    @Test
    public void shouldValidateName() throws Exception {
        URI dsUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dsUri).path(V1Constants.NAME_VALIDATION_SEGMENT).path("ValidName").build();
        HttpGet request = request(uri, RequestType.GET, MediaType.TEXT_PLAIN_TYPE);
        HttpResponse response = executeOk(request);

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, is("")); // no error message since name was valid
    }

    @Test
    public void shouldDeleteViewEditorStatesWhenDeletingVirtualization() throws Exception {
        // create virtualization
        final String virtualizationName = "MyVirtualization";
        this.serviceTestUtilities.createDataservice( virtualizationName, false, USER_NAME );

        // verify virtualization exists
        final Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, virtualizationName );
        uriBuilder().addSetting( settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri() );
        final URI uri = uriBuilder().dataserviceUri( LinkType.SELF, settings );
        final HttpGet request = jsonRequest( uri, RequestType.GET );
        final HttpResponse response = execute( request );
        okResponse( response );

        final String entities = extractResponse( response );
        assertThat( entities, is( notNullValue() ) );

        // create editor state
        final String vdbName = "Northwind";
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
        final String modelName = "MyModel";
        this.serviceTestUtilities.createVdbModelView( vdbName, modelName, viewName, USER_NAME );
        this.serviceTestUtilities.addViewEditorState( USER_NAME, editorStateId,
        		                                                 undoId, undoArgs, redoId, redoArgs,
        		                                                 viewName, viewDescr, sourcePaths,
        		                                                 compName, compDescr, compLeftSource, compRightSource,
        		                                                 leftColumn, rightColumn, type, operator);
        assertThat( this.serviceTestUtilities.viewEditorStateExists( USER_NAME, editorStateId ), is( true ) );

        // delete virtualization and make sure editor state is deleted
        final HttpDelete deleteRequest = jsonRequest( uri, RequestType.DELETE );
        executeOk( deleteRequest );
        assertThat( this.serviceTestUtilities.viewEditorStateExists( USER_NAME, editorStateId ), is( false ) );
    }

}
