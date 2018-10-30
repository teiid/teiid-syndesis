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
package org.komodo.rest.service.unit;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.komodo.spi.storage.git.GitStorageConnectorConstants.AUTHOR_EMAIL_PROPERTY;
import static org.komodo.spi.storage.git.GitStorageConnectorConstants.AUTHOR_NAME_PROPERTY;
import static org.komodo.spi.storage.git.GitStorageConnectorConstants.REPO_PASSWORD;
import static org.komodo.spi.storage.git.GitStorageConnectorConstants.REPO_PATH_PROPERTY;
import static org.komodo.spi.storage.git.GitStorageConnectorConstants.REPO_USERNAME;

import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.core.UriBuilder;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.junit.Test;
import org.komodo.importer.ImportMessages;
import org.komodo.relational.profile.GitRepository;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.KomodoService;
import org.komodo.rest.cors.CorsHeaders;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.rest.relational.response.RestGitRepository;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate.RestStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;
import org.komodo.rest.service.KomodoUtilService;
import org.komodo.rest.service.KomodoVdbService;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.TestUtilities;

import com.google.common.net.HttpHeaders;

@SuppressWarnings( {"javadoc", "nls"} )
@net.jcip.annotations.NotThreadSafe
public class KomodoUtilServiceTestInSuite extends AbstractKomodoServiceTest {

    private static final String AUTH_HEADER_VALUE = "Basic YWRtaW48";

    private String viewName = "myNewView";
    private String view2Name = "myNewViewS";
    private String undoRedoId = "UpdateViewNameCommand";
    private String untitledName = "untitled";
    private String oldNameKey = "oldName";
    private String newNameKey = "newName";

    public KomodoUtilServiceTestInSuite() throws Exception {
        super();
    }

    private void loadSamples(String user) throws Exception {
        for (String sample : KomodoUtilService.SAMPLES) {
            ImportMessages msgs = restApp().importVdb(KomodoUtilService.getVdbSample(sample), user);
            assertTrue(msgs.getErrorMessages().isEmpty());
        }
    }

    @Test
    public void shouldAbout() throws Exception {
        String[] EXPECTED = {
            OPEN_BRACE + NEW_LINE,
            "  \"Information\": " +  OPEN_BRACE + NEW_LINE,
            "    \"Repository Workspace\": \"komodoLocalWorkspace\"," + NEW_LINE,
            "    \"Repository Configuration\"", // Configuration Url contains local file names so impossible to test
            "    \"Repository Vdb Total\": \"",
            "  " + CLOSE_BRACE + NEW_LINE };

        loadSamples(USER_NAME);

        // get
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.ABOUT).build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
//        System.out.println("Response from uri " + uri + ":\n" + entity);
        for (String expected : EXPECTED) {
            assertTrue(entity.contains(expected));
        }

        // This are generated on build from maven variables so check they are listed
        assertTrue(entity.contains(KomodoUtilService.APP_NAME));
        assertTrue(entity.contains(KomodoUtilService.APP_TITLE));
        assertTrue(entity.contains(KomodoUtilService.APP_VERSION));
        assertTrue(entity.contains(KomodoUtilService.APP_DESCRIPTION));
    }

    @Test
    public void shouldGetUserProfile() throws Exception {
        String[] EXPECTED = {
            OPEN_BRACE + NEW_LINE,
            "  \"Information\": " +  OPEN_BRACE + NEW_LINE,
            "    \"" + KomodoUtilService.USER_NAME + "\": \"" + USER_NAME + "\"," + NEW_LINE,
            "    \"" + KomodoUtilService.WORKSPACE + "\": \"" + serviceTestUtilities.getWorkspace(USER_NAME) + "\"," + NEW_LINE, // Configuration Url contains local file names so impossible to test
            "    \"Repository Vdb Total\": \"",
            "  " + CLOSE_BRACE + NEW_LINE };

        // get
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE).build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
//        System.out.println("Response from uri " + uri + ":\n" + entity);
        for (String expected : EXPECTED) {
            assertTrue(entity.contains(expected));
        }

        // This are generated on build from maven variables so check they are listed
        assertTrue(entity.contains(KomodoUtilService.USER_NAME));
        assertTrue(entity.contains(KomodoUtilService.WORKSPACE));
    }

    @Test
    public void shouldLoadSampleData() throws Exception {

        try {
            serviceTestUtilities.deleteVdbs(USER_NAME);

            // get
            URI uri = UriBuilder.fromUri(uriBuilder().baseUri()).path(V1Constants.SERVICE_SEGMENT).path(V1Constants.SAMPLE_DATA).build();

            HttpPost request = jsonRequest(uri, RequestType.POST);
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);
            // System.out.println("Response from uri " + uri + ":\n" + entity);

            KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
            assertNotNull(status);

            assertEquals("Sample Vdb Import", status.getTitle());
            Map<String, String> attributes = status.getAttributes();

            for (String sample : KomodoUtilService.SAMPLES) {
                String message = attributes.get(sample);
                assertNotNull(message);
                assertTrue(message.startsWith("The sample vdb"));
            }
        } finally {
            //
            // Restore the sample vdbs
            //
            UnitServiceResources.getInstance().loadVdbs();            
        }
    }

    @Test
    public void shouldLoadSamplesDataAlreadyExists() throws Exception {
        loadSamples(USER_NAME);

        // get
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.SAMPLE_DATA).build();

        HttpPost request = jsonRequest(uri, RequestType.POST);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        // System.out.println("Response from uri " + uri + ":\n" + entity);

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        assertEquals("Sample Vdb Import", status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        for (String sample : KomodoUtilService.SAMPLES) {
            String message = attributes.get(sample);
            assertNotNull(message);

            assertEquals(
                         RelationalMessages.getString(
                                                      RelationalMessages.Error.VDB_SAMPLE_IMPORT_VDB_EXISTS,
                                                      sample), message);
        }
    }

    @Test
    public void shouldReturnTeiidSchema() throws Exception {

        // get
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.SCHEMA_SEGMENT)
                                                    .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
//        System.out.println("Response from uri " + uri + ":\n" + entity);

        InputStream schemaStream = getClass().getResourceAsStream("teiid-schema.json");
        String expected = TestUtilities.toString(schemaStream);

        if (! expected.trim().equals(entity)) {
            schemaStream = getClass().getResourceAsStream("teiid-schema2.json");
            expected = TestUtilities.toString(schemaStream);
            assertEquals(expected.trim(), entity);
        }
    }

    @Test
    public void shouldReturnTeiidSchemaForKType() throws Exception {

        // get
        UriBuilder baseBuilder = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.SCHEMA_SEGMENT);

        // Get model
        URI uri = baseBuilder.clone()
                                         .queryParam(KomodoService.QueryParamKeys.KTYPE, KomodoType.MODEL)
                                         .build();

        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
//        System.out.println("Response from uri " + uri + ":\n" + entity);

        assertFalse(entity.contains("\"schema-1\" : {"));
        assertFalse(entity.contains("\"keng__id\" : \"vdb\""));
        assertFalse(entity.contains("\"keng__id\": \"connection\""));
        assertFalse(entity.contains("\"keng__id\" : \"importVdb\""));
        assertTrue(entity.contains("\"keng__id\" : \"model\""));
        assertFalse(entity.contains("\"keng__id\" : \"source\""));
        assertFalse(entity.contains("\"keng__id\" : \"metadata\""));
        assertFalse(entity.contains("\"keng__id\" : \"validationError\""));
        assertFalse(entity.contains("\"keng__id\" : \"translator\""));
        assertFalse(entity.contains("\"keng__id\" : \"dataRole\""));
        assertFalse(entity.contains("\"keng__id\" : \"permission\""));
        assertFalse(entity.contains("\"keng__id\" : \"condition\""));
        assertFalse(entity.contains("\"keng__id\" : \"mask\""));
        assertFalse(entity.contains("\"keng__id\" : \"entry\""));

        // Get data role
        uri = baseBuilder.clone()
                                  .queryParam(KomodoService.QueryParamKeys.KTYPE, KomodoType.VDB_DATA_ROLE)
                                  .build();

        request = jsonRequest(uri, RequestType.GET);
        response = executeOk(request);

        entity = extractResponse(response);

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        assertFalse(entity.contains("\"schema-1\" : {"));
        assertFalse(entity.contains("\"keng__id\" : \"vdb\""));
        assertFalse(entity.contains("\"keng__id\": \"connection\""));
        assertFalse(entity.contains("\"keng__id\" : \"importVdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"model\""));
        assertFalse(entity.contains("\"keng__id\" : \"source\""));
        assertFalse(entity.contains("\"keng__id\" : \"metadata\""));
        assertFalse(entity.contains("\"keng__id\" : \"validationError\""));
        assertFalse(entity.contains("\"keng__id\" : \"translator\""));
        assertTrue(entity.contains("\"keng__id\" : \"dataRole\""));
        assertFalse(entity.contains("\"keng__id\" : \"permission\""));
        assertFalse(entity.contains("\"keng__id\" : \"condition\""));
        assertFalse(entity.contains("\"keng__id\" : \"mask\""));
        assertFalse(entity.contains("\"keng__id\" : \"entry\""));

        // Get mask
        uri = baseBuilder.clone().
                                   queryParam(KomodoService.QueryParamKeys.KTYPE, KomodoType.VDB_MASK)
                                   .build();

        request = jsonRequest(uri, RequestType.GET);
        response = executeOk(request);

        entity = extractResponse(response);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        assertFalse(entity.contains("\"schema-1\" : {"));
        assertFalse(entity.contains("\"keng__id\" : \"vdb\""));
        assertFalse(entity.contains("\"keng__id\": \"connection\""));
        assertFalse(entity.contains("\"keng__id\" : \"importVdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"model\""));
        assertFalse(entity.contains("\"keng__id\" : \"source\""));
        assertFalse(entity.contains("\"keng__id\" : \"metadata\""));
        assertFalse(entity.contains("\"keng__id\" : \"validationError\""));
        assertFalse(entity.contains("\"keng__id\" : \"translator\""));
        assertFalse(entity.contains("\"keng__id\" : \"dataRole\""));
        assertFalse(entity.contains("\"keng__id\" : \"permission\""));
        assertFalse(entity.contains("\"keng__id\" : \"condition\""));
        assertTrue(entity.contains("\"keng__id\" : \"mask\""));
        assertFalse(entity.contains("\"keng__id\" : \"entry\""));

        // Get data source
        uri = baseBuilder.clone().
                                   queryParam(KomodoService.QueryParamKeys.KTYPE, KomodoType.CONNECTION)
                                   .build();

        request = jsonRequest(uri, RequestType.GET);
        addJsonConsumeContentType(request);
        response = executeOk(request);

        entity = extractResponse(response);
//        System.out.println("Response from uri " + uri + ":\n" + entity);

        assertFalse(entity.contains("\"schema-1\" : {"));
        assertFalse(entity.contains("\"keng__id\" : \"vdb\""));
        assertTrue(entity.contains("\"keng__id\": \"connection\""));
        assertFalse(entity.contains("\"keng__id\" : \"importVdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"model\""));
        assertFalse(entity.contains("\"keng__id\" : \"source\""));
        assertFalse(entity.contains("\"keng__id\" : \"metadata\""));
        assertFalse(entity.contains("\"keng__id\" : \"validationError\""));
        assertFalse(entity.contains("\"keng__id\" : \"translator\""));
        assertFalse(entity.contains("\"keng__id\" : \"dataRole\""));
        assertFalse(entity.contains("\"keng__id\" : \"permission\""));
        assertFalse(entity.contains("\"keng__id\" : \"condition\""));
        assertFalse(entity.contains("\"keng__id\" : \"mask\""));
        assertFalse(entity.contains("\"keng__id\" : \"entry\""));
    }

    @Test
    public void shouldAddUserProfileGitRepository() throws Exception {
        String gitName = "myGitRepo";
        String gitUrl = "https://github.com/teiid/mygit";
        String gitBranch = "testWork1";
        String gitUser = "user";
        String gitPassword = "user";
        String gitCommitAuthor = "User";
        String gitCommitEmail = "user@user.com";

        String[] EXPECTED = {
            OPEN_BRACE + NEW_LINE,
            "\"name\": " + SPEECH_MARK + gitName + SPEECH_MARK + COMMA + NEW_LINE,
            "\"url\": " + SPEECH_MARK + gitUrl + SPEECH_MARK + COMMA + NEW_LINE,
            "\"branch\": " + SPEECH_MARK + gitBranch + SPEECH_MARK + COMMA + NEW_LINE,
            "\"user\": " + SPEECH_MARK + gitUser + SPEECH_MARK + COMMA + NEW_LINE,
            "\"password\": " + SPEECH_MARK + KomodoService.ENCRYPTED_PREFIX,
            "\"commitAuthor\": " + SPEECH_MARK + gitCommitAuthor + SPEECH_MARK + COMMA + NEW_LINE,
            "\"commitEmail\": " + SPEECH_MARK + gitCommitEmail + SPEECH_MARK,
            CLOSE_BRACE};

        // put
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.GIT_REPOSITORY).build();

        RestGitRepository gitRepository = new RestGitRepository();
        gitRepository.setName(gitName);
        gitRepository.setUrl(new URL(gitUrl));
        gitRepository.setBranch(gitBranch);
        gitRepository.setUser(gitUser);
        gitRepository.setPassword(gitPassword);
        gitRepository.setCommitAuthor(gitCommitAuthor);
        gitRepository.setCommitEmail(gitCommitEmail);

        HttpPut request = jsonRequest(uri, RequestType.PUT);
        addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
        addHeader(request, HttpHeaders.AUTHORIZATION, AUTH_HEADER_VALUE);
        addBody(request, gitRepository);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
//        System.out.println("Response from uri " + uri + ":\n" + entity);
        for (String expected : EXPECTED) {
            assertTrue(entity.contains(expected));
        }
    }

    @Test
    public void shouldRemoveUserProfileGitRepository() throws Exception {
        String gitName = "myGitRepo";
        String gitUrl = "https://github.com/teiid/mygit";
        String gitUser = "user";
        String gitPassword = "user";
        String gitCommitAuthor = "User";
        String gitCommitEmail = "user@user.com";

        //
        // Populate the user profile ready for the test
        //
        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setParameter(REPO_PATH_PROPERTY, gitUrl);
        storageAttr.setParameter(REPO_USERNAME, gitUser);
        storageAttr.setParameter(REPO_PASSWORD, gitPassword);
        storageAttr.setParameter(AUTHOR_NAME_PROPERTY, gitCommitAuthor);
        storageAttr.setParameter(AUTHOR_EMAIL_PROPERTY, gitCommitEmail);
        GitRepository config = serviceTestUtilities.addGitRepositoryConfig(USER_NAME, gitName, storageAttr);
        assertNotNull(config);

        // delete
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.GIT_REPOSITORY)
                                                    .path(gitName)
                                                    .build();

        HttpDelete request = jsonRequest(uri, RequestType.DELETE);
        addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
//        System.out.println("Response from uri " + uri + ":\n" + entity);

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        assertEquals("Delete Status", status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        for (Map.Entry<String, String> entry : attributes.entrySet()) {
            String message = entry.getValue();
            assertNotNull(message);

            assertEquals("Successfully deleted", message);
        }
    }

    @Test
    public void shouldAddUserProfileViewEditorState() throws Exception {
        // put
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.VIEW_EDITOR_STATES).build();

        RestStateCommand undo = new RestStateCommand();
        undo.setId(undoRedoId);
        undo.addArgument(oldNameKey, viewName);
        undo.addArgument(newNameKey, untitledName);

        RestStateCommand redo = new RestStateCommand();
        redo.setId(undoRedoId);
        redo.addArgument(oldNameKey, untitledName);
        redo.addArgument(newNameKey, viewName);

        RestStateCommandAggregate command = new RestStateCommandAggregate();
        command.setUndo(undo);
        command.setRedo(redo);

        RestStateCommandAggregate[] content = { command };

        RestViewEditorState[] restViewEditorStates = new RestViewEditorState[1];

        RestViewEditorState restViewEditorState = new RestViewEditorState();
        restViewEditorState.setBaseUri(uriBuilder().baseUri());
        restViewEditorState.setId(viewName);
        restViewEditorState.setCommands(content);

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
        
        RestSqlComposition restComposition = new RestSqlComposition(compName,compDescr,compLeftSource,compRightSource,
        		                                                    leftColumn,rightColumn,type,operator);

        RestSqlComposition[] compositionArray = new RestSqlComposition[1];
        compositionArray[0] = restComposition;
        
        RestViewDefinition restViewDefn = new RestViewDefinition();
        restViewDefn.setViewName(viewName);
        restViewDefn.setDescription(viewDescr);
        restViewDefn.setSourcePaths(sourcePaths);
        restViewDefn.setSqlCompositions(compositionArray);
        restViewDefn.setComplete(true);
        
        restViewEditorState.setViewDefinition(restViewDefn);
        
        restViewEditorStates[0] = restViewEditorState;
        
        try {
            HttpPut request = jsonRequest(uri, RequestType.PUT);
            addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
            addHeader(request, HttpHeaders.AUTHORIZATION, AUTH_HEADER_VALUE);
            addBody(request, restViewEditorStates);

            HttpResponse response = executeOk(request);
            String entity = extractResponse(response);
            
            KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
            assertNotNull(status);

            assertEquals("Stash Status", status.getTitle());
            Map<String, String> attributes = status.getAttributes();

            for (Map.Entry<String, String> entry : attributes.entrySet()) {
                String message = entry.getValue();
                assertNotNull(message);

                assertEquals("Successfully stashed", message);
            }
        } finally {
            serviceTestUtilities.removeViewEditorState(USER_NAME, viewName);
        }
    }

    @Test
    public void shouldAddUserProfileViewEditorStates() throws Exception {
        // put
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.VIEW_EDITOR_STATES).build();

        RestStateCommand undo = new RestStateCommand();
        undo.setId(undoRedoId);
        undo.addArgument(oldNameKey, viewName);
        undo.addArgument(newNameKey, untitledName);

        RestStateCommand redo = new RestStateCommand();
        redo.setId(undoRedoId);
        redo.addArgument(oldNameKey, untitledName);
        redo.addArgument(newNameKey, viewName);

        RestStateCommandAggregate command = new RestStateCommandAggregate();
        command.setUndo(undo);
        command.setRedo(redo);

        RestStateCommandAggregate[] content = { command };

        // Create 2 RestViewEditorStates, for testing bulk create
        RestViewEditorState[] restViewEditorStates = new RestViewEditorState[2];
        for(int i=0; i<2; i++) {
        	String vName = (i == 0) ? viewName : view2Name;
            RestViewEditorState restViewEditorState = new RestViewEditorState();
            restViewEditorState.setBaseUri(uriBuilder().baseUri());
            restViewEditorState.setId(vName);
            restViewEditorState.setCommands(content);

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
            
            RestSqlComposition restComposition = new RestSqlComposition(compName,compDescr,compLeftSource,compRightSource,
            		                                                    leftColumn,rightColumn,type,operator);

            RestSqlComposition[] compositionArray = new RestSqlComposition[1];
            compositionArray[0] = restComposition;
            
            RestViewDefinition restViewDefn = new RestViewDefinition();
            restViewDefn.setViewName(vName);
            restViewDefn.setDescription(viewDescr);
            restViewDefn.setSourcePaths(sourcePaths);
            restViewDefn.setSqlCompositions(compositionArray);
            restViewDefn.setComplete(true);
            
            restViewEditorState.setViewDefinition(restViewDefn);
            
            restViewEditorStates[i] = restViewEditorState;
        }
        
        try {
            HttpPut request = jsonRequest(uri, RequestType.PUT);
            addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
            addHeader(request, HttpHeaders.AUTHORIZATION, AUTH_HEADER_VALUE);
            addBody(request, restViewEditorStates);
            
            HttpResponse response = executeOk(request);
            String entity = extractResponse(response);
            
            KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
            assertNotNull(status);

            assertEquals("Stash Status", status.getTitle());
            Map<String, String> attributes = status.getAttributes();

            for (Map.Entry<String, String> entry : attributes.entrySet()) {
                String message = entry.getValue();
                assertNotNull(message);

                assertEquals("Successfully stashed", message);
            }
        } finally {
            serviceTestUtilities.removeViewEditorState(USER_NAME, viewName);
            serviceTestUtilities.removeViewEditorState(USER_NAME, view2Name);
        }
    }

    /*
    @Test
    public void shouldEditViewDefinitionViewEditorState() throws Exception {
    	final URI baseUri = new URI("baseUri");
        final String viewDefName = "testView";
        final String desc = "test view description text";
        final boolean isComplete = true;
        final String sourcePath1 = "path/to/source1";
        final String sourcePath2 = "path/to/source2";
        final String[] sourcePaths = {sourcePath1, sourcePath2};
        final String compName = "sqlComp";
        final String compDesc = "description for comp1";
        final String leftSrc = "path/to/source1";
        final String rightSrc = "path/to/source2";
        final String leftCol = "column1";
        final String rightCol = "column2";
        final String type = "INNER_JOIN";
        final String operator = "EQ";

        // put
        URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.VIEW_EDITOR_STATE).build();

        RestStateCommand undo = new RestStateCommand();
        undo.setId(undoRedoId);
        undo.addArgument(oldNameKey, viewName);
        undo.addArgument(newNameKey, untitledName);

        RestStateCommand redo = new RestStateCommand();
        redo.setId(undoRedoId);
        redo.addArgument(oldNameKey, untitledName);
        redo.addArgument(newNameKey, viewName);

        RestStateCommandAggregate command = new RestStateCommandAggregate();
        command.setUndo(undo);
        command.setRedo(redo);

        RestStateCommandAggregate[] content = { command };

        RestViewEditorState restViewEditorState = new RestViewEditorState();
        restViewEditorState.setBaseUri(uriBuilder().baseUri());
        restViewEditorState.setId(viewName);
        restViewEditorState.setCommands(content);
        
        RestViewDefinition def = new RestViewDefinition();
        def.setBaseUri(baseUri);
        def.setViewName(viewDefName);
        def.setDescription(desc);
        def.setComplete(isComplete);
        def.setSourcePaths(sourcePaths);
        RestSqlComposition sqlComp = 
        		new RestSqlComposition(compName, compDesc, leftSrc, rightSrc, leftCol, rightCol, type, operator);
        RestSqlComposition[] comps = {sqlComp};
        
        def.setSqlCompositions(comps);
        
        restViewEditorState.setViewDefinition(def);

        try {
            HttpPut request = jsonRequest(uri, RequestType.PUT);
            addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
            addHeader(request, HttpHeaders.AUTHORIZATION, AUTH_HEADER_VALUE);
            addBody(request, restViewEditorState);
            HttpResponse response = executeOk(request);
            String entity = extractResponse(response);

            RestViewEditorState restState = KomodoJsonMarshaller.unmarshall(entity, RestViewEditorState.class);
            assertEquals(viewName, restState.getId());
            assertEquals(restViewEditorState, restState);
            
            assertEquals(sourcePaths, restState.getViewDefinition().getSourcePaths());
            assertEquals(isComplete, restState.getViewDefinition().isComplete());
        } finally {
            serviceTestUtilities.removeViewEditorState(USER_NAME, viewName);
        }
    }
*/

    @Test
    public void shouldReplaceEditorStateIfExists() throws Exception {
        final String vdbName = "MyVdb";
        final String modelName = "MyModel";
        final String viewName = "MyView";

        final String editorStateId = KomodoVdbService.getViewEditorStateId( vdbName, viewName );
        
        { // Add view definition info
        	
        }

        { // first add an existing state
            final String newDescription = "theNewDescription";
            final String oldDescription = "theOldDescription";
    
            final String undoId = "UpdateViewDescriptionCommand";
            final Map< String, String > undoArgs = new HashMap<>();
            undoArgs.put( "newDescription", newDescription );
            undoArgs.put( "oldDescription", oldDescription );
    
            final String redoId = "UpdateViewDescriptionCommand";
            final Map< String, String > redoArgs = new HashMap<>();
            undoArgs.put( "newDescription", oldDescription );
            undoArgs.put( "oldDescription", newDescription );
    
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
            
            // setup test by creating view and initial view editor state
            this.serviceTestUtilities.createVdbModelView( vdbName, modelName, viewName, USER_NAME );

            this.serviceTestUtilities.addViewEditorState( USER_NAME, editorStateId, 
                    undoId, undoArgs, redoId, redoArgs,
                    viewName, viewDescr, sourcePaths,
                    compName, compDescr, compLeftSource, compRightSource,
                    leftColumn, rightColumn, type, operator);
            
            assertThat( this.serviceTestUtilities.viewEditorStateExists( USER_NAME, editorStateId ), is( true ) );

            // verify initial editor state
            final ViewEditorState editorState = this.serviceTestUtilities.getViewEditorState( USER_NAME, editorStateId );
            final UnitOfWork uow = this.serviceTestUtilities.createReadTransaction( USER_NAME );
            final StateCommandAggregate[] commands = editorState.getCommands( uow );
            assertThat( commands.length, is( 1 ) );

            final StateCommandAggregate undoable = commands[ 0 ];
            assertThat( undoable.getUndo( uow ).getId( uow ), is( undoId ) );
            assertThat( undoable.getRedo( uow ).getId( uow ), is( redoId ) );
            
            ViewDefinition viewDefn = editorState.getViewDefinition(uow);
            assertNotNull( viewDefn );
            assertThat( viewDefn.getName(uow), is( "tko:viewDefinition" ) ); 
            assertThat( viewDefn.getDescription(uow), is( viewDescr ) ); 
            assertThat( viewDefn.getSourcePaths(uow).length, is( 2 ) ); 
            uow.commit();
        }

        // add new editor state
        final RestStateCommand undo = new RestStateCommand();
        undo.setId( undoRedoId );
        undo.addArgument( oldNameKey, viewName );
        undo.addArgument( newNameKey, untitledName );

        final RestStateCommand redo = new RestStateCommand();
        redo.setId( undoRedoId );
        redo.addArgument( oldNameKey, untitledName );
        redo.addArgument( newNameKey, viewName );

        final RestStateCommandAggregate command = new RestStateCommandAggregate();
        command.setUndo( undo );
        command.setRedo( redo );

        final RestStateCommandAggregate[] content = { command };

        RestViewEditorState[] restViewEditorStates = new RestViewEditorState[1];
        final RestViewEditorState restViewEditorState = new RestViewEditorState();
        restViewEditorState.setBaseUri( uriBuilder().baseUri() );
        restViewEditorState.setId( editorStateId );
        restViewEditorState.setCommands( content );
        
        RestViewDefinition viewDefn2 = new RestViewDefinition();
        viewDefn2.setId("test");
        viewDefn2.setDescription("descr");
        String[] srcPaths = new String[1];
        srcPaths[0] = "path";
        viewDefn2.setSourcePaths(srcPaths);
        restViewEditorState.setViewDefinition( viewDefn2 );
        restViewEditorStates[0] = restViewEditorState;
        
        final URI uri = UriBuilder.fromUri( uriBuilder().baseUri() )
                                  .path( V1Constants.SERVICE_SEGMENT )
                                  .path( V1Constants.USER_PROFILE )
                                  .path( V1Constants.VIEW_EDITOR_STATES ).build();

        try {
            final HttpPut request = jsonRequest( uri, RequestType.PUT );
            addHeader( request, CorsHeaders.ORIGIN, "http://localhost:2772" );
            addHeader( request, HttpHeaders.AUTHORIZATION, AUTH_HEADER_VALUE );
            addBody( request, restViewEditorStates );

            final HttpResponse response = executeOk( request );
            final String entity = extractResponse(response);
            
            KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
            assertNotNull(status);

            assertEquals("Stash Status", status.getTitle());
            Map<String, String> attributes = status.getAttributes();

            for (Map.Entry<String, String> entry : attributes.entrySet()) {
                String message = entry.getValue();
                assertNotNull(message);

                assertEquals("Successfully stashed", message);
            }

            assertThat( this.serviceTestUtilities.viewEditorStateExists( USER_NAME, editorStateId ), is( true ) );

            // verify new editor state
            final ViewEditorState editorState = this.serviceTestUtilities.getViewEditorState( USER_NAME, editorStateId );
            final UnitOfWork uow = this.serviceTestUtilities.createReadTransaction( USER_NAME );
            final StateCommandAggregate[] commands = editorState.getCommands( uow );
            assertThat( commands.length, is( 1 ) );

            final StateCommandAggregate undoable = commands[ 0 ];
            assertThat( undoable.getUndo( uow ).getId( uow ), is( this.undoRedoId ) );
            assertThat( undoable.getRedo( uow ).getId( uow ), is( this.undoRedoId ) );

            final ViewDefinition newViewDefn = editorState.getViewDefinition(uow);
            assertNotNull( newViewDefn );
            assertThat( newViewDefn.getName(uow), is( "tko:viewDefinition" ) ); 
            assertThat( newViewDefn.getDescription(uow), is( "descr" ) ); 
            assertThat( newViewDefn.getSourcePaths(uow).length, is( 1 ) ); 
            uow.commit();
        } finally {
            serviceTestUtilities.removeViewEditorState(USER_NAME, editorStateId);
        }
    }

    @Test
    public void shouldRemoveUserProfileViewEditorState() throws Exception {
        String undoId = undoRedoId;
        Map<String, String> undoArgs = new HashMap<>();
        undoArgs.put(newNameKey, untitledName);
        undoArgs.put(oldNameKey, viewName);

        String redoId = undoRedoId;
        Map<String, String> redoArgs = new HashMap<>();
        undoArgs.put(newNameKey, viewName);
        undoArgs.put(oldNameKey, untitledName);

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
        
        try {
            ViewEditorState state = serviceTestUtilities.addViewEditorState(USER_NAME, viewName,
                                                                                       undoId, undoArgs, redoId, redoArgs,
                                                                                       viewName, viewDescr, sourcePaths,
                                                                                       compName, compDescr, compLeftSource, compRightSource,
                                                                                       leftColumn, rightColumn, type, operator);
            assertNotNull(state);

            // delete
            URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.VIEW_EDITOR_STATE)
                                                    .path(viewName)
                                                    .build();

            HttpDelete request = jsonRequest(uri, RequestType.DELETE);
            addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);
//          System.out.println("Response from uri " + uri + ":\n" + entity);

            KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
            assertNotNull(status);

            assertEquals("Delete Status", status.getTitle());
            Map<String, String> attributes = status.getAttributes();

            for (Map.Entry<String, String> entry : attributes.entrySet()) {
                String message = entry.getValue();
                assertNotNull(message);

                assertEquals("Successfully deleted", message);
            }

        } finally {
            serviceTestUtilities.removeViewEditorState(USER_NAME, viewName);
        }
    }

    @Test
    public void shouldGetUserProfileViewEditorStates() throws Exception {
        String undoId = undoRedoId;
        Map<String, String> undoArgs = new HashMap<>();
        undoArgs.put(newNameKey, untitledName);
        undoArgs.put(oldNameKey, viewName);

        String redoId = undoRedoId;
        Map<String, String> redoArgs = new HashMap<>();
        undoArgs.put(newNameKey, viewName);
        undoArgs.put(oldNameKey, untitledName);

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
        
        try {
            ViewEditorState state = serviceTestUtilities.addViewEditorState(USER_NAME, viewName,
                                                                                       undoId, undoArgs, redoId, redoArgs,
                                                                                       viewName, viewDescr, sourcePaths,
                                                                                       compName, compDescr, compLeftSource, compRightSource,
                                                                                       leftColumn, rightColumn, type, operator);
            assertNotNull(state);

            // get
            URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.VIEW_EDITOR_STATE)
                                                    .build();

            HttpGet request = jsonRequest(uri, RequestType.GET);
            addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);
//          System.out.println("Response from uri " + uri + ":\n" + entity);

            RestViewEditorState[] restStates = KomodoJsonMarshaller.unmarshallArray(entity, RestViewEditorState[].class);
            assertNotNull(restStates);
            assertTrue(restStates.length == 1);

            RestViewEditorState restState = restStates[0];
            assertEquals(viewName, restState.getId());

            RestStateCommandAggregate[] aggregates = restState.getCommands();
            assertNotNull(aggregates);
            assertTrue(aggregates.length == 1);

            RestStateCommandAggregate agg = aggregates[0];
            RestStateCommand undo = agg.getUndo();
            assertNotNull(undo);
            assertEquals(undoRedoId, undo.getId());
            assertEquals(undoArgs, undo.getArguments());

            RestStateCommand redo = agg.getRedo();
            assertNotNull(redo);
            assertEquals(undoRedoId, redo.getId());
            assertEquals(redoArgs, redo.getArguments());

        } finally {
            serviceTestUtilities.removeViewEditorState(USER_NAME, viewName);
        }
    }

    @Test
    public void shouldGetUserProfileViewEditorState() throws Exception {
        String undoId = undoRedoId;
        Map<String, String> undoArgs = new HashMap<>();
        undoArgs.put(newNameKey, untitledName);
        undoArgs.put(oldNameKey, viewName);

        String redoId = undoRedoId;
        Map<String, String> redoArgs = new HashMap<>();
        undoArgs.put(newNameKey, viewName);
        undoArgs.put(oldNameKey, untitledName);

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
        
        try {
            ViewEditorState state = serviceTestUtilities.addViewEditorState(USER_NAME, viewName,
                                                                                       undoId, undoArgs, redoId, redoArgs,
                                                                                       viewName, viewDescr, sourcePaths,
                                                                                       compName, compDescr, compLeftSource, compRightSource,
                                                                                       leftColumn, rightColumn, type, operator);
            assertNotNull(state);

            // get
            URI uri = UriBuilder.fromUri(uriBuilder().baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.USER_PROFILE)
                                                    .path(V1Constants.VIEW_EDITOR_STATE)
                                                    .path(viewName)
                                                    .build();

            HttpGet request = jsonRequest(uri, RequestType.GET);
            addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");
            HttpResponse response = executeOk(request);

            String entity = extractResponse(response);
//          System.out.println("Response from uri " + uri + ":\n" + entity);

            RestViewEditorState restState = KomodoJsonMarshaller.unmarshall(entity, RestViewEditorState.class);
            assertEquals(viewName, restState.getId());

            RestStateCommandAggregate[] aggregates = restState.getCommands();
            assertNotNull(aggregates);
            assertTrue(aggregates.length == 1);

            RestStateCommandAggregate agg = aggregates[0];
            RestStateCommand undo = agg.getUndo();
            assertNotNull(undo);
            assertEquals(undoRedoId, undo.getId());
            assertEquals(undoArgs, undo.getArguments());

            RestStateCommand redo = agg.getRedo();
            assertNotNull(redo);
            assertEquals(undoRedoId, redo.getId());
            assertEquals(redoArgs, redo.getArguments());
        } finally {
            serviceTestUtilities.removeViewEditorState(USER_NAME, viewName);
        }
    }
}
