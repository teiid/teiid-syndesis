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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.net.URI;
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
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.relational.ViewBuilderCriteriaPredicate;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.rest.relational.response.RestDataserviceViewInfo;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.service.KomodoVdbService;
import org.komodo.test.utils.TestUtilities;

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
        assertNotNull(myService.getDataPath());
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
    public void shouldGetDataserviceConnections() throws Exception {
        loadStatesDataService();

        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.CONNECTIONS, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        //        System.out.println("Response:\n" + entity);

        RestConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entity, RestConnection[].class);
        assertNotNull(connections);
        assertEquals(1, connections.length);

        RestConnection connection = connections[0];
        assertEquals("USStatesConnection", connection.getId());
        assertEquals("java:/USStatesSource", connection.getJndiName());
        assertEquals("h2", connection.getDriverName());

        Collection<RestLink> links = connection.getLinks();
        assertNotNull(links);
        assertEquals(3, links.size());

        for (RestLink link : links) {
            LinkType rel = link.getRel();
            assertTrue(LinkType.SELF.equals(rel) || LinkType.PARENT.equals(rel) || LinkType.CHILDREN.equals(rel));

            if (LinkType.SELF.equals(rel)) {
                String href = uriBuilder().workspaceConnectionsUri() + FORWARD_SLASH + connection.getId();
                assertEquals(href, link.getHref().toString());
            }
        }
    }

    @Test
    public void shouldGetDataserviceDrivers() throws Exception {
        loadStatesDataService();

        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.DRIVERS, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        //        System.out.println("Response:\n" + entity);

        RestConnectionDriver[] drivers = KomodoJsonMarshaller.unmarshallArray(entity, RestConnectionDriver[].class);
        assertNotNull(drivers);
        assertEquals(1, drivers.length);

        RestConnectionDriver connectionDriver = drivers[0];
        assertEquals("mysql-connector-java-5.1.39-bin.jar", connectionDriver.getName());
    }

    @Test
    public void shouldGetViewInfoForSingleSourceDataService() throws Exception {
        loadDsbSingleSourceDataService();

        // get
        String dsName = TestUtilities.PARTS_SINGLE_SOURCE_SERVICE_NAME;
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.SERVICE_VIEW_INFO, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestDataserviceViewInfo[] viewInfos = KomodoJsonMarshaller.unmarshallArray(entity, RestDataserviceViewInfo[].class);
        assertEquals(2, viewInfos.length);

        for (int i = 0; i < viewInfos.length; i++) {
            String infoType = viewInfos[i].getInfoType();
            if (infoType.equals(RestDataserviceViewInfo.LH_TABLE_INFO)) {
                assertEquals("OracleParts", viewInfos[i].getSourceVdbName());
                assertEquals("SUPPLIER", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("SUPPLIER_ID"));
                assertTrue(colList.contains("SUPPLIER_NAME"));
                assertTrue(colList.contains("SUPPLIER_STATUS"));
                assertTrue(colList.contains("SUPPLIER_CITY"));
                assertTrue(colList.contains("SUPPLIER_STATE"));
            } else if (infoType.equals(RestDataserviceViewInfo.DDL_INFO)) {
                assertEquals(true, viewInfos[i].isViewEditable());
                assertEquals("CREATE VIEW PartsSingleSourceView (\n\tSUPPLIER_ID string,\n\tSUPPLIER_NAME string,\n\tSUPPLIER_STATUS bigdecimal,\n\tSUPPLIER_CITY string,\n\tSUPPLIER_STATE string,\n\tPRIMARY KEY(SUPPLIER_ID)\n)\nAS\nSELECT SUPPLIER_ID, SUPPLIER_NAME, SUPPLIER_STATUS, SUPPLIER_CITY, SUPPLIER_STATE FROM OracleParts.SUPPLIER;\n",
                             viewInfos[i].getViewDdl());
            }
        }
    }

    @Test
    public void shouldGetViewInfoForJoinSameTableNamesDataService() throws Exception {
        loadDsbJoinSameTableNamesDataService();

        // get
        String dsName = TestUtilities.JOIN_SAME_TABLE_NAMES_SERVICE_NAME;
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.SERVICE_VIEW_INFO, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestDataserviceViewInfo[] viewInfos = KomodoJsonMarshaller.unmarshallArray(entity, RestDataserviceViewInfo[].class);
        assertEquals(4, viewInfos.length);

        for (int i = 0; i < viewInfos.length; i++) {
            String infoType = viewInfos[i].getInfoType();
            if (infoType.equals(RestDataserviceViewInfo.LH_TABLE_INFO)) {
                assertEquals("BQTOracle", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLA", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("INTKEY"));
                assertTrue(colList.contains("STRINGKEY"));
            } else if (infoType.equals(RestDataserviceViewInfo.RH_TABLE_INFO)) {
                assertEquals("BQTOracle2", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLA", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("STRINGKEY"));
                assertTrue(colList.contains("INTNUM"));
            } else if (infoType.equals(RestDataserviceViewInfo.CRITERIA_INFO)) {
                assertEquals("INNER", viewInfos[i].getJoinType());
                List<ViewBuilderCriteriaPredicate> colList = viewInfos[i].getCriteriaPredicates();
                assertEquals(1, colList.size());
                ViewBuilderCriteriaPredicate predicate = colList.get(0);
                assertEquals("INTKEY", predicate.getLhColumn());
                assertEquals("INTKEY", predicate.getRhColumn());
                assertEquals("=", predicate.getOperator());
            } else if (infoType.equals(RestDataserviceViewInfo.DDL_INFO)) {
                assertEquals(true, viewInfos[i].isViewEditable());
                assertEquals("CREATE VIEW JoinServiceSameTableNamesView (\n\tRowId integer,\n\tINTKEY bigdecimal,\n\tA_STRINGKEY string,\n\tB_STRINGKEY string,\n\tINTNUM bigdecimal,\n\tPRIMARY KEY(RowId)\n)\nAS\nSELECT ROW_NUMBER() OVER (ORDER BY A.INTKEY), A.INTKEY, A.STRINGKEY, B.STRINGKEY, B.INTNUM FROM BQTOracle.SMALLA AS A INNER JOIN BQTOracle2.SMALLA AS B ON A.INTKEY = B.INTKEY;\n",
                             viewInfos[i].getViewDdl());
            }
        }
    }

    @Test
    public void shouldGetViewInfoForJoinDifferentTableNamesDataService() throws Exception {
        loadDsbJoinDifferentTableNamesDataService();

        // get
        String dsName = TestUtilities.JOIN_DIFFERENT_TABLE_NAMES_SERVICE_NAME;
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.SERVICE_VIEW_INFO, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestDataserviceViewInfo[] viewInfos = KomodoJsonMarshaller.unmarshallArray(entity, RestDataserviceViewInfo[].class);
        assertEquals(4, viewInfos.length);

        for (int i = 0; i < viewInfos.length; i++) {
            String infoType = viewInfos[i].getInfoType();
            if (infoType.equals(RestDataserviceViewInfo.LH_TABLE_INFO)) {
                assertEquals("BQTOracle", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLA", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("INTKEY"));
                assertTrue(colList.contains("STRINGKEY"));
            } else if (infoType.equals(RestDataserviceViewInfo.RH_TABLE_INFO)) {
                assertEquals("BQTOracle2", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLB", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("STRINGKEY"));
                assertTrue(colList.contains("INTNUM"));
            } else if (infoType.equals(RestDataserviceViewInfo.CRITERIA_INFO)) {
                assertEquals("INNER", viewInfos[i].getJoinType());
                List<ViewBuilderCriteriaPredicate> colList = viewInfos[i].getCriteriaPredicates();
                assertEquals(1, colList.size());
                ViewBuilderCriteriaPredicate predicate = colList.get(0);
                assertEquals("INTKEY", predicate.getLhColumn());
                assertEquals("INTKEY", predicate.getRhColumn());
                assertEquals("=", predicate.getOperator());
            } else if (infoType.equals(RestDataserviceViewInfo.DDL_INFO)) {
                assertEquals(true, viewInfos[i].isViewEditable());
                assertEquals("CREATE VIEW JoinServiceDifferentTableNamesView (\n\tRowId integer,\n\tINTKEY bigdecimal,\n\tA_STRINGKEY string,\n\tB_STRINGKEY string,\n\tINTNUM bigdecimal,\n\tPRIMARY KEY(RowId)\n)\nAS\nSELECT ROW_NUMBER() OVER (ORDER BY A.INTKEY), A.INTKEY, A.STRINGKEY, B.STRINGKEY, B.INTNUM FROM BQTOracle.SMALLA AS A INNER JOIN BQTOracle2.SMALLB AS B ON A.INTKEY = B.INTKEY;\n",
                             viewInfos[i].getViewDdl());
            }
        }
    }

    @Test
    public void shouldGetWorkspaceSourceVdbForDataService() throws Exception {
        loadStatesServiceSourceVdb();
        loadStatesDataService();

        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.SOURCE_VDB_MATCHES, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertNotNull(vdbs);
        assertEquals(1, vdbs.length);

        RestVdb vdb = vdbs[0];
        assertEquals("ServiceSource", vdb.getId());

    }

    @Test
    public void shouldNotGetWorkspaceSourceVdbForDataService() throws Exception {
        loadStatesDataService();

        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = uriBuilder().createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        uriBuilder().addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, uriBuilder().workspaceDataservicesUri());

        URI uri = uriBuilder().dataserviceUri(LinkType.SOURCE_VDB_MATCHES, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertNotNull(vdbs);
        for (RestVdb v : vdbs) {
            assertNotEquals(TestUtilities.US_STATES_VDB_NAME, v.getId());
        }
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
    public void shouldCloneDataservice() throws Exception {
        String dataserviceName = "shouldCloneDataservice";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CLONE_SEGMENT).path(dataserviceName).build();

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        String clonedDataservice = "clonedDataservice";
        addBody(request, clonedDataservice);

        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestDataservice dataservice = KomodoJsonMarshaller.unmarshall(entity, RestDataservice.class);
        assertNotNull(dataservice);

        logObjectPath(dataservice.getDataPath());
        logObjectPath(serviceTestUtilities.getWorkspace(USER_NAME) + FORWARD_SLASH + clonedDataservice + "VDB");
        assertEquals(dataservice.getId(), clonedDataservice);
    }

    @Test
    public void shouldNotCloneDataservice() throws Exception {
        String dataserviceName = "shouldNotCloneDataservice";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CLONE_SEGMENT).path(dataserviceName).build();

        // Attempt to clone using the same service name should fail...
        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, dataserviceName);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("cannot be the same"));
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
