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
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.StringStartsWith.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

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
import org.komodo.relational.ViewBuilderCriteriaPredicate;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoDataserviceSingleSourceAttributes;
import org.komodo.rest.relational.request.KomodoDataserviceUpdateAttributes;
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.rest.relational.response.RestDataserviceViewInfo;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.service.KomodoVdbService;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoDataserviceServiceTestInSuite extends AbstractKomodoServiceTest {

    @Rule
    public TestName testName = new TestName();

    public KomodoDataserviceServiceTestInSuite() throws Exception {
        super();
    }

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
    public void shouldNotSetServiceVdbForSingleSourceTablesMissingParameter() throws Exception {
        String dataserviceName = "shouldNotSetServiceVdbForSingleSourceTablesMissingParameter";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_SOURCE_TABLES).build();

        // Required parms (dataserviceName, modelSourcePath, tablePath).
        // fails due to missing modelSourcePath
        KomodoDataserviceSingleSourceAttributes ssrcAttr = new KomodoDataserviceSingleSourceAttributes();
        ssrcAttr.setDataserviceName(dataserviceName);
        // List of Table paths
        List<String> tablePaths = new ArrayList<String>();
        String tablePath = "tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1";
        tablePaths.add(tablePath);
        tablePaths.add(tablePath);
        ssrcAttr.setTablePaths(tablePaths);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, ssrcAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("missing one or more required parameters"));

    }

    @Test
    public void shouldNotSetServiceVdbForSingleSourceTablesBadTablePath() throws Exception {
        String dataserviceName = "shouldNotSetServiceVdbForSingleSourceTablesBadTablePath";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_SOURCE_TABLES).build();

        // Required parms (dataserviceName, modelSourcePath, tablePath).
        // fails due to bad table path - (doesnt resolve to a table)
        KomodoDataserviceSingleSourceAttributes ssrcAttr = new KomodoDataserviceSingleSourceAttributes();
        ssrcAttr.setDataserviceName(dataserviceName);
        // List of Table paths
        List<String> tablePaths = new ArrayList<String>();
        String tablePath = "/path/to/table";
        tablePaths.add(tablePath);
        tablePaths.add(tablePath);
        ssrcAttr.setTablePaths(tablePaths);
        ssrcAttr.setModelSourcePath("/path/to/ModelSource");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, ssrcAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("specified Table does not exist"));

    }

    @Test
    public void shouldNotSetServiceVdbForSingleSourceTablesBadDdl() throws Exception {
        String dataserviceName = "shouldNotSetServiceVdbForSingleSourceTablesBadDdl";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_SOURCE_TABLES).build();

        KomodoDataserviceSingleSourceAttributes ssrcAttr = new KomodoDataserviceSingleSourceAttributes();
        ssrcAttr.setDataserviceName(dataserviceName);
        // List of Table paths
        List<String> tablePaths = new ArrayList<String>();
        String tablePath = "tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1";
        tablePaths.add(tablePath);
        tablePaths.add(tablePath);
        ssrcAttr.setTablePaths(tablePaths);

        ssrcAttr.setModelSourcePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/Accounts/vdb:sources/h2-connector");
        ssrcAttr.setViewDdl("CREATE VIEW blah blah");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, ssrcAttr);

        try {
            HttpResponse response = execute(request);

            assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);
            String entity = extractResponse(response);
            assertTrue(entity.contains("DDL Parsing encountered a problem"));
        } finally {
            //
            // Ensure the vdb is tidied up and removed after use
            //
            Vdb vdb = serviceTestUtilities.getVdb(USER_NAME, dataserviceName + "VDB");
            if (vdb != null)
                logObjectPath(vdb.getAbsolutePath());
        }
    }

    @Test
    public void shouldSetServiceVdbForSingleSourceTables() throws Exception {
        String dataserviceName = "shouldSetServiceVdbForSingleSourceTables";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_SOURCE_TABLES).build();

        KomodoDataserviceSingleSourceAttributes ssrcAttr = new KomodoDataserviceSingleSourceAttributes();
        ssrcAttr.setDataserviceName(dataserviceName);
        // List of Table paths
        List<String> tablePaths = new ArrayList<String>();
        String tablePath = "tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1";
        tablePaths.add(tablePath);
        tablePaths.add(tablePath);
        ssrcAttr.setTablePaths(tablePaths);
        ssrcAttr.setModelSourcePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/Accounts/vdb:sources/h2-connector");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, ssrcAttr);

        try {
            HttpResponse response = execute(request);

            okResponse(response);
            String entity = extractResponse(response);
            assertTrue(entity.contains("Successfully updated"));
        } finally {
            //
            // Ensure the vdb is tidied up and removed after use
            //
            Vdb vdb = serviceTestUtilities.getVdb(USER_NAME, dataserviceName + "VDB");
            if (vdb != null)
                logObjectPath(vdb.getAbsolutePath());
        }
    }
    
    @Test
    public void shouldNotSetServiceVdbForJoinTablesMissingParameter() throws Exception {
        String dataserviceName = "shouldNotSetServiceVdbForJoinTablesMissingParameter";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, modelSourcePath, rhModelSourcePath, tablePath, rhTablePath).
        // fails due to missing rhModelSourcePath
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");
        updateAttr.setModelSourcePath("/path/to/lhModelSource");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("missing one or more required parameters"));

    }

    @Test
    public void shouldNotSetServiceVdbForJoinTablesBadTablePath() throws Exception {
        String dataserviceName = "shouldNotSetServiceVdbForJoinTablesBadTablePath";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, modelSourcePath, rhModelSourcePath, tablePath, rhTablePath).
        // Must also have 'viewDdl' OR 
        // fails due to bad table path
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");
        updateAttr.setModelSourcePath("/path/to/lhModelSource");
        updateAttr.setRhModelSourcePath("/path/to/rhModelSource");
        updateAttr.setViewDdl("CREATE VIEW blah blah");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("specified Table does not exist"));

    }

    @Test
    public void shouldNotSetServiceVdbForJoinTablesBadDdl() throws Exception {
        String dataserviceName = "shouldNotSetServiceVdbForJoinTablesBadDdl";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_JOIN_TABLES).build();

        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setModelSourcePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/Accounts/vdb:sources/h2-connector");
        updateAttr.setRhModelSourcePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/Accounts/vdb:sources/h2-connector");
        updateAttr.setViewDdl("CREATE VIEW blah blah");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        try {
            HttpResponse response = execute(request);

            // Bogus DDL results in an error
            assertResponse(response, HttpStatus.SC_INTERNAL_SERVER_ERROR);
            String entity = extractResponse(response);
            assertTrue(entity.contains("DDL Parsing encountered a problem"));
        } finally {
            //
            // Ensure the vdb is tidied up and removed after use
            //
            Vdb vdb = serviceTestUtilities.getVdb(USER_NAME, dataserviceName + "VDB");
            if (vdb != null)
                logObjectPath(vdb.getAbsolutePath());
        }
    }

    @Test
    public void shouldNotGenerateViewDdlForSingleTableMissingParameter() throws Exception {
        String dataserviceName = "shouldNotGenerateViewDdlForSingleTableMissingParameter";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, tablePath).
        // fails due to missing table path
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        // Bogus DDL results in an error
        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("missing one or more required parameters"));

    }

    @Test
    public void shouldNotGenerateViewDdlForSingleTableBadTablePath() throws Exception {
        String dataserviceName = "shouldNotGenerateViewDdlForSingleTableBadTablePath";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, tablePath).
        // fails due to bad table path (does not resolve to table)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("/path/to/table");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        // Bogus DDL results in an error
        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("specified Table does not exist"));

    }

    @Test
    public void shouldGenerateViewDdlForSingleTable() throws Exception {
        String dataserviceName = "shouldGenerateViewDdlForSingleTable";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, tablePath).
        // Valid entries - should generate DDL
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);

        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);
        assertEquals(RestDataserviceViewInfo.DDL_INFO, viewInfo.getInfoType());
        assertEquals("CREATE VIEW " + dataserviceName + "View"
                     + " ( ROW_ID INTEGER, ACCOUNT_ID INTEGER, PRODUCT_TYPE STRING, PRODUCT_VALUE STRING, "
                     + "CONSTRAINT PK0 PRIMARY KEY (ROW_ID)) AS \nSELECT  ROW_ID, ACCOUNT_ID, PRODUCT_TYPE, PRODUCT_VALUE \nFROM PersonalValuations.Sheet1;",
                     viewInfo.getViewDdl());
        assertEquals(false, viewInfo.isViewEditable());

    }

    @Test
    public void shouldNotGenerateViewDdlForJoinTablesMissingParameter() throws Exception {
        String dataserviceName = "shouldNotGenerateViewDdlForJoinTablesMissingParameter";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, tablePath, rhTablePath, joinType).
        // fails due to missing joinType
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("missing one or more required parameters"));

    }

    @Test
    public void shouldNotGenerateViewDdlForJoinTablesBadTablePath() throws Exception {
        String dataserviceName = "shouldNotGenerateViewDdlForJoinTablesBadTablePath";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, tablePath, rhTablePath, joinType).
        // fails due to bad table path (does not resolve to a Table)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");
        updateAttr.setJoinType("INNER");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("specified Table does not exist"));

    }

    @Test
    public void shouldGenerateViewDdlForJoinTables() throws Exception {
        String dataserviceName = "shouldGenerateViewDdlForJoinTables";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, tablePath, rhTablePath, joinType).
        // Valid entries - should generate DDL
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(dataserviceName);
        updateAttr.setTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setJoinType("INNER");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);

        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);
        assertEquals(RestDataserviceViewInfo.DDL_INFO, viewInfo.getInfoType());
        assertEquals("CREATE VIEW " + dataserviceName
                     + "View (RowId integer PRIMARY KEY,  A_ROW_ID INTEGER, A_ACCOUNT_ID INTEGER, A_PRODUCT_TYPE STRING, A_PRODUCT_VALUE STRING,  "
                     + "B_ROW_ID INTEGER, B_ACCOUNT_ID INTEGER, B_PRODUCT_TYPE STRING, B_PRODUCT_VALUE STRING) AS \n"
                     + "SELECT ROW_NUMBER() OVER (ORDER BY A.ROW_ID), A.ROW_ID, A.ACCOUNT_ID, A.PRODUCT_TYPE, A.PRODUCT_VALUE, B.ROW_ID, B.ACCOUNT_ID, B.PRODUCT_TYPE, B.PRODUCT_VALUE \n"
                     + "FROM \nPersonalValuations.Sheet1 AS A \nINNER JOIN \nPersonalValuations.Sheet1 AS B ;",
                     viewInfo.getViewDdl());
        assertEquals(false, viewInfo.isViewEditable());

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

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, startsWith("RESTEASY"));
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

        String errorMsg = extractResponse(response);
        assertThat(errorMsg, startsWith("RESTEASY"));
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
    public void shouldNotGenerateJoinCriteriaMissingParameter() throws Exception {
        String dataserviceName = "shouldNotGenerateJoinCriteriaMissingParameter";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // fails due to missing joinType
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("/path/to/lhTable");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("missing one or more required parameters"));

    }

    @Test
    public void shouldNotGenerateJoinCriteriaBadTablePath() throws Exception {
        String dataserviceName = "shouldNotGenerateJoinCriteriaBadTablePath";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // fails due to bad table path (does not resolve to a Table)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("specified Table does not exist"));

    }

    @Test
    public void shouldGenerateJoinCriteriaEmpty() throws Exception {
        String dataserviceName = "shouldGenerateJoinCriteriaEmpty";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // Valid entries - should generate join criteria (empty)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/MyPartsVDB_Dynamic/PartsSS/PARTS");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/MyPartsVDB_Dynamic/PartsSS/STATUS");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);

        // Info is returned but the criteria is empty, since there are no table pk-fk relationships
        assertThat(entity, is(notNullValue()));
        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);

        assertTrue("CRITERIA".equals(viewInfo.getInfoType()));
        assertEquals(viewInfo.getCriteriaPredicates().size(), 0);

    }

    @Test
    public void shouldGenerateJoinCriteria() throws Exception {
        String dataserviceName = "shouldGenerateJoinCriteria";
        createDataservice(dataserviceName);
        URI dataservicesUri = uriBuilder().workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // Valid entries - should generate join criteria (empty)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/MyPartsVDB_Dynamic/PartsSS/PARTS");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/" + USER_NAME + "/MyPartsVDB_Dynamic/PartsSS/SUPPLIER_PARTS");

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);

        // Info returned.  Should contain a single criteria predicate (PART_ID = PART_ID).
        assertThat(entity, is(notNullValue()));
        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);

        assertTrue("CRITERIA".equals(viewInfo.getInfoType()));
        assertEquals(1, viewInfo.getCriteriaPredicates().size());
        assertEquals("PART_ID", viewInfo.getCriteriaPredicates().get(0).getLhColumn());
        assertEquals("PART_ID", viewInfo.getCriteriaPredicates().get(0).getRhColumn());
        assertEquals("=", viewInfo.getCriteriaPredicates().get(0).getOperator());

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
