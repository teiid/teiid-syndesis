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

package org.komodo.rest.service;

import static org.junit.Assert.*;

import java.sql.Connection;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.internal.DefaultMetadataInstance;
import org.komodo.metadata.internal.TeiidDataSourceImpl;
import org.komodo.metadata.query.QSResult;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.Application;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.CredentialsProvider;
import org.komodo.rest.KomodoJsonMarshaller;
import org.komodo.rest.V1Constants;
import org.komodo.rest.connections.SyndesisConnectionMonitor;
import org.komodo.rest.connections.SyndesisConnectionSynchronizer;
import org.komodo.rest.datavirtualization.KomodoQueryAttribute;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.datavirtualization.RestDataVirtualization;
import org.komodo.rest.datavirtualization.RestViewDefinitionStatus;
import org.komodo.rest.service.IntegrationTest.IntegrationTestConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = {IntegrationTestConfiguration.class, Application.class})
@DirtiesContext
@SuppressWarnings("nls")
public class IntegrationTest {

    //inject simple auth bypass
    @TestConfiguration
    static class IntegrationTestConfiguration {
        @Primary
        @Bean
        public CredentialsProvider credentialsProvider() {
            return new CredentialsProvider() {

                @Override
                public OAuthCredentials getCredentials() {
                    return new OAuthCredentials("token", "user");
                }
            };
        }

        /* Stub out the connectivity to syndesis / openshift */
        @MockBean
        private SyndesisConnectionMonitor syndesisConnectionMonitor;
    }

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired
    private SyndesisConnectionSynchronizer syndesisConnectionSynchronizer;
    @Autowired
    private TeiidOpenShiftClient teiidOpenShiftClient;

    @Test
    public void testAbout() {
        ResponseEntity<KomodoStatusObject> response = restTemplate.getForEntity("/v1/service/about", KomodoStatusObject.class);
        assertEquals(V1Constants.App.name(), response.getBody().getAttributes().get(KomodoUtilService.APP_NAME));
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    public void testGetPublished() {
        ResponseEntity<List> response = restTemplate.getForEntity("/v1/metadata/publish", List.class);
        assertNotNull(response.getBody());
    }

    @Test
    public void testError() throws Exception {
    	KomodoQueryAttribute kqa = new KomodoQueryAttribute();
    	ResponseEntity<String> response = restTemplate.postForEntity("/v1/metadata/query", kqa, String.class);
        assertEquals(HttpStatus.FORBIDDEN, response.getStatusCode());
    	assertTrue(response.getBody().endsWith("\"status\":403,\"error\":\"Forbidden\","
    			+ "\"message\":\"No query has been specified\",\"path\":\"/v1/metadata/query\"}"));

    }

    /**
     * Tests a simple view layering with no sources
     * @throws Exception
     */
    @Test
    public void testViewLayers() throws Exception {
        RestDataVirtualization rdv = new RestDataVirtualization();
        String dvName = "dv";
        rdv.setName(dvName);
        rdv.setDescription("description");

        ResponseEntity<String> response = restTemplate.postForEntity(
                "/v1/workspace/dataservices/dv", rdv, String.class);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        ViewDefinition vd = new ViewDefinition(dvName, "myview");
        vd.setComplete(true);
        vd.setDdl("create view myview as select 1 as col");
        vd.setUserDefined(true);

        //using a string response as spring does not seem to handle the
        //unwrap correctly
        ResponseEntity<String> stashStatus = restTemplate.exchange(
                "/v1/service/userProfile/viewEditorState", HttpMethod.PUT,
                new HttpEntity<ViewDefinition>(vd), String.class);

        assertEquals(HttpStatus.OK, stashStatus.getStatusCode());
        RestViewDefinitionStatus saved = KomodoJsonMarshaller.unmarshall(stashStatus.getBody(), RestViewDefinitionStatus.class);
        String id = saved.getViewDefinition().getId();

        assertEquals("SUCCESS", saved.getStatus());
        assertNotNull(saved.getViewDefinition().getCreatedAt());

        ViewDefinition vd2 = new ViewDefinition(dvName, "myview2");
        vd2.setComplete(true);
        vd2.setDdl("create view myview as select * from myview");
        vd2.setUserDefined(true);

        ResponseEntity<String> stashStatus2 = restTemplate.exchange(
                "/v1/service/userProfile/viewEditorState", HttpMethod.PUT,
                new HttpEntity<ViewDefinition>(vd2), String.class);

        assertEquals(HttpStatus.OK, stashStatus2.getStatusCode());
        String id2 = KomodoJsonMarshaller.unmarshall(stashStatus2.getBody(), RestViewDefinitionStatus.class)
                .getViewDefinition().getId();
        assertNotNull(id2);

        ResponseEntity<ViewDefinition> view = restTemplate.getForEntity(
                "/v1/service/userProfile/viewEditorState/{id}",
                ViewDefinition.class, id);
        assertEquals(HttpStatus.OK, view.getStatusCode());

        assertNotNull(view.getBody().getModifiedAt());
        assertNotNull(view.getBody().getCreatedAt());
        assertNotNull(view.getBody().getVersion());

        ResponseEntity<String> validate = restTemplate.getForEntity(
                "/v1/workspace/{virtualization}/views/nameValidation/{viewName}",
                String.class, dvName, "myView");
        assertEquals(HttpStatus.OK, validate.getStatusCode());
        assertNotNull(validate.getBody());

        query("select * from dv.myview", dvName, true);

        //myview2 is not yet valid
        query("select * from dv.myview2", dvName, false);

        //correct it
        vd2.setDdl("create view myview2 as select * from myview");
        restTemplate.exchange(
                "/v1/service/userProfile/viewEditorState", HttpMethod.PUT,
                new HttpEntity<ViewDefinition>(vd2), KomodoStatusObject.class);

        query("select * from dv.myview2", dvName, true);
    }

    private void query(String queryString, String dvName, boolean ok) {
        KomodoQueryAttribute queryAttribute = new KomodoQueryAttribute();
        queryAttribute.setQuery(queryString);
        queryAttribute.setTarget(dvName);
        ResponseEntity<QSResult> query = restTemplate.postForEntity(
                "/v1/metadata/query", queryAttribute, QSResult.class);
        if (ok) {
            assertEquals(HttpStatus.OK, query.getStatusCode());
            QSResult result = query.getBody();
            assertEquals(1, result.getColumns().size());
            assertEquals(1, result.getRows().size());
        } else {
            //resolving error is a bad request
            assertEquals(HttpStatus.BAD_REQUEST, query.getStatusCode());
            //TODO: need to make sure of the other codes
            //can't connect = service unavailable
            //unexpected exception = 500
            //need to ensure that a redeploy while querying is accounted for
        }
    }

    @Autowired
    DefaultMetadataInstance metadata;

    /**
     * Tests an update to source metadata
     * @throws Exception
     */
    @Test
    public void testSourceRefresh() throws Exception {
        RestDataVirtualization rdv = new RestDataVirtualization();
        String dvName = "testSourceRefresh";
        rdv.setName(dvName);
        rdv.setDescription("description");

        ResponseEntity<String> response = restTemplate.postForEntity(
                "/v1/workspace/dataservices/testSourceRefresh", rdv, String.class);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        //the syndesis and os logic has been stubbed, but we can interact
        //directly with the synchronizer

        DefaultSyndesisDataSource dsd = new DefaultSyndesisDataSource();

        //invalid, but should silently fail
        syndesisConnectionSynchronizer.addConnection(dsd, false);

        dsd.setId("1");
        dsd.setSyndesisName("super integration source");
        dsd.setTranslatorName("h2");
        Map<String, String> properties = new HashMap<String, String>();
        properties.put("url", "jdbc:h2:mem:"+dvName);
        properties.put("schema", "DV");
        dsd.setProperties(properties);
        dsd.setDefinition(teiidOpenShiftClient.getSourceDefinitionThatMatches(properties, "sql"));

        syndesisConnectionSynchronizer.addConnection(dsd, false);

        Thread.sleep(1000); //TODO: wait for this to fail

        //source is still invalid - no tables, but we should still have gotten a local name assigned
        String komodoName = "superintegrationsource";
        assertEquals(komodoName, dsd.getTeiidName());

        //preview vdb should still be up regardless
        query("select 1", dvName, true);

        //add a source table
        TeiidDataSourceImpl tds = metadata.getDataSource(komodoName);
        Connection c = ((DataSource)tds.getConnectionFactory()).getConnection();
        c.createStatement().execute("create schema DV");
        c.createStatement().execute("create table DV.t (col integer)");

        //issue a refresh over rest, first not valid
        ResponseEntity<KomodoStatusObject> statusResponse = restTemplate.postForEntity(
                "/v1/metadata/refresh-schema/xyz", rdv, KomodoStatusObject.class);
        assertEquals(HttpStatus.NOT_FOUND, statusResponse.getStatusCode());
        //now valid
        statusResponse = restTemplate.postForEntity(
                "/v1/metadata/refresh-schema/{komodoName}", rdv, KomodoStatusObject.class, komodoName);
        assertEquals(HttpStatus.OK, statusResponse.getStatusCode());

        for (int i = 0; i < 10; i++) {
            Thread.sleep(1000); //TODO: a better wait for this to succeed
            try {
                query("select col from superintegrationsource.t union select 1 as col", dvName, true);
                break;
            } catch (AssertionError e) {
                if (i == 9) {
                    throw e;
                }
            }
        }

        //test that unqualified does not work
        query("select col from t union select 1 as col", dvName, false);

        ResponseEntity<List> sourceStatusResponse = restTemplate.getForEntity("/v1/metadata/syndesisSourceStatuses", List.class);
        assertEquals(HttpStatus.OK, sourceStatusResponse.getStatusCode());
        Map status = (Map)sourceStatusResponse.getBody().get(0);
        assertEquals(0, ((List)status.get("errors")).size());
        assertEquals("ACTIVE", status.get("schemaState"));
        assertEquals(Boolean.FALSE, status.get("loading"));

        //add another source table
        c.createStatement().execute("create table DV.t2 (col integer)");
        //update through the synchronizer
        syndesisConnectionSynchronizer.addConnection(dsd, true);

        for (int i = 0; i < 10; i++) {
            Thread.sleep(1000); //TODO: a better wait for this to succeed
            try {
                query("select col from superintegrationsource.t2 union select 1 as col", dvName, true);
                break;
            } catch (AssertionError e) {
                if (i == 9) {
                    throw e;
                }
            }
        }

        c.createStatement().execute("create table DV.t3 (col integer)");

        //manually call the timed refresh - there's more refactoring to do to isolate syndesis calls
        syndesisConnectionSynchronizer.synchronizeConnections(true, Arrays.asList(dsd));

        for (int i = 0; i < 10; i++) {
            Thread.sleep(1000); //TODO: a better wait for this to succeed
            try {
                query("select col from superintegrationsource.t3 union select 1 as col", dvName, true);
                break;
            } catch (AssertionError e) {
                if (i == 9) {
                    throw e;
                }
            }
        }

        c.createStatement().execute("drop schema DV CASCADE");

        syndesisConnectionSynchronizer.addConnection(dsd, true);

        //the same query should now fail that the schema was dropped
        for (int i = 0; i < 10; i++) {
            Thread.sleep(1000); //TODO: a better wait for this to succeed
            try {
                query("select col from superintegrationsource.t2 union select 1 as col", dvName, false);
                break;
            } catch (AssertionError e) {
                if (i == 9) {
                    throw e;
                }
            }
        }

        sourceStatusResponse = restTemplate.getForEntity("/v1/metadata/syndesisSourceStatuses", List.class);
        assertEquals(HttpStatus.OK, sourceStatusResponse.getStatusCode());
        status = (Map)sourceStatusResponse.getBody().get(0);
        assertEquals(1, ((List)status.get("errors")).size());
        assertEquals("FAILED", status.get("schemaState"));
        assertEquals(Boolean.FALSE, status.get("loading"));

        ResponseEntity<List> virts = restTemplate.getForEntity("/v1/workspace/dataservices", List.class);
        assertEquals(HttpStatus.OK, virts.getStatusCode());
        assertEquals(1, virts.getBody().size());
        Map virt = (Map)virts.getBody().get(0);
        assertEquals("testSourceRefresh", virt.get("keng__id"));
    }
}
