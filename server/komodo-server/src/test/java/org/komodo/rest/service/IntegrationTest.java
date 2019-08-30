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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.StringConstants;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.query.QSResult;
import org.komodo.rest.Application;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.CredentialsProvider;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.KomodoQueryAttribute;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.datavirtualization.RestDataVirtualization;
import org.komodo.rest.service.IntegrationTest.IntegrationTestConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.context.TestConfiguration;
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
    }

    @Autowired
    private TestRestTemplate restTemplate;

    @Test
    public void testAbout() {
        ResponseEntity<KomodoStatusObject> response = restTemplate.getForEntity("/v1/service/about", KomodoStatusObject.class);
        assertEquals(V1Constants.App.name(), response.getBody().getAttributes().get(KomodoUtilService.APP_NAME));
        assertEquals(HttpStatus.OK, response.getStatusCode());
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
        rdv.setName("dv");
        rdv.setDescription("description");

        ResponseEntity<String> response = restTemplate.postForEntity(
                "/v1/workspace/dataservices/dv", rdv, String.class);
        assertEquals(HttpStatus.OK, response.getStatusCode());

        ViewDefinition vd = new ViewDefinition("dv", "myview");
        vd.setComplete(true);
        vd.setDdl("create view myview as select 1 as col");
        vd.setUserDefined(true);

        ResponseEntity<KomodoStatusObject> stashStatus = restTemplate.exchange(
                "/v1/service/userProfile/viewEditorState", HttpMethod.PUT,
                new HttpEntity<ViewDefinition>(vd), KomodoStatusObject.class);

        assertEquals(HttpStatus.OK, stashStatus.getStatusCode());
        String id = stashStatus.getBody().getAttributes()
                .get(StringConstants.ID_LABEL);

        ViewDefinition vd2 = new ViewDefinition("dv", "myview2");
        vd2.setComplete(true);
        vd2.setDdl("create view myview as select * from myview");
        vd2.setUserDefined(true);

        ResponseEntity<KomodoStatusObject> stashStatus2 = restTemplate.exchange(
                "/v1/service/userProfile/viewEditorState", HttpMethod.PUT,
                new HttpEntity<ViewDefinition>(vd2), KomodoStatusObject.class);

        assertEquals(HttpStatus.OK, stashStatus.getStatusCode());
        String id2 = stashStatus2.getBody().getAttributes()
                .get(StringConstants.ID_LABEL);
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
                String.class, "dv", "myView");
        assertEquals(HttpStatus.OK, validate.getStatusCode());
        assertNotNull(validate.getBody());

        query("select * from views.myview", true);

        //myview2 is not yet valid
        query("select * from views.myview2", false);

        //correct it
        vd2.setDdl("create view myview2 as select * from myview");
        restTemplate.exchange(
                "/v1/service/userProfile/viewEditorState", HttpMethod.PUT,
                new HttpEntity<ViewDefinition>(vd2), KomodoStatusObject.class);

        query("select * from views.myview2", true);
    }


    private void query(String queryString, boolean ok) {
        KomodoQueryAttribute queryAttribute = new KomodoQueryAttribute();
        queryAttribute.setQuery(queryString);
        queryAttribute.setTarget("dv");
        ResponseEntity<QSResult> query = restTemplate.postForEntity(
                "/v1/metadata/query", queryAttribute, QSResult.class);
        if (ok) {
            assertEquals(HttpStatus.OK, query.getStatusCode());
            QSResult result = query.getBody();
            assertEquals(1, result.getColumns().size());
            assertEquals(1, result.getRows().size());
        } else {
            assertNotEquals(HttpStatus.OK, query.getStatusCode());
            //TODO: the error handling here is off
            //a resolving error is 500 status - if we open up what can
            //be queried, then it needs to be bad request
        }
    }
}
