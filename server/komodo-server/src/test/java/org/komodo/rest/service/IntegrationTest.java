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

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.rest.Application;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.CredentialsProvider;
import org.komodo.rest.V1Constants;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.komodo.rest.service.IntegrationTest.IntegrationTestConfiguration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
@ContextConfiguration(classes = {IntegrationTestConfiguration.class, Application.class})
@DirtiesContext
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
	
	@Test public void testAbout() {
		ResponseEntity<KomodoStatusObject> response = restTemplate.getForEntity("/v1/service/about", KomodoStatusObject.class);
		assertEquals(V1Constants.App.name(), response.getBody().getAttributes().get(KomodoUtilService.APP_NAME));
		assertEquals(HttpStatus.OK, response.getStatusCode());
	}
	
}
