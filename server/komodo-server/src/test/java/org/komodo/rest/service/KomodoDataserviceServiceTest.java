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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.TeiidDataSource;
import org.komodo.metadata.internal.DefaultMetadataInstance;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.repository.KomodoRepositoryConfiguration;
import org.komodo.repository.WorkspaceManagerImpl;
import org.komodo.rest.KomodoJsonMarshaller;
import org.komodo.rest.datavirtualization.ImportPayload;
import org.komodo.rest.datavirtualization.KomodoStatusObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@DataJpaTest
@ContextConfiguration(classes = {KomodoRepositoryConfiguration.class, ServiceTestConfiguration.class})
public class KomodoDataserviceServiceTest {
	
	@Autowired
	private WorkspaceManagerImpl workspaceManagerImpl;
	
	@Autowired
	private KomodoDataserviceService komodoDataserviceService;
	
	@Autowired
	private DefaultMetadataInstance metadataInstance;
	
	@Test public void testImport() throws Exception {
		ImportPayload payload = new ImportPayload();
		payload.setTables(Arrays.asList("tbl"));
		
		Response r = komodoDataserviceService.importViews("dv", "source", payload, Arrays.asList(MediaType.APPLICATION_JSON_TYPE));
		//dv not found
		assertEquals(404, r.getStatus());
		
		workspaceManagerImpl.createDataVirtualization("dv");
		
		r = komodoDataserviceService.importViews("dv", "source", payload, Arrays.asList(MediaType.APPLICATION_JSON_TYPE));
		//source not found
		assertEquals(404, r.getStatus());
		
		Map<String, String> props = new HashMap<>();
		props.put(TeiidOpenShiftClient.ID, "someid");
		props.put(TeiidDataSource.DATASOURCE_DRIVERNAME, "h2");
		
		metadataInstance.createDataSource("source", "h2", props);
		
		r = komodoDataserviceService.importViews("dv", "source", payload, Arrays.asList(MediaType.APPLICATION_JSON_TYPE));
		//source not found - as the properties are not valid
		assertEquals(404, r.getStatus());
		
		//add the schema definition - so that we don't really need the datasource, and redeploy
		workspaceManagerImpl.createOrUpdateSchema("someid", "source", 
				"create foreign table tbl (col string) options (\"teiid_rel:fqn\" 'fqn');");
		metadataInstance.undeployDynamicVdb(KomodoMetadataService.getWorkspaceSourceVdbName("source"));
		
		r = komodoDataserviceService.importViews("dv", "source", payload, Arrays.asList(MediaType.APPLICATION_JSON_TYPE));
		KomodoStatusObject kso = KomodoJsonMarshaller.unmarshall(r.getEntity().toString(), KomodoStatusObject.class);
		assertEquals(1, kso.getAttributes().size());
		
		String id = kso.getAttributes().values().iterator().next();
		
		ViewDefinition vd = workspaceManagerImpl.findViewDefinition(id);
		vd.setId("consistent");
		assertEquals("{\n" + 
				"  \"compositions\" : [ ],\n" + 
				"  \"dataVirtualizationName\" : \"dv\",\n" + 
				"  \"id\" : \"consistent\",\n" + 
				"  \"isComplete\" : false,\n" + 
				"  \"isUserDefined\" : false,\n" + 
				"  \"name\" : \"tbl\",\n" + 
				"  \"projectedColumns\" : [ {\n" + 
				"    \"name\" : \"ALL\",\n" + 
				"    \"selected\" : false\n" + 
				"  } ],\n" + 
				"  \"sourcePaths\" : [ \"connection=source/fqn\" ]\n" + 
				"}", KomodoJsonMarshaller.marshall(vd));
	}
	
}
