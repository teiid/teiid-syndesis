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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.datavirtualization.DataVirtualization;
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
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.http.ResponseEntity;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.server.ResponseStatusException;

@RunWith(SpringRunner.class)
@DataJpaTest
@ContextConfiguration(classes = {KomodoRepositoryConfiguration.class, ServiceTestConfiguration.class})
@DirtiesContext
@SuppressWarnings("nls")
public class KomodoDataserviceServiceTest {

    @Autowired
    private TestEntityManager entityManager;

    @Autowired
    private WorkspaceManagerImpl workspaceManagerImpl;

    @Autowired
    private KomodoDataserviceService komodoDataserviceService;

    @Autowired
    private DefaultMetadataInstance metadataInstance;

    @Test public void testImport() throws Exception {
        ImportPayload payload = new ImportPayload();
        payload.setTables(Arrays.asList("tbl"));

        KomodoStatusObject kso = null;
        try {
            kso = komodoDataserviceService.importViews("dv", "source", payload);
            fail();
        } catch (ResponseStatusException e) {
            //dv not found
        }

        DataVirtualization dv = workspaceManagerImpl.createDataVirtualization("dv");

        try {
            kso = komodoDataserviceService.importViews("dv", "source", payload);
            fail();
        } catch (ResponseStatusException e) {
            //source not found
        }

        Map<String, String> props = new HashMap<>();
        props.put(TeiidOpenShiftClient.ID, "someid");
        props.put(TeiidDataSource.DATASOURCE_DRIVERNAME, "h2");

        metadataInstance.createDataSource("source", "h2", props);

        try {
            kso = komodoDataserviceService.importViews("dv", "source", payload);
            fail();
        } catch (ResponseStatusException e) {
            //source not found - as the properties are not valid
        }

        //add the schema definition - so that we don't really need the datasource, and redeploy
        workspaceManagerImpl.createSchema("someid", "source",
                "create foreign table tbl (col string) options (\"teiid_rel:fqn\" 'schema=s/table=tbl');");
        metadataInstance.undeployDynamicVdb(KomodoMetadataService.getWorkspaceSourceVdbName("source"));

        kso = komodoDataserviceService.importViews("dv", "source", payload);
        assertEquals(1, kso.getAttributes().size());

        String id = kso.getAttributes().values().iterator().next();

        ViewDefinition vd = workspaceManagerImpl.findViewDefinition(id);

        assertTrue(vd.isParsable());

        vd.setId("consistent");
        assertEquals("{\n" +
                "  \"dataVirtualizationName\" : \"dv\",\n" +
                "  \"ddl\" : \"CREATE VIEW tbl (col) AS \\nSELECT col\\nFROM source.tbl;\",\n" +
                "  \"id\" : \"consistent\",\n" +
                "  \"isComplete\" : true,\n" +
                "  \"isUserDefined\" : false,\n" +
                "  \"name\" : \"tbl\",\n" +
                "  \"sourcePaths\" : [ \"schema=source/table=tbl\" ],\n" +
                "  \"version\" : 0\n" +
                "}", KomodoJsonMarshaller.marshall(vd));

        vd.setId(id);

        entityManager.flush();

        assertEquals(Long.valueOf(1), dv.getVersion());
    }

    @Test public void testValidateName() throws Exception {
        ResponseEntity<String> response = komodoDataserviceService.validateDataserviceName("foo");
        assertNull(response.getBody());

        //must end with number/letter
        response = komodoDataserviceService.validateDataserviceName("foo-");
        assertNotNull(response.getBody());

        //bad chars
        response = komodoDataserviceService.validateDataserviceName("%foo&");
        assertNotNull(response.getBody());

        workspaceManagerImpl.createDataVirtualization("foo");

        //conflicts
        response = komodoDataserviceService.validateDataserviceName("FOO");
        assertNotNull(response.getBody());
    }
}
