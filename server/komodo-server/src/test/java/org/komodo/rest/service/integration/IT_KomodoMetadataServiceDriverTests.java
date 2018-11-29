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
package org.komodo.rest.service.integration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.util.Base64;
import java.util.Map;
import javax.ws.rs.core.UriBuilder;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpPost;
import org.jboss.arquillian.junit.Arquillian;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoFileAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;

@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public class IT_KomodoMetadataServiceDriverTests extends AbstractKomodoMetadataServiceTest  {

    @Override
    protected int getTestTotalInClass() {
        return 2;
    }

    @Test
    public void shouldDeployDriver() throws Exception {
        undeployDrivers();
        assertNoMysqlDriver();

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.METADATA_SEGMENT)
                                            .path(V1Constants.METADATA_DRIVER)
                                            .build();

        KomodoFileAttributes fileAttr = new KomodoFileAttributes();
        fileAttr.setName(MYSQL_DRIVER);

        InputStream driverStream = TestUtilities.mySqlDriver();
        assertNotNull(driverStream);

        byte[] driverBytes = TestUtilities.streamToBytes(driverStream);
        String content = Base64.getEncoder().encodeToString(driverBytes);
        fileAttr.setContent(content);

        HttpPost request = jsonRequest(uri, RequestType.POST);
        addBody(request, fileAttr);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
        assertEquals(title, status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        assertFalse(attributes.isEmpty());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_DEPLOYED);
        boolean foundValue = false;
        for (String value : attributes.values()) {
            if (deployMsg.equals(value)) {
               foundValue = true;
               break;
            }
        }

        assertTrue(foundValue);

        assertMysqlDriver();
    }

    @Test
    public void shouldUndeployDriver() throws Exception {
        InputStream driverStream = TestUtilities.mySqlDriver();
        assertNotNull(driverStream);
        byte[] driverBytes = TestUtilities.streamToBytes(driverStream);
        File driverFile = File.createTempFile(MYSQL_DRIVER, DOT + JAR);
        FileUtils.write(driverBytes, driverFile);

        getMetadataInstance().deployDataSourceDriver(MYSQL_DRIVER, driverFile);
        assertMysqlDriver();

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.METADATA_SEGMENT)
                                            .path(V1Constants.METADATA_DRIVER)
                                            .path(MYSQL_DRIVER)
                                            .build();

        HttpDelete request = jsonRequest(uri, RequestType.DELETE);
        HttpResponse response = executeOk(request);

        
        String entity = extractResponse(response);
        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        wait(4);

        getMetadataInstance().refresh();

        String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
        assertEquals(title, status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        assertEquals(1, attributes.size());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UNDEPLOYED);
        assertEquals(deployMsg, attributes.values().iterator().next());

        assertNoMysqlDriver();
    }
}
