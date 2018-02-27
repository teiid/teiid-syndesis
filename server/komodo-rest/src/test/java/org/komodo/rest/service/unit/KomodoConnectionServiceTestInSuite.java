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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.net.URI;
import java.util.Properties;

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoConnectionServiceTestInSuite extends AbstractKomodoServiceTest {

    @Rule
    public TestName testName = new TestName();

    public KomodoConnectionServiceTestInSuite() throws Exception {
        super();
    }

    @Test
    public void shouldGetConnections() throws Exception {
        String connectionName = "shouldGetConnections";
        createConnection(connectionName);

        // get
        URI uri = uriBuilder().workspaceConnectionsUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);
        assertThat(entities, is(notNullValue()));

        // System.out.println("Response:\n" + entities);
        // make sure the Dataservice JSON document is returned for each dataservice
        RestConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entities, RestConnection[].class);
        assertTrue(connections.length > 0);

        RestConnection mySource = null;
        for (RestConnection conn : connections) {
            if (connectionName.equals(conn.getId())) {
                mySource = conn;
                break;
            }
        }
        assertNotNull(mySource);
        assertNotNull(mySource.getDataPath());
        assertNotNull(mySource.getkType());
    }
    
    @Test
    public void shouldReturnEmptyListWhenNoDataservicesInWorkspace() throws Exception {
        URI uri = uriBuilder().workspaceDataservicesUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entity, RestConnection[].class);
        assertNotNull(connections);
        assertEquals(0, connections.length);
    }
    
    @Test
    public void shouldGetConnection() throws Exception {
        String connectionName = "shouldGetConnection";
        createConnection(connectionName);

        // get
        Properties settings = uriBuilder().createSettings(SettingNames.CONNECTION_NAME, connectionName);
        uriBuilder().addSetting(settings, SettingNames.PARENT_PATH, uriBuilder().workspaceConnectionsUri());

        URI uri = uriBuilder().connectionUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        String entity = extractResponse(response);
//        System.out.println("Response:\n" + entity);

        RestConnection connection = KomodoJsonMarshaller.unmarshall(entity, RestConnection.class);
        assertNotNull(connection);
        
        assertEquals(connection.getId(), connectionName);
    }

    @Test
    public void shouldFailCreateConnectionNoServiceCatalog() throws Exception {
        String connectionName = "shouldFailCreateConnectionNoServiceCatalog";

        // post
        Properties settings = uriBuilder().createSettings(SettingNames.CONNECTION_NAME, connectionName);
        uriBuilder().addSetting(settings, SettingNames.PARENT_PATH, uriBuilder().workspaceConnectionsUri());

        URI uri = uriBuilder().connectionUri(LinkType.SELF, settings);
        HttpPost request = jsonRequest(uri, RequestType.POST);

        KomodoConnectionAttributes rcAttr = new KomodoConnectionAttributes();
        rcAttr.setDescription("A description");

        addBody(request, rcAttr);
        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("missing one or more required parameters"));
    }

    @Test
    public void shouldFailUpdateConnectionNoServiceCatalog() throws Exception {
        String connectionName = "shouldFailUpdateConnectionNoServiceCatalog";

        createConnection(connectionName);

        // put
        Properties settings = uriBuilder().createSettings(SettingNames.CONNECTION_NAME, connectionName);
        uriBuilder().addSetting(settings, SettingNames.PARENT_PATH, uriBuilder().workspaceConnectionsUri());

        URI uri = uriBuilder().connectionUri(LinkType.SELF, settings);
        HttpPut request = jsonRequest(uri, RequestType.PUT);

        KomodoConnectionAttributes rcAttr = new KomodoConnectionAttributes();
        rcAttr.setDescription("A description");

        addBody(request, rcAttr);
        HttpResponse response = execute(request);

        assertResponse(response, HttpStatus.SC_FORBIDDEN);
        String entity = extractResponse(response);
        assertTrue(entity.contains("missing one or more required parameters"));
    }
}
