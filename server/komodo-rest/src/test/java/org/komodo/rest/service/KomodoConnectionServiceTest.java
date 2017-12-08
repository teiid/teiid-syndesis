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
package org.komodo.rest.service;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.net.URI;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoConnectionServiceTest extends AbstractKomodoServiceTest {

    public static String CONNECTION_NAME = "MyConnection"; 
    
    @Rule
    public TestName testName = new TestName();

    @Test
    public void shouldGetConnections() throws Exception {
        createConnection(CONNECTION_NAME);

        // get
        URI uri = _uriBuilder.workspaceConnectionsUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);

        String entities = extractResponse(response);
        assertThat(entities, is(notNullValue()));

        // System.out.println("Response:\n" + entities);
        // make sure the Dataservice JSON document is returned for each dataservice
        RestConnection[] connection = KomodoJsonMarshaller.unmarshallArray(entities, RestConnection[].class);

        assertEquals(1, connection.length);
        RestConnection mySource = connection[0];
        assertNotNull(mySource.getId());
        assertTrue(CONNECTION_NAME.equals(mySource.getId()));
        assertNotNull(mySource.getDataPath());
        assertNotNull(mySource.getkType());
    }
    
    @Test
    public void shouldReturnEmptyListWhenNoDataservicesInWorkspace() throws Exception {
        URI uri = _uriBuilder.workspaceDataservicesUri();
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
        createConnection(CONNECTION_NAME);

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.CONNECTION_NAME, CONNECTION_NAME);
        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, _uriBuilder.workspaceConnectionsUri());

        URI uri = _uriBuilder.connectionUri(LinkType.SELF, settings);
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        String entity = extractResponse(response);
//        System.out.println("Response:\n" + entity);

        RestConnection connection = KomodoJsonMarshaller.unmarshall(entity, RestConnection.class);
        assertNotNull(connection);
        
        assertEquals(connection.getId(), CONNECTION_NAME);
    }

    @Test
    public void shouldCreateConnection() throws Exception {
        // post
        Properties settings = _uriBuilder.createSettings(SettingNames.CONNECTION_NAME, CONNECTION_NAME);
        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, _uriBuilder.workspaceConnectionsUri());

        URI uri = _uriBuilder.connectionUri(LinkType.SELF, settings);
        HttpPost request = jsonRequest(uri, RequestType.POST);

        KomodoConnectionAttributes rcAttr = new KomodoConnectionAttributes();
        rcAttr.setJndi("jndi:/MySqlDS1");
        rcAttr.setDriver("mysql");
        rcAttr.setJdbc(true);
        rcAttr.setParameter("username", "test");
        rcAttr.setParameter("password", "myPassword");

        addBody(request, rcAttr);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));
//        System.out.println("Response:\n" + entity);

        RestConnection rsObj = KomodoJsonMarshaller.unmarshall(entity, RestConnection.class);
        assertEquals(rcAttr.getDriver(), rsObj.getDriverName());
        assertEquals(rcAttr.getJndi(), rsObj.getJndiName());
        assertEquals(rcAttr.isJdbc(), rsObj.isJdbc());

        List<RestProperty> rsProps = rsObj.getProperties();
        for (Entry<String, Object> parameter : rcAttr.getParameters().entrySet()) {
            RestProperty rsProp = null;

            for (RestProperty rsp : rsProps) {
                if (! rsp.getName().equals(parameter.getKey()))
                    continue;

                rsProp = rsp;
            }

            assertNotNull(parameter.getKey() + " property not handled", rsProp);
            assertEquals(parameter.getValue(), rsProp.getValue());
        }
    }

    @Test
    public void shouldUpdateConnection() throws Exception {
        createConnection(CONNECTION_NAME);

        // put
        Properties settings = _uriBuilder.createSettings(SettingNames.CONNECTION_NAME, CONNECTION_NAME);
        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, _uriBuilder.workspaceConnectionsUri());

        URI uri = _uriBuilder.connectionUri(LinkType.SELF, settings);
        HttpPut request = jsonRequest(uri, RequestType.PUT);

        KomodoConnectionAttributes rcAttr = new KomodoConnectionAttributes();
        rcAttr.setJndi("jndi:/MySqlDS1");
        rcAttr.setDriver("mysql");
        rcAttr.setJdbc(true);
        rcAttr.setParameter("username", "test");
        rcAttr.setParameter("password", "myPassword");

        addBody(request, rcAttr);
        HttpResponse response = executeOk(request);

        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));
//        System.out.println("Response:\n" + entity);

        RestConnection rsObj = KomodoJsonMarshaller.unmarshall(entity, RestConnection.class);
        assertEquals(rcAttr.getDriver(), rsObj.getDriverName());
        assertEquals(rcAttr.getJndi(), rsObj.getJndiName());
        assertEquals(rcAttr.isJdbc(), rsObj.isJdbc());

        List<RestProperty> rsProps = rsObj.getProperties();
        for (Entry<String, Object> parameter : rcAttr.getParameters().entrySet()) {
            RestProperty rsProp = null;

            for (RestProperty rsp : rsProps) {
                if (! rsp.getName().equals(parameter.getKey()))
                    continue;

                rsProp = rsp;
            }

            assertNotNull(parameter.getKey() + " property not handled", rsProp);
            assertEquals(parameter.getValue(), rsProp.getValue());
        }
    }
}
