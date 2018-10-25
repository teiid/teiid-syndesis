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

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.RestConnectionDriver;

@SuppressWarnings( {"javadoc", "nls"} )
@net.jcip.annotations.NotThreadSafe
public class TestKomodoDriverService extends AbstractKomodoServiceTest {

    public TestKomodoDriverService() throws Exception {
        super();
    }

    @Rule
    public TestName testName = new TestName();

    @Test
    @Ignore
    public void shouldGetDrivers() throws Exception {
        String driverName = "shouldGetDrivers";
        createDriver(driverName);

        // get
        URI uri = uriBuilder().workspaceDriversUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = executeOk(request);


        String entities = extractResponse(response);
        assertNotNull(entities);
        
        assertThat(entities, is(notNullValue()));

        // System.out.println("Response:\n" + entities);
        // make sure the Driver JSON document is returned for each driver
        RestConnectionDriver[] driver = KomodoJsonMarshaller.unmarshallArray(entities, RestConnectionDriver[].class);

        assertEquals(1, driver.length);
        RestConnectionDriver myDriver = driver[0];
        assertTrue(driverName.equals(myDriver.getName()));
    }
    
    @Test
    public void shouldReturnEmptyListWhenNoDriversInWorkspace() throws Exception {
        URI uri = uriBuilder().workspaceDriversUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        System.out.println("Response:\n" + entity);

        RestConnectionDriver[] drivers = KomodoJsonMarshaller.unmarshallArray(entity, RestConnectionDriver[].class);
        
        assertNotNull(drivers);
    }
    
}
