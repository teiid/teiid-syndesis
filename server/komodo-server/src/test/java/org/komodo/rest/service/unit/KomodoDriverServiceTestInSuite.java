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
package org.komodo.rest.service.unit;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import java.net.URI;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.RestConnectionDriver;

@SuppressWarnings( {"javadoc", "nls"} )
public class KomodoDriverServiceTestInSuite extends AbstractKomodoServiceTest {

    public KomodoDriverServiceTestInSuite() throws Exception {
        super();
    }

    @Rule
    public TestName testName = new TestName();

    @Test
    public void shouldReturnEmptyListWhenNoDriversInWorkspace() throws Exception {
        URI uri = uriBuilder().workspaceDriversUri();
        HttpGet request = jsonRequest(uri, RequestType.GET);
        HttpResponse response = execute(request);

        okResponse(response);
        String entity = extractResponse(response);
        assertThat(entity, is(notNullValue()));

        RestConnectionDriver[] drivers = KomodoJsonMarshaller.unmarshallArray(entity, RestConnectionDriver[].class);

        assertNotNull(drivers);
    }

}
