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
package org.komodo.rest.relational.json;

import static org.junit.Assert.assertEquals;

import java.net.URLDecoder;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.mockito.Mockito;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataserviceSerializerTest extends AbstractSerializerTest  {

    private static final String DESCRIPTION = "my description";

    private static final String JSON = OPEN_BRACE + NEW_LINE +
        "  \"" + BASE_URI + "\": \"" + MY_BASE_URI + "\"," + NEW_LINE +
        "  \"keng__id\": \"" + DATASERVICE_NAME + "\"," + NEW_LINE +
        "  \"keng__kType\": \"Dataservice\"," + NEW_LINE +
        "  \"tko__description\": \"my description\"," + NEW_LINE +
        "  \"serviceVdbVersion\": \"1\"," + NEW_LINE +
        "  \"keng__hasChildren\": true," + NEW_LINE +
        "  \"publishedState\": \"NOTFOUND\"," + NEW_LINE +
        "  \"keng___links\": [" + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"self\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + DATASERVICE_DATA_PATH + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"parent\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + "/workspace/dataservices\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"vdbs\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + DATASERVICE_DATA_PATH + "/Vdbs\"" + NEW_LINE +
        "    " + CLOSE_BRACE + NEW_LINE +
        "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
        CLOSE_BRACE;

    private RestDataservice dataservice;

    @Before
    public void init() throws Exception {
        Vdb serviceVdb = Mockito.mock(Vdb.class);
        Mockito.when(serviceVdb.getName()).thenReturn("ServiceVdb");
        Mockito.when(serviceVdb.getVersion()).thenReturn(1);

        Dataservice theService = Mockito.mock(Dataservice.class);
        Mockito.when(theService.getName()).thenReturn(DATASERVICE_NAME);
        Mockito.when(theService.getServiceVdbName()).thenReturn("ServiceVdb");

        this.dataservice = new RestDataservice(MY_BASE_URI, theService, false, serviceVdb);
        this.dataservice.setDescription(DESCRIPTION);
    }

    @Test
    public void shouldExportJson() throws Exception {
        String json = KomodoJsonMarshaller.marshall( this.dataservice );
        json = URLDecoder.decode(json, "UTF-8");
        assertEquals(JSON, json);
    }

    @Test
    public void shouldImportJson() {
        final RestDataservice descriptor = KomodoJsonMarshaller.unmarshall( JSON, RestDataservice.class );
        assertEquals(DATASERVICE_NAME, descriptor.getId());
        assertEquals(DESCRIPTION, descriptor.getDescription());
        assertEquals(3, descriptor.getLinks().size());
        assertEquals(true, descriptor.getProperties().isEmpty());
    }

    @Test( expected = Exception.class )
    public void shouldNotExportJsonWhenNameIsMissing() {
        final RestDataservice descriptor = new RestDataservice();
        KomodoJsonMarshaller.marshall( descriptor );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
        KomodoJsonMarshaller.unmarshall( malformed, RestDataservice.class );
    }

}
