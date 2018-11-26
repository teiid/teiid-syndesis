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
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class LinkSerializerTest {

    private static final String JSON = "{\"rel\":\"self\",\"href\":\"http://localhost:8080\"}";
    private static final LinkType LINK_TYPE = LinkType.SELF;
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();
    private static final LinkSerializer BUILDER = new LinkSerializer();

    @Test
    public void shouldExportJson() throws Exception {
        final RestLink link = new RestLink( LINK_TYPE, URI);
        String json = BUILDER.toJson( link );
        assertEquals(JSON, json);
    }

    @Test
    public void shouldImportJson() throws Exception {
        final RestLink link = BUILDER.fromJson( JSON );
        assertThat( link.getRel(), is( LINK_TYPE ) );
        assertThat( link.getHref(), is( URI ) );
    }

}
