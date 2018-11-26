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
package org.komodo.rest;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertThat;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.RestLink.LinkType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestLinkTest {

    private static final LinkType LINK_TYPE = LinkType.SELF;

    private static final LinkType LINK_TYPE_2 = LinkType.PARENT;

    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();

    @Test
    public void shouldBeEqual() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI );
        final RestLink thatLink = new RestLink( thisLink.getRel(), thisLink.getHref());
        assertThat( thisLink, is( thatLink ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypeIsNull() {
        new RestLink( null, URI );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypeIsNull2() {
        new RestLink( null, URI);
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI);
        final RestLink thatLink = new RestLink( thisLink.getRel(), thisLink.getHref());
        assertThat( thisLink.hashCode(), is( thatLink.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenHrefDifferent() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI);
        final RestLink thatLink = new RestLink( thisLink.getRel(),
                                                UriBuilder.fromUri( "http://org.komodo:1234" ).build());
        assertThat( thisLink.getHref(), is( not( thatLink.getHref() ) ) );
        assertThat( thisLink, is( not( thatLink ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenRelDifferent() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI);
        final RestLink thatLink = new RestLink( LINK_TYPE_2, thisLink.getHref());
        assertThat( thisLink.getRel(), is( not( thatLink.getRel() ) ) );
        assertThat( thisLink, is( not( thatLink ) ) );
    }

}
