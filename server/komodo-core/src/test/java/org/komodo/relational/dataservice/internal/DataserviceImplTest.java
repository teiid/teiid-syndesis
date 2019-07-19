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
package org.komodo.relational.dataservice.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

import java.util.Calendar;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataserviceImplTest extends RelationalModelTest {

    private static final String SERVICE_NAME = "myService";

    protected DataserviceImpl dataservice;

    @Before
    public void init() throws Exception {
        this.dataservice = createDataservice( SERVICE_NAME );
    }

    @Test
    public void shouldHaveName() throws Exception {
        assertThat( this.dataservice.getName( ), is( SERVICE_NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( );
        final String[] rawProps = this.dataservice.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( );
        final Filter[] filters = ((DataserviceImpl)this.dataservice).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.dataservice.getPrimaryType( ).getName(), is( DataVirtLexicon.DataService.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.dataservice.getTypeIdentifier( ), is(KomodoType.DATASERVICE));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String descr = "This is a description";
        this.dataservice.setDescription(descr);

        assertThat( this.dataservice.getDescription( ), is( descr ) );
    }

    @Test
    public void shouldSetModifiedBy() throws Exception {
        final String user = "elvis";
        this.dataservice.setModifiedBy( user );
        assertThat( this.dataservice.getModifiedBy( ), is( user ) );
    }

    @Test
    public void shouldSetLastModified() throws Exception {
        final Calendar date = Calendar.getInstance();
        date.set( 2016, 8, 23, 13, 48, 33 );
        this.dataservice.setLastModified( date );
        assertThat( this.dataservice.getLastModified( ), is( date ) );
    }

}
