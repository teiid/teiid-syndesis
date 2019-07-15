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
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class StatementOptionImplTest extends RelationalModelTest {

    private static final String NAME = "statementoption";

    private StatementOptionImpl option;

    @Before
    public void init() throws Exception {
        final TableImpl table = createTable();
        this.option = RelationalModelFactory.createStatementOption( getTransaction(), _repo, table, NAME, "initialValue" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.option.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotStatementOption() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new StatementOptionImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.option.hasDescriptor( getTransaction(), StandardDdlLexicon.TYPE_STATEMENT_OPTION ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.option.getTypeIdentifier( getTransaction() ), is(KomodoType.STATEMENT_OPTION));
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.option.getName( getTransaction() ), is( NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.option.getPropertyNames( getTransaction() );
        final String[] rawProps = this.option.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.option.addChild( getTransaction(), "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowEmptyOptionValueProperty() throws Exception {
        this.option.setOption( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowNullOptionValueProperty() throws Exception {
        this.option.setOption( getTransaction(), null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.option.getPropertyNames( getTransaction() );
        final Filter[] filters = ((StatementOptionImpl)this.option).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.option.rename( getTransaction(), newName );
        assertThat( this.option.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetOptionValueProperty() throws Exception {
        final String value = "optionvalue";
        this.option.setOption( getTransaction(), value );
        assertThat( this.option.getOption( getTransaction() ), is( value ) );
        assertThat( this.option.getProperty( getTransaction(), StandardDdlLexicon.VALUE ).getStringValue( getTransaction() ), is( value ) );
    }

}
