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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VirtualProcedureImplTest extends RelationalModelTest {

    private static final String AS_ClAUSE = "BEGIN SELECT a,b FROM x; END";

    private VirtualProcedure procedure;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.procedure = model.addVirtualProcedure( getTransaction(), "myProcedure" );
        commit();
    }

    @Test
    public void shouldClearAsClauseWithEmptyString() throws Exception {
        this.procedure.setAsClauseStatement( getTransaction(), AS_ClAUSE );
        this.procedure.setAsClauseStatement( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.procedure.getAsClauseStatement( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldClearAsClauseWithNullString() throws Exception {
        this.procedure.setAsClauseStatement( getTransaction(), AS_ClAUSE );
        this.procedure.setAsClauseStatement( getTransaction(), null );
        assertThat( this.procedure.getAsClauseStatement( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldFailConstructionIfNotVirtualProcedure() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new VirtualProcedureImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.procedure.getSchemaElementType( getTransaction() ), is( SchemaElementType.VIRTUAL ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.procedure.getTypeIdentifier( getTransaction() ), is(KomodoType.VIRTUAL_PROCEDURE));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( getTransaction() );
        final String[] rawProps = this.procedure.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( getTransaction() );
        final Filter[] filters = this.procedure.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.procedure.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.procedure.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveAsClauseStatementPropertyAfterConstruction() throws Exception {
        assertThat( this.procedure.getAsClauseStatement( getTransaction() ), is( nullValue() ) );
        assertThat( this.procedure.hasProperty( getTransaction(), TeiidDdlLexicon.CreateProcedure.STATEMENT ), is( false ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.procedure.rename( getTransaction(), newName );
        assertThat( this.procedure.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetAsClauseProperty() throws Exception {
        final String value = AS_ClAUSE;
        this.procedure.setAsClauseStatement( getTransaction(), value );
        assertThat( this.procedure.getAsClauseStatement( getTransaction() ), is( value ) );
        assertThat( this.procedure.getProperty( getTransaction(), TeiidDdlLexicon.CreateProcedure.STATEMENT ).getStringValue( getTransaction() ),
                    is( value ) );
    }

}
