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
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class PushdownFunctionImplTest extends RelationalModelTest {

    private PushdownFunction function;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.function = model.addPushdownFunction( getTransaction(), "function" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotPushdownFunction() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new PushdownFunctionImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingResultSetIfOneDoesNotExist() throws Exception {
        this.function.removeResultSet( getTransaction() );
    }

    @Test
    public void shouldGetOnlyResultSetWhenGettingChildren() throws Exception {
        final TabularResultSet resultSet = this.function.setResultSet( getTransaction(), TabularResultSet.class );
        assertThat( this.function.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.function.getChildren( getTransaction() )[0], is( ( KomodoObject )resultSet ) );
    }

    @Test
    public void shouldGetChildren() throws Exception {
        this.function.addParameter( getTransaction(), "param" );
        this.function.setResultSet( getTransaction(), DataTypeResultSet.class );
        assertThat( this.function.getChildren( getTransaction() ).length, is( 2 ) );
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.function.getSchemaElementType( getTransaction() ), is( SchemaElementType.FOREIGN ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.function.getTypeIdentifier( getTransaction() ), is(KomodoType.PUSHDOWN_FUNCTION));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( getTransaction() );
        final String[] rawProps = this.function.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( getTransaction() );
        final Filter[] filters = this.function.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.function.setAggregate( getTransaction(), true );
        this.function.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.function.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveResultSetAfterConstruction() throws Exception {
        assertThat( this.function.getResultSet( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveResultSet() throws Exception {
        this.function.setResultSet( getTransaction(), TabularResultSet.class );
        this.function.removeResultSet( getTransaction() );
        assertThat( this.function.getResultSet( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.function.rename( getTransaction(), newName );
        assertThat( this.function.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetDataTypeResultSet() throws Exception {
        assertThat( this.function.setResultSet( getTransaction(), DataTypeResultSet.class ), is( notNullValue() ) );
        assertThat( this.function.getResultSet( getTransaction() ), is( instanceOf( DataTypeResultSet.class ) ) );
    }

    @Test
    public void shouldSetTabularResultSet() throws Exception {
        assertThat( this.function.setResultSet( getTransaction(), TabularResultSet.class ), is( notNullValue() ) );
        assertThat( this.function.getResultSet( getTransaction() ), is( instanceOf( TabularResultSet.class ) ) );
    }

}
