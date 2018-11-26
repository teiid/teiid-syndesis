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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class UniqueConstraintImplTest extends RelationalModelTest {

    private static final String NAME = "uniqueconstraint";

    private Table table;
    private UniqueConstraint uniqueConstraint;

    @Before
    public void init() throws Exception {
        this.table = createTable();
        this.uniqueConstraint = this.table.addUniqueConstraint( getTransaction(), NAME );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.uniqueConstraint.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotUniqueConstraint() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new UniqueConstraintImpl( getTransaction(), _repo, this.table.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat( this.uniqueConstraint.getConstraintType(), is( TableConstraint.ConstraintType.UNIQUE ) );
        assertThat( this.uniqueConstraint.getRawProperty( getTransaction(), TeiidDdlLexicon.Constraint.TYPE ).getStringValue( getTransaction() ),
                    is( TableConstraint.ConstraintType.UNIQUE.toValue() ) );
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.uniqueConstraint.hasDescriptor( getTransaction(), TeiidDdlLexicon.Constraint.TABLE_ELEMENT ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.uniqueConstraint.getTypeIdentifier( getTransaction() ), is(KomodoType.UNIQUE_CONSTRAINT));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.uniqueConstraint.getPropertyNames( getTransaction() );
        final String[] rawProps = this.uniqueConstraint.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat( this.uniqueConstraint.getParent( getTransaction() ), is( instanceOf( Table.class ) ) );
        assertThat( this.uniqueConstraint.getTable( getTransaction() ), is( this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.uniqueConstraint.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.uniqueConstraint.getPropertyNames( getTransaction() );
        final Filter[] filters = this.uniqueConstraint.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.uniqueConstraint.rename( getTransaction(), newName );
        assertThat( this.uniqueConstraint.getName( getTransaction() ), is( newName ) );
    }

}
