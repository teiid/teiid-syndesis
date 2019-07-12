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
import org.komodo.core.repository.KomodoObject;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;

@SuppressWarnings( { "javadoc", "nls" } )
public final class PrimaryKeyImplTest extends RelationalModelTest {

    private static final String NAME = "primaryKey";

    private PrimaryKeyImpl primaryKey;
    private TableImpl table;

    @Before
    public void init() throws Exception {
        this.table = createTable();
        this.primaryKey = this.table.setPrimaryKey( getTransaction(), NAME );
        commit();
    }

    @Test
    public void shouldAddColumnReference() throws Exception {
        final ColumnImpl column = this.table.addColumn( getTransaction(), "MyColumn" );
        this.primaryKey.addColumn( getTransaction(), column );
        commit();

        assertThat( this.primaryKey.getColumns( getTransaction() ).length, is( 1 ) );
        assertThat( this.primaryKey.getColumns( getTransaction() )[0], is( column ) );
        assertThat( this.primaryKey.getProperty( getTransaction(), Constraint.REFERENCES ).getValues( getTransaction() ).length, is( 1 ) );
        assertThat( this.primaryKey.getProperty( getTransaction(), Constraint.REFERENCES ).getValues( getTransaction() )[0].toString(),
                    // TODO Is this worth creating a getId() API method??
                    is( ((ColumnImpl)column).getObjectFactory().getId(getTransaction(), (KomodoObject)column).getStringValue( getTransaction() ) ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.primaryKey.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotPrimaryKey() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new PrimaryKeyImpl( getTransaction(), _repo, this.table.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat( this.primaryKey.getConstraintType(), is( TableConstraint.ConstraintType.PRIMARY_KEY ) );
        assertThat( this.primaryKey.getRawProperty( getTransaction(), TeiidDdlLexicon.Constraint.TYPE ).getStringValue( getTransaction() ),
                    is( TableConstraint.ConstraintType.PRIMARY_KEY.toValue() ) );
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.primaryKey.hasDescriptor( getTransaction(), TeiidDdlLexicon.Constraint.TABLE_ELEMENT ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.primaryKey.getName( getTransaction() ), is( NAME ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.primaryKey.getTypeIdentifier( getTransaction() ), is(KomodoType.PRIMARY_KEY));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.primaryKey.getPropertyNames( getTransaction() );
        final String[] rawProps = this.primaryKey.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat( this.primaryKey.getParent( getTransaction() ), is( instanceOf( Table.class ) ) );
        assertThat( this.primaryKey.getTable( getTransaction() ), is( this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.primaryKey.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.primaryKey.getPropertyNames( getTransaction() );
        final Filter[] filters = ((PrimaryKeyImpl)this.primaryKey).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.primaryKey.rename( getTransaction(), newName );
        assertThat( this.primaryKey.getName( getTransaction() ), is( newName ) );
    }

}
