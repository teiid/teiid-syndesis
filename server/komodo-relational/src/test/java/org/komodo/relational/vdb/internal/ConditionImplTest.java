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
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ConditionImplTest extends RelationalModelTest {

    private Condition condition;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole( getTransaction(), "dataRole" );
        final Permission permission = dataRole.addPermission( getTransaction(), "permission" );
        this.condition = permission.addCondition( getTransaction(), "condition" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.condition.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotCondition() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ConditionImpl( getTransaction(), _repo, this.condition.getParent( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveConstraintDefaultValueAfterConstruction() throws Exception {
        assertThat( this.condition.isConstraint( getTransaction() ), is( Condition.DEFAULT_CONSTRAINT ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.condition.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.Permission.Condition.CONDITION ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.condition.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_CONDITION));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.condition.getPropertyNames( getTransaction() );
        final String[] rawProps = this.condition.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentPermission() throws Exception {
        assertThat( this.condition.getParent( getTransaction() ), is( instanceOf( Permission.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.condition.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.condition.getPropertyNames( getTransaction() );
        final Filter[] filters = this.condition.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.condition.rename( getTransaction(), newName );
        assertThat( this.condition.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetConstraintValue() throws Exception {
        final boolean newValue = !Condition.DEFAULT_CONSTRAINT;
        this.condition.setConstraint( getTransaction(), newValue );
        assertThat( this.condition.isConstraint( getTransaction() ), is( newValue ) );
    }

}
