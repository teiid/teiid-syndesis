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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.lexicon.vdb.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class MaskImplTest extends RelationalModelTest {

    private Mask mask;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole( getTransaction(), "dataRole" );
        final Permission permission = dataRole.addPermission( getTransaction(), "permission" );
        this.mask = permission.addMask( getTransaction(), "mask" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.mask.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotMask() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new MaskImpl( getTransaction(), _repo, this.mask.getParent( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.mask.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.Permission.Mask.MASK ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.mask.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_MASK));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.mask.getPropertyNames( getTransaction() );
        final String[] rawProps = this.mask.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentPermission() throws Exception {
        assertThat( this.mask.getParent( getTransaction() ), is( instanceOf( Permission.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.mask.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.mask.getPropertyNames( getTransaction() );
        final Filter[] filters = this.mask.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveOrderAfterConstruction() throws Exception {
        assertThat( this.mask.getOrder( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.mask.rename( getTransaction(), newName );
        assertThat( this.mask.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetOrder() throws Exception {
        final String newValue = "newOrder";
        this.mask.setOrder( getTransaction(), newValue );
        assertThat( this.mask.getOrder( getTransaction() ), is( newValue ) );
    }

}
