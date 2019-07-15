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

import org.junit.Before;
import org.junit.Test;
import org.komodo.core.repository.KomodoObject;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ViewImplTest extends RelationalModelTest {

    private ModelImpl model;
    private ViewImpl view;

    @Before
    public void init() throws Exception {
        this.model = createModel();
        this.view = this.model.addView( getTransaction(), "view" );
        commit();
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.view.getTypeIdentifier( getTransaction() ), is(KomodoType.VIEW));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.view.getPropertyNames( getTransaction() );
        final String[] rawProps = this.view.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentModel() throws Exception {
        assertThat( this.view.getParent( getTransaction() ), is( instanceOf( ModelImpl.class ) ) );
        assertThat( this.view.getParent( getTransaction() ), is( ( KomodoObject )this.model ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.view.getPropertyNames( getTransaction() );
        final Filter[] filters = ((ViewImpl)this.view).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.view.rename( getTransaction(), newName );
        assertThat( this.view.getName( getTransaction() ), is( newName ) );
    }

}
