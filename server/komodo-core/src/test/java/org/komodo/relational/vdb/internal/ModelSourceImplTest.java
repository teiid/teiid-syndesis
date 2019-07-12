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
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ModelSourceImplTest extends RelationalModelTest {

    private ModelSourceImpl source;

    @Before
    public void init() throws Exception {
        final ModelImpl model = createModel();
        this.source = model.addSource( getTransaction(), "source" );
        commit();
    }

    @Test
    public void shouldBeAbleToSetEmptyJndi() throws Exception {
        this.source.setJndiName( getTransaction(), "blah" );
        this.source.setJndiName( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.source.getJndiName( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldBeAbleToSetEmptyTranslator() throws Exception {
        this.source.setTranslatorName( getTransaction(), "blah" );
        this.source.setTranslatorName( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.source.getTranslatorName( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldBeAbleToSetNullJndi() throws Exception {
        this.source.setJndiName( getTransaction(), "blah" );
        this.source.setJndiName( getTransaction(), null );
        assertThat( this.source.getJndiName( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldBeAbleToSetNullTranslator() throws Exception {
        this.source.setTranslatorName( getTransaction(), "blah" );
        this.source.setTranslatorName( getTransaction(), null );
        assertThat( this.source.getTranslatorName( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.source.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotSource() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new TranslatorImpl( getTransaction(), _repo, this.source.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.source.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Source.SOURCE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.source.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_MODEL_SOURCE));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.source.getPropertyNames( getTransaction() );
        final String[] rawProps = this.source.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentModel() throws Exception {
        assertThat( this.source.getParent( getTransaction() ), is( instanceOf( ModelImpl.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.source.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.source.getPropertyNames( getTransaction() );
        final Filter[] filters = ((ModelSourceImpl)this.source).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveJndiNameAfterConstruction() throws Exception {
        assertThat( this.source.getJndiName( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveTranslatorNameAfterConstruction() throws Exception {
        assertThat( this.source.getTranslatorName( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.source.rename( getTransaction(), newName );
        assertThat( this.source.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetJndiName() throws Exception {
        final String name = "jndiName";
        this.source.setJndiName( getTransaction(), name );
        assertThat( this.source.getJndiName( getTransaction() ), is( name ) );
    }

    @Test
    public void shouldSetTranslatorName() throws Exception {
        final String name = "translatorName";
        this.source.setTranslatorName( getTransaction(), name );
        assertThat( this.source.getTranslatorName( getTransaction() ), is( name ) );
    }

}
