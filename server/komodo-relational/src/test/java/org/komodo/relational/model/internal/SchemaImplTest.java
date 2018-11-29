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
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

@SuppressWarnings( {"javadoc", "nls"} )
public class SchemaImplTest extends RelationalModelTest {

    private static final String DDL_VIEW = "CREATE VIEW G1 (" + NEW_LINE +
                                                                        TAB + "e1 integer," + NEW_LINE +
                                                                        TAB + "e2 varchar" + NEW_LINE +
                                                                        ") OPTIONS (\"CARDINALITY\" '1234567954432')" + NEW_LINE +
                                                                        "AS" + NEW_LINE +
                                                                        "SELECT e1, e2 FROM foo.bar;\n";

    private static final String NAME = "schema";

    private Schema schema;

    private KomodoObject workspace;

    @Before
    public void init() throws Exception {
        WorkspaceManager manager = WorkspaceManager.getInstance(_repo, getTransaction());
        workspace = _repo.komodoWorkspace(getTransaction());
        this.schema = manager.createSchema(getTransaction(), workspace, NAME);
        commit();
    }

    private void setRenditionValueAwaitSequencing( final String value ) throws Exception {
        this.schema.setRendition( getTransaction(), value );
        commit();

//        traverse( getTransaction(), this.schema.getAbsolutePath() );
    }

    @Test
    public void shouldFailConstructionIfNotSchema() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new SchemaImpl(getTransaction(), _repo, workspace.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldAllowEmptyRendition() throws Exception {
        this.schema.setRendition(getTransaction(), EMPTY_STRING);
        String rendition = this.schema.getRendition(getTransaction());
        assertThat(rendition, is(notNullValue()));
        assertThat(rendition.isEmpty(), is(true));
    }

    @Test
    public void shouldAllowNullRendition() throws Exception {
        this.schema.setRendition(getTransaction(), null);
        String rendition = this.schema.getRendition(getTransaction());
        assertThat(rendition, is(notNullValue()));
        assertThat(rendition.isEmpty(), is(true));
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.schema.rename( getTransaction(), newName );
        assertThat( this.schema.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldsetRendition() throws Exception {
        setRenditionValueAwaitSequencing(DDL_VIEW);
        assertThat(this.schema.getRendition(getTransaction()), is(DDL_VIEW));
    }

    @Test
    public void shouldExportEmptyDdl() throws Exception {
        byte[] fragmentBytes = this.schema.export(getTransaction(), new Properties());
        final String fragment = new String(fragmentBytes);
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportInvalidDdl() throws Exception {
        this.schema.setRendition( getTransaction(), "This is not ddl syntax" );

        // Invalid ddl
        commit(UnitOfWork.State.ERROR);

//        traverse( getTransaction(), this.schema.getAbsolutePath() );

        byte[] fragmentBytes = this.schema.export(getTransaction(), new Properties());
        final String fragment = new String(fragmentBytes);
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(true));
    }

    @Test
    public void shouldExportDdl() throws Exception {
        setRenditionValueAwaitSequencing(DDL_VIEW);

        // test
        byte[] fragmentBytes = this.schema.export(getTransaction(), new Properties());
        final String fragment = new String(fragmentBytes);
        assertThat(fragment, is(notNullValue()));
        assertThat(fragment.isEmpty(), is(false));
        assertEquals(DDL_VIEW, fragment);
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.schema.getTypeIdentifier( getTransaction() ), is(KomodoType.SCHEMA));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.schema.getPropertyNames( getTransaction() );
        final String[] rawProps = this.schema.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.schema.getPropertyNames( getTransaction() );
        final Filter[] filters = this.schema.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

}
