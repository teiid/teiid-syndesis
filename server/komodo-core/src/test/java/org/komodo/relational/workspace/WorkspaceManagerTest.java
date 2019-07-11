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
package org.komodo.relational.workspace;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;

import java.io.File;
import java.util.Arrays;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.FileUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class WorkspaceManagerTest extends RelationalModelTest {

    private WorkspaceManagerImpl wsMgr;

    private File myFileDir;

    @Before
    public void obtainWorkspaceManager() throws Exception {
        wsMgr = WorkspaceManagerImpl.getInstance(_repo, getTransaction());
    }

    @After
    public void uncacheWorkspaceManager() {
        WorkspaceManagerImpl.uncacheInstance(_repo, getTransaction().getUserName());
        wsMgr = null;

        if (myFileDir != null)
            FileUtils.removeDirectoryAndChildren(myFileDir);
    }

    @Test
    public void shouldCreateDataservice() throws Exception {
        final Dataservice dataservice = this.wsMgr.createDataservice( getTransaction(), null, "service" );
        assertThat( dataservice, is( notNullValue() ) );
        assertThat( _repo.getFromWorkspace( getTransaction(), dataservice.getAbsolutePath() ), is( ( KomodoObject )dataservice ) );
    }

    @Test
    public void shouldCreateVdb() throws Exception {
        final KomodoObject parent = _repo.add(getTransaction(), _repo.komodoWorkspace(getTransaction()).getAbsolutePath(), "parent", null);
        final Vdb vdb = this.wsMgr.createVdb(getTransaction(), parent, "vdb", "externalFilePath");
        assertThat(vdb, is(notNullValue()));
        assertThat(_repo.getFromWorkspace(getTransaction(), vdb.getAbsolutePath()), is((KomodoObject)vdb));
    }

    @Test
    public void shouldCreateVdbWithNullParent() throws Exception {
        final Vdb vdb = this.wsMgr.createVdb(getTransaction(), null, this.name.getMethodName(), "externalFilePath");
        assertThat(vdb, is(notNullValue()));
        assertThat(vdb.getParent(getTransaction()), is(_repo.komodoWorkspace(getTransaction())));
    }

    @Test
    public void shouldDeleteVdb() throws Exception {
        final Vdb vdb = createVdb();
        this.wsMgr.delete(getTransaction(), vdb);
        assertThat(this.wsMgr.findVdbs(getTransaction()).length, is(0));
    }

    @Test
    public void shouldFindAllObjectsOfTypeInWorkspaceWhenPassingInEmptyParentPath() throws Exception {
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for ( int i = 0; i < 5; ++i ) {
            createVdb( ( prefix + ++suffix ), ( VDB_PATH + i ) );
        }

        // create under a folder
        final KomodoObject parent = _repo.add( getTransaction(), null, "blah", null );

        for ( int i = 0; i < 7; ++i ) {
            createVdb( ( prefix + ++suffix ), parent, ( VDB_PATH + i ) );
        }

        commit(); // must save before running a query

        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, StringConstants.EMPTY_STRING, null, false ).length,
                    is( suffix ) );
    }

    @Test
    public void shouldFindAllObjectsOfTypeUnderASpecificParentPath() throws Exception {
        final String prefix = this.name.getMethodName();
        final int numAtRoot = 3;

        // create at root
        for ( int i = 0; i < numAtRoot; ++i ) {
            createVdb( ( prefix + i ), ( VDB_PATH + i ) );
        }

        // create under a folder
        final KomodoObject parent = _repo.add( getTransaction(), null, "blah", null );
        final int expected = 3;

        for ( int i = numAtRoot; i < ( numAtRoot + expected ); ++i ) {
            createVdb( ( prefix + i ), parent, ( VDB_PATH + i ) );
        }

        commit(); // must save before running a query

        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, parent.getAbsolutePath(), null, false ).length,
                    is( expected ) );
    }

    @Test
    public void shouldFindMatchingObjects() throws Exception {
        // create at workspace root
        createVdb( "blah" );
        createVdb( "elvis" );
        createVdb( "sledge" );

        // create under a folder
        final KomodoObject parent = _repo.add( getTransaction(), null, "folder", null );
        createVdb( "john", parent, VDB_PATH );
        createVdb( "paul", parent, VDB_PATH );
        createVdb( "george", parent, VDB_PATH );
        createVdb( "ringo", parent, VDB_PATH );

        commit(); // must save before running a query

        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, null, null, false ).length, is( 7 ) );
        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, null, "*", false ).length, is( 7 ) );
        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, null, "blah", false ).length, is( 1 ) );
        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, null, "*e", false ).length, is( 2 ) );
        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, null, "*o*", false ).length, is( 3 ) );
        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, null, "pa%l", false ).length, is( 1 ) );
        assertThat( this.wsMgr.findByType( getTransaction(), VdbLexicon.Vdb.VIRTUAL_DATABASE, null, "a*", false ).length, is( 0 ) );
    }

    @Test
    public void shouldFindVdbs() throws Exception {
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            createVdb((prefix + ++suffix), (VDB_PATH + i));
        }

        // create under a folder
        final KomodoObject parent = _repo.add(getTransaction(), null, "blah", null);

        for (int i = 0; i < 7; ++i) {
            createVdb( ( prefix + ++suffix ), parent, ( VDB_PATH + i ) );
        }

        commit(); // must save before running a query

        assertThat(this.wsMgr.findVdbs(getTransaction()).length, is(suffix));
    }

    @Test
    public void shouldFindVdbsWhenMixedWithDataservices() throws Exception {
        final String prefix = this.name.getMethodName();
        int vdbCount = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            createVdb((prefix + ++vdbCount), (VDB_PATH + i));
        }

        createDataservice();

        commit(); // must save before running a query

        assertThat(this.wsMgr.findVdbs(getTransaction()).length, is(vdbCount));
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        KomodoType[] types = { Dataservice.IDENTIFIER, Vdb.IDENTIFIER, Schema.IDENTIFIER};
        assertThat( Arrays.asList( this.wsMgr.getChildTypes() ), hasItems( types ) );
        assertThat( this.wsMgr.getChildTypes().length, is( types.length ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowEmptyExternalFilePath() throws Exception {
        this.wsMgr.createVdb(getTransaction(), null, "vdbName", StringConstants.EMPTY_STRING);
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowEmptyTypeWhenFindingObjects() throws Exception {
        this.wsMgr.findByType( getTransaction(), StringConstants.EMPTY_STRING, "/my/path", null, false );
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowEmptyVdbName() throws Exception {
        this.wsMgr.createVdb(getTransaction(), null, StringConstants.EMPTY_STRING, "externalFilePath");
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowNullExternalFilePath() throws Exception {
        this.wsMgr.createVdb(getTransaction(), null, "vdbName", null);
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowNullTypeWhenFindingObjects() throws Exception {
        this.wsMgr.findByType( getTransaction(), null, "/my/path", null, false );
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowNullVdbName() throws Exception {
        this.wsMgr.createVdb(getTransaction(), null, null, "externalFilePath");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowRemove() throws Exception {
        this.wsMgr.remove( getTransaction() );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowRename() throws Exception {
        this.wsMgr.rename( getTransaction(), "newName" );
    }

    @Test
    public void shouldNotFindVdbsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findVdbs(getTransaction()).length, is(0));
    }

    @Test
    public void shouldNotResolveAsVdb() throws Exception {
        final Model model = createModel();
        final KomodoObject kobject = new ObjectImpl(_repo, model.getAbsolutePath(), model.getIndex());
        assertNull(this.wsMgr.resolve(getTransaction(), kobject, Vdb.class));
    }

    @Test
    public void shouldResolveDataservice() throws Exception {
        final Dataservice service = this.wsMgr.createDataservice(getTransaction(), null, "service");
        final KomodoObject kobject = new ObjectImpl(_repo, service.getAbsolutePath(), service.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, Dataservice.class), is(instanceOf(Dataservice.class)));
    }

    @Test
    public void shouldResolveForeignKey() throws Exception {
        final Table refTable = createTable(getDefaultVdbName(), VDB_PATH, "mymodel", "refTable");
        final Table table = createTable();
        final ForeignKey foreignKey = table.addForeignKey(getTransaction(), "foreignKey", refTable);
        final KomodoObject kobject = new ObjectImpl(_repo, foreignKey.getAbsolutePath(), foreignKey.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, ForeignKey.class), is(instanceOf(ForeignKey.class)));
    }

    @Test
    public void shouldResolveModel() throws Exception {
        final Model model = createModel();
        final KomodoObject kobject = new ObjectImpl(_repo, model.getAbsolutePath(), model.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, Model.class), is(instanceOf(Model.class)));
    }

    @Test
    public void shouldResolveModelSource() throws Exception {
        final Model model = createModel();
        final ModelSource source = model.addSource(getTransaction(), "source");
        final KomodoObject kobject = new ObjectImpl(_repo, source.getAbsolutePath(), source.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, ModelSource.class), is(instanceOf(ModelSource.class)));
    }

    @Test
    public void shouldResolvePrimaryKey() throws Exception {
        final Table table = createTable();
        final PrimaryKey pk = table.setPrimaryKey(getTransaction(), "pk");
        final KomodoObject kobject = new ObjectImpl(_repo, pk.getAbsolutePath(), pk.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, PrimaryKey.class), is(instanceOf(PrimaryKey.class)));
    }

    @Test
    public void shouldResolveTable() throws Exception {
        final Table table = createTable();
        final KomodoObject kobject = new ObjectImpl(_repo, table.getAbsolutePath(), table.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, Table.class), is(instanceOf(Table.class)));
    }

    @Test
    public void shouldResolveUniqueConstraint() throws Exception {
        final Table table = createTable();
        final UniqueConstraint uniqueConstraint = table.addUniqueConstraint(getTransaction(), "uniqueConstraint");
        final KomodoObject kobject = new ObjectImpl(_repo, uniqueConstraint.getAbsolutePath(), uniqueConstraint.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, UniqueConstraint.class), is(instanceOf(UniqueConstraint.class)));
    }

    @Test
    public void shouldResolveVdb() throws Exception {
        final Vdb vdb = createVdb();
        final KomodoObject kobject = new ObjectImpl(_repo, vdb.getAbsolutePath(), vdb.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, Vdb.class), is(instanceOf(Vdb.class)));
    }

    @Test
    public void shouldResolveExportable() throws Exception {
        final Vdb vdb = createVdb();
        final KomodoObject kobject = new ObjectImpl(_repo, vdb.getAbsolutePath(), vdb.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, Exportable.class), is(instanceOf(Exportable.class)));
    }

    @Test
    public void shouldResolveView() throws Exception {
        final Model model = createModel();
        final View view = model.addView(getTransaction(), "view");
        final KomodoObject kobject = new ObjectImpl(_repo, view.getAbsolutePath(), view.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, View.class), is(instanceOf(View.class)));
    }

    @Test
    public void shouldAdaptAllRelationalObjects() throws Exception {
        Vdb vdb = createVdb();
        Model model = vdb.addModel(getTransaction(), "testControlModel");
        Table table = model.addTable(getTransaction(), "testControlTable");
        KomodoObject o = null;

        // Null object should safely return null
        assertNull(wsMgr.resolve(getTransaction(), o, Vdb.class));

        // Vdb should always equal vdb
        o = vdb;
        Vdb vdb1 = wsMgr.resolve(getTransaction(), o, Vdb.class);
        assertNotNull(vdb1);

        // ObjectImpl referencing a vdb should be able to be adapted to a Vdb
        ObjectImpl vdbObj = new ObjectImpl(_repo, vdb.getAbsolutePath(), 0);
        Vdb vdb2 = wsMgr.resolve(getTransaction(), vdbObj, Vdb.class);
        assertNotNull(vdb2);

        // Model should always equal model
        o = model;
        Model model1 = wsMgr.resolve(getTransaction(), o, Model.class);
        assertNotNull(model1);

        // ObjectImpl referencing a model should be able to be adapted to a Model
        ObjectImpl modelObj = new ObjectImpl(_repo, model.getAbsolutePath(), 0);
        Model model2 = wsMgr.resolve(getTransaction(), modelObj, Model.class);
        assertNotNull(model2);

        // Table should always equal table
        o = table;
        Table table1 = wsMgr.resolve(getTransaction(), o, Table.class);
        assertNotNull(table1);

        // ObjectImpl referencing a table should be able to be adapted to a Table
        ObjectImpl tableObj = new ObjectImpl(_repo, table.getAbsolutePath(), 0);
        Table table2 = wsMgr.resolve(getTransaction(), tableObj, Table.class);
        assertNotNull(table2);
    }

}
