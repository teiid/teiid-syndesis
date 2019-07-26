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
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.StateCommand;
import org.komodo.relational.dataservice.StateCommandAggregate;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.profile.internal.StateCommandAggregateImpl;
import org.komodo.relational.profile.internal.ViewEditorStateImpl;
import org.komodo.spi.StringConstants;
import org.komodo.utils.FileUtils;

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
        final DataserviceImpl dataservice = this.wsMgr.createDataservice( getTransaction(), null, "service" );
        assertThat( dataservice, is( notNullValue() ) );
        assertThat( _repo.getFromWorkspace( getTransaction(), dataservice.getAbsolutePath() ), is( ( KomodoObject )dataservice ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowEmptyTypeWhenFindingObjects() throws Exception {
        this.wsMgr.findByType( getTransaction(), StringConstants.EMPTY_STRING, "/my/path", null, false );
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowNullTypeWhenFindingObjects() throws Exception {
        this.wsMgr.findByType( getTransaction(), null, "/my/path", null, false );
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
    public void shouldResolveDataservice() throws Exception {
        final DataserviceImpl service = this.wsMgr.createDataservice(getTransaction(), null, "service");
        final KomodoObject kobject = new ObjectImpl(getTransaction(), _repo, service.getAbsolutePath(), service.getIndex());
        assertThat(this.wsMgr.resolve(getTransaction(), kobject, Dataservice.class), is(instanceOf(Dataservice.class)));
    }

    @Test
    public void shouldFindDataservices() throws Exception {
    	Dataservice ds = wsMgr.createDataservice("service");
    	
    	commit();
    	
    	Dataservice[] services = wsMgr.findDataservices(null);
    	
    	assertEquals(1, services.length);
    	assertEquals(ds.getName(), services[0].getName());
    }
    
    @Test
    public void shouldSaveSchema() throws Exception {
    	assertNull(wsMgr.findSchema("a"));
    	
    	wsMgr.saveSchema("a", "b");
    	
    	assertEquals("b", wsMgr.findSchema("a"));
    	
    	assertTrue(wsMgr.deleteSchema("a"));
    	
    	assertNull(wsMgr.findSchema("a"));
    }
    
    @Test
    public void shouldAddViewEditorState() throws Exception {
        String name = "myNewView";
        ViewEditorStateImpl viewEditorState = RelationalModelFactory.createViewEditorState(getTransaction(),
                                                                                                                                               _repo, wsMgr, name);
        assertEquals(name, viewEditorState.getName());

        String undoId = "removeSourceCommand";
        String srcName = "myFabSource";
        Map<String, String> undoArgs = new HashMap<>();
        undoArgs.put("srcName", srcName);

        String redoId = "addSourceCommand";
        String srcPath = "connection=conn1/schema=public/table=customer";
        Map<String, String> redoArgs = new HashMap<>();
        redoArgs.put("srcName", srcName);
        redoArgs.put("srcPath", srcPath);

        StateCommandAggregateImpl stateCmdAgg = viewEditorState.addCommand();
        StateCommand undoCommand = stateCmdAgg.setUndo(undoId, undoArgs);
        StateCommand redoCommand = stateCmdAgg.setRedo(redoId, redoArgs);

        StateCommandAggregate[] commands = viewEditorState.getCommands();
        assertEquals(1, commands.length);
        assertEquals(stateCmdAgg, commands[0]);
        assertEquals(KomodoLexicon.StateCommandAggregate.NAME_PREFIX + 0,
                                     stateCmdAgg.getName());

        assertEquals(undoId, undoCommand.getId());
        assertEquals(undoArgs, undoCommand.getArguments());
        assertEquals(redoId, redoCommand.getId());
        assertEquals(redoArgs, redoCommand.getArguments());
    }

    @Test
    public void shouldGetViewEditorStates() throws Exception {
        final int numViews = 5;

        for ( int i = 0; i < numViews; ++i ) {
            String name = "myNewView" + i;
            wsMgr.addViewEditorState( name);
        }

        assertThat( this.wsMgr.getViewEditorStates( ).length, is( numViews ) );
    }

    @Test
    public void shouldRemoveView() throws Exception {
        String name = "myNewView";
        this.wsMgr.addViewEditorState( name);
        this.wsMgr.removeViewEditorState( name );
        assertThat( this.wsMgr.getViewEditorStates( ).length, is( 0 ) );
    }

}
