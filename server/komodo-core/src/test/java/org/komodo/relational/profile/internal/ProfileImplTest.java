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
package org.komodo.relational.profile.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.StateCommand;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.workspace.WorkspaceManagerImpl;
import org.komodo.spi.repository.KomodoObject;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ProfileImplTest extends RelationalModelTest {

    private Profile profile;

    private Profile createProfile() throws Exception {
        final WorkspaceManagerImpl mgr = WorkspaceManagerImpl.getInstance(_repo, getTransaction());
        KomodoObject userProfileObj = _repo.komodoProfile(getTransaction());
        Profile profile = mgr.resolve(getTransaction(), userProfileObj, Profile.class);
        assertNotNull(profile);
        return profile;
    }

    @Before
    public void init() throws Exception {
        this.profile = createProfile();
        commit();
    }

    @Test
    public void shouldAddViewEditorState() throws Exception {
        String name = "myNewView";
        ViewEditorState viewEditorState = RelationalModelFactory.createViewEditorState(getTransaction(),
                                                                                                                                               _repo, profile, name);
        assertTrue(profile.hasChild(getTransaction(), name));
        assertEquals(name, viewEditorState.getName(getTransaction()));

        String undoId = "removeSourceCommand";
        String srcName = "myFabSource";
        Map<String, String> undoArgs = new HashMap<>();
        undoArgs.put("srcName", srcName);

        String redoId = "addSourceCommand";
        String srcPath = "connection=conn1/schema=public/table=customer";
        Map<String, String> redoArgs = new HashMap<>();
        redoArgs.put("srcName", srcName);
        redoArgs.put("srcPath", srcPath);

        StateCommandAggregate stateCmdAgg = viewEditorState.addCommand(getTransaction());
        StateCommand undoCommand = stateCmdAgg.setUndo(getTransaction(), undoId, undoArgs);
        StateCommand redoCommand = stateCmdAgg.setRedo(getTransaction(), redoId, redoArgs);

        StateCommandAggregate[] commands = viewEditorState.getCommands(getTransaction());
        assertEquals(1, commands.length);
        assertEquals(stateCmdAgg, commands[0]);
        assertEquals(KomodoLexicon.StateCommandAggregate.NAME_PREFIX + 0,
                                     stateCmdAgg.getName(getTransaction()));

        assertEquals(undoId, undoCommand.getId(getTransaction()));
        assertEquals(undoArgs, undoCommand.getArguments(getTransaction()));
        assertEquals(redoId, redoCommand.getId(getTransaction()));
        assertEquals(redoArgs, redoCommand.getArguments(getTransaction()));
    }

    @Test
    public void shouldGetViewEditorStates() throws Exception {
        final int numViews = 5;

        for ( int i = 0; i < numViews; ++i ) {
            String name = "myNewView" + i;
            profile.addViewEditorState( getTransaction(), name);
        }

        assertThat( this.profile.getViewEditorStates( getTransaction() ).length, is( numViews ) );

        KomodoObject[] children = profile.getChildren(getTransaction());
        assertEquals(numViews, children.length);
    }

    @Test
    public void shouldRemoveView() throws Exception {
        String name = "myNewView";
        this.profile.addViewEditorState( getTransaction(), name);
        this.profile.removeViewEditorState( getTransaction(), name );
        assertThat( this.profile.getViewEditorStates( getTransaction() ).length, is( 0 ) );
    }
}
