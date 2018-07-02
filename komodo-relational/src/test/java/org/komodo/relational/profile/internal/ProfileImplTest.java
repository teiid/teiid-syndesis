/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.ViewEditorStateCommand;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.KomodoObject;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ProfileImplTest extends RelationalModelTest {

    private Profile profile;

    private Profile createProfile() throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
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

        ViewEditorStateCommand viewEditorStateCmd = RelationalModelFactory.createViewEditorStateCommand(
                                                                                                        getTransaction(), _repo,
                                                                                                        viewEditorState,
                                                                                                        undoId, undoArgs,
                                                                                                        redoId, redoArgs);

        ViewEditorStateCommand[] commands = viewEditorState.getCommands(getTransaction());
        assertEquals(1, commands.length);
        assertEquals(viewEditorStateCmd, commands[0]);

        assertEquals(KomodoLexicon.ViewEditorStateCommand.COMMAND_ID_PREFIX + 0, viewEditorStateCmd.getName(getTransaction()));
        assertEquals(undoId, viewEditorStateCmd.getUndoId(getTransaction()));
        assertEquals(undoArgs, viewEditorStateCmd.getUndoArguments(getTransaction()));
        assertEquals(redoId, viewEditorStateCmd.getRedoId(getTransaction()));
        assertEquals(redoArgs, viewEditorStateCmd.getRedoArguments(getTransaction()));
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
