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
package org.komodo.rest.relational.json;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.profile.StateCommand;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate.RestStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;
import org.komodo.spi.KException;

public class ViewEditorStateSerializerTest extends AbstractSerializerTest {

    private String viewName = "myNewView";
    private String undoRedoId = "UpdateViewNameCommand";
    private String untitledName = "untitled";
    private String oldNameKey = "oldName";
    private String newNameKey = "newName";

    private String createViewEditorState() {
        String state = EMPTY_STRING +
            OPEN_BRACE + NEW_LINE +
                TAB + q(RestViewEditorState.BASE_URI) + colon() + q(MY_BASE_URI) + pnl(COMMA) +
                TAB + q(RestViewEditorState.ID_LABEL) + colon() + q(viewName) + pnl(COMMA) +
                TAB + q(RestViewEditorState.CONTENT_LABEL) + colon() + pnl(OPEN_SQUARE_BRACKET) +

                tab(2) + pnl(OPEN_BRACE) +
                    tab(3) + q(RestStateCommandAggregate.UNDO_LABEL) + colon() + pnl(OPEN_BRACE ) +
                        tab(4) + q(RestStateCommand.ID_LABEL) +
                                              colon() + q(undoRedoId) + pnl(COMMA) +
                        tab(4) + q(RestStateCommand.ARGS_LABEL) + colon() + pnl(OPEN_BRACE) +
                            tab(5) + q(oldNameKey) + colon() + q(viewName) + pnl(COMMA) +
                            tab(5) + q(newNameKey) + colon() + pnl(q(untitledName)) +
                        tab(4) + pnl(CLOSE_BRACE) +
                    tab(3) + CLOSE_BRACE + pnl(COMMA) +

                    tab(3) + q(RestStateCommandAggregate.REDO_LABEL) + colon() + pnl(OPEN_BRACE) +
                        tab(4) + q(RestStateCommand.ID_LABEL) +
                                              colon() + q(undoRedoId) + pnl(COMMA) +
                        tab(4) + q(RestStateCommand.ARGS_LABEL) + colon() + pnl(OPEN_BRACE) +
                            tab(5) + q(oldNameKey) + colon() + q(untitledName) + pnl(COMMA) +
                            tab(5) + q(newNameKey) + colon() + pnl(q(viewName)) +
                        tab(4) + pnl(CLOSE_BRACE) +
                    tab(3) + pnl(CLOSE_BRACE) +

                tab(2) + pnl(CLOSE_BRACE) +

                TAB + pnl(CLOSE_SQUARE_BRACKET) +    
            CLOSE_BRACE;

        return state;
    }

    @Test
    public void shouldImportJson() {
        String state = createViewEditorState();
        System.out.println(state);

        RestViewEditorState viewEditorState = KomodoJsonMarshaller.unmarshall(state, RestViewEditorState.class);
        assertEquals(viewName, viewEditorState.getId());

        RestStateCommandAggregate[] content = viewEditorState.getContent();
        assertNotNull(content);
        assertEquals(1, content.length);

        RestStateCommandAggregate cmdAgg = content[0];
        RestStateCommand undo = cmdAgg.getUndo();
        assertNotNull(undo);

        String undoId = "UpdateViewNameCommand";
        assertEquals(undoId, undo.getId());
        Map<String, String> undoArgs = undo.getArguments();
        assertNotNull(undoArgs);
        assertEquals(2, undoArgs.size());
        assertEquals(untitledName, undoArgs.get(newNameKey));
        assertEquals(viewName, undoArgs.get(oldNameKey));

        RestStateCommand redo = cmdAgg.getRedo();
        assertNotNull(redo);

        String redoId = undoId;
        assertEquals(redoId, redo.getId());
        Map<String, String> redoArgs = redo.getArguments();
        assertNotNull(redoArgs);
        assertEquals(2, redoArgs.size());
        assertEquals(untitledName, redoArgs.get(oldNameKey));
        assertEquals(viewName, redoArgs.get(newNameKey));
    }

    @Test
    public void shouldExportJson() throws KException {
        String newName = viewName;

        Map<String, String> undoArgs = new LinkedHashMap<>();
        undoArgs.put(oldNameKey, newName);
        undoArgs.put(newNameKey, untitledName);

        Map<String, String> redoArgs = new LinkedHashMap<>();
        redoArgs.put(oldNameKey, untitledName);
        redoArgs.put(newNameKey, newName);

        StateCommand undoCommand = mock(StateCommand.class);
        when(undoCommand.getId(transaction)).thenReturn(undoRedoId);
        when(undoCommand.getArguments(transaction)).thenReturn(undoArgs);

        StateCommand redoCommand = mock(StateCommand.class);
        when(redoCommand.getId(transaction)).thenReturn(undoRedoId);
        when(redoCommand.getArguments(transaction)).thenReturn(redoArgs);

        StateCommandAggregate command = mock(StateCommandAggregate.class);
        when(command.getName(transaction)).thenReturn(
                                                      KomodoLexicon.StateCommandAggregate.NAME_PREFIX + 0);
        when(command.getUndo(transaction)).thenReturn(undoCommand);
        when(command.getRedo(transaction)).thenReturn(redoCommand);

        StateCommandAggregate[] commands = { command };
        ViewEditorState state = mock(ViewEditorState.class);
        when(state.getName(transaction)).thenReturn(viewName);
        when(state.getCommands(transaction)).thenReturn(commands);

        RestViewEditorState restState = new RestViewEditorState(MY_BASE_URI, state, transaction);

        String expectedJson = createViewEditorState()
                                                    .replaceAll(NEW_LINE,  SPACE)
                                                    .replaceAll(TAB, SPACE)
                                                    .replaceAll(SPACE, EMPTY_STRING);

        String resultJson = KomodoJsonMarshaller.marshall(restState)
                                                    .replaceAll(NEW_LINE,  SPACE)
                                                    .replaceAll(TAB, SPACE)
                                                    .replaceAll(SPACE, EMPTY_STRING);

        assertEquals(expectedJson, resultJson);
    }
}
