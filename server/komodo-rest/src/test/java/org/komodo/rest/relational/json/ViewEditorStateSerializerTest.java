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
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.ViewEditorStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorStateCommand;
import org.komodo.rest.service.ServiceTestUtilities;
import org.komodo.spi.KException;
import org.komodo.test.utils.TestUtilities;

public class ViewEditorStateSerializerTest extends AbstractSerializerTest {

    private String createViewEditorState(String id) {
        String state = EMPTY_STRING +
            OPEN_BRACE + NEW_LINE +
                TestUtilities.space(2) + "\"keng__baseUri\"" + COLON + SPACE + SPEECH_MARK + MY_BASE_URI + SPEECH_MARK + COMMA + NEW_LINE +
                TestUtilities.space(2) + "\"id\"" + COLON + SPACE + SPEECH_MARK + id + SPEECH_MARK + COMMA + NEW_LINE +
                TestUtilities.space(2) + "\"content\"" + COLON + SPACE +
                    ServiceTestUtilities.viewEditorCommandContent(id, MY_BASE_URI) + NEW_LINE +
            CLOSE_BRACE;

        return state;
    }

    @Test
    public void shouldImportJson() {
        String id = "myNewView";
        String state = createViewEditorState(id);
        System.out.println(state);

        RestViewEditorState viewEditorState = KomodoJsonMarshaller.unmarshall(state, RestViewEditorState.class);
        assertEquals(id, viewEditorState.getId());

        RestViewEditorStateCommand[] content = viewEditorState.getContent();
        assertNotNull(content);
        assertEquals(1, content.length);
        
        RestViewEditorStateCommand cmd = content[0];
        assertEquals("set-view-name", cmd.getId());
        Map<String, String> args = cmd.getArguments();
        assertNotNull(args);
        assertEquals(2, args.size());

        assertEquals("untitled", args.get("oldName"));
        assertEquals(id, args.get("newName"));
    }

    @Test
    public void shouldExportJson() throws KException {
        String id = "myNewView";
        String stateJson = createViewEditorState(id);

        String commandId = "set-view-name";
        Map<String, String> args = new HashMap<>();
        args.put("oldName", "untitled");
        args.put("newName", id);

        ViewEditorStateCommand command = mock(ViewEditorStateCommand.class);
        when(command.getName(transaction)).thenReturn(commandId);
        when(command.getArguments(transaction)).thenReturn(args);
        ViewEditorStateCommand[] commands = { command };

        ViewEditorState state = mock(ViewEditorState.class);
        when(state.getName(transaction)).thenReturn(id);
        when(state.getCommands(transaction)).thenReturn(commands);

        RestViewEditorState restState = new RestViewEditorState(MY_BASE_URI, state, transaction);
        String json = KomodoJsonMarshaller.marshall(restState);
        
        stateJson = stateJson.replace(NEW_LINE,  SPACE).replaceAll(TAB, SPACE).replace(SPACE, EMPTY_STRING);
        json = json.replace(NEW_LINE,  SPACE).replaceAll(TAB, SPACE).replace(SPACE, EMPTY_STRING);
        assertEquals(stateJson, json);
    }
}
