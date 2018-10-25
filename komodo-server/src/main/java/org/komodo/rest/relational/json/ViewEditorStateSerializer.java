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

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;

import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public class ViewEditorStateSerializer extends AbstractEntitySerializer<RestViewEditorState> {

    @Override
    protected RestViewEditorState createEntity() {
        return new RestViewEditorState();
    }

    @Override
    protected boolean isComplete(RestViewEditorState entity) {
        return entity.getId() != null;
    }

    @Override
    protected String readExtension(String name, RestViewEditorState state, JsonReader in) {
    	if( RestViewEditorState.VIEW_DEFINITION_LABEL.equals(name) ) {
    		RestViewDefinition viewDef = BUILDER.fromJson(in, RestViewDefinition.class);
            state.setViewDefinition(viewDef);
    		return name;
    	} else if (RestViewEditorState.CONTENT_LABEL.equals(name)) {
            RestStateCommandAggregate[] commands = BUILDER.fromJson(in, RestStateCommandAggregate[].class);
            state.setCommands(commands);
            return Integer.toString(commands.length);
        }

        return null; // not processed
    }

    @Override
    protected void writeExtensions(JsonWriter out, RestViewEditorState state) throws IOException {
        RestViewDefinition viewDef = state.getViewDefinition();
    	
        if( viewDef != null ) {
        	out.name(RestViewEditorState.VIEW_DEFINITION_LABEL);
    		BUILDER.getAdapter( RestViewDefinition.class ).write( out, state.getViewDefinition());
        }
    	
    	RestStateCommandAggregate[] commands = state.getCommands();

        if (commands.length != 0) {
            out.name(RestViewEditorState.CONTENT_LABEL);
            out.beginArray();
            
            for (RestStateCommandAggregate command : commands) {
                BUILDER.getAdapter(RestStateCommandAggregate.class).write(out, command);
            }
            
            out.endArray();
        }
    }
}
