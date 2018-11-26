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
