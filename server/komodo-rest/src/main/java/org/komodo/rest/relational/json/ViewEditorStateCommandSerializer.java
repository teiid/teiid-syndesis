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
import java.util.Map;
import java.util.Map.Entry;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorStateCommand;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public class ViewEditorStateCommandSerializer extends AbstractEntitySerializer<RestViewEditorStateCommand> {

    @Override
    protected RestViewEditorStateCommand createEntity() {
        return new RestViewEditorStateCommand();
    }

    @Override
    protected boolean isComplete(RestViewEditorStateCommand entity) {
        return entity.getId() != null;
    }

    @Override
    protected String readExtension(String name, RestViewEditorStateCommand command, JsonReader in) {
        if (RestViewEditorStateCommand.ARGS_LABEL.equals(name)) {
            Map<String, String> args = BUILDER.fromJson(in, STRING_MAP_TYPE);
            for (Map.Entry<String, String> entry : args.entrySet()) {
                command.addArgument(entry.getKey(), entry.getValue());
            }
            return Integer.toString(args.size());
        }

        return null; // not processed
    }

    @Override
    protected void writeExtensions(JsonWriter out, RestViewEditorStateCommand command) throws IOException {
        Map<String, String> args = command.getArguments();

        if (args.size() != 0) {
            out.name(RestViewEditorStateCommand.ARGS_LABEL);
            out.beginObject();
            
            for (Entry<String, String> arg : args.entrySet()) {
                out.name(arg.getKey());
                out.value(arg.getValue());
            }

            out.endObject();
        }
    }
}
