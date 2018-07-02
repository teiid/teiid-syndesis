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

import static org.komodo.rest.Messages.Error.INCOMPLETE_JSON;
import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Map.Entry;
import org.komodo.rest.Messages;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorStateCommand.RestViewEditorStateCmdUnit;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public class ViewEditorStateCommandSerializer extends TypeAdapter<RestViewEditorStateCommand> implements JsonConstants {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    public static class ViewEditorStateCmdUnitSerializer extends TypeAdapter<RestViewEditorStateCmdUnit> implements JsonConstants {

        private boolean isComplete(RestViewEditorStateCmdUnit entity) {
            return entity.getId() != null;
        }

        @Override
        public RestViewEditorStateCmdUnit read(JsonReader in) throws IOException {
            final RestViewEditorStateCmdUnit unit = new RestViewEditorStateCmdUnit();
            in.beginObject();

            while ( in.hasNext() ) {
                final String name = in.nextName();

                switch (name) {
                    case RestViewEditorStateCommand.ID_LABEL:
                        String id = in.nextString();
                        unit.setId(id);
                        break;
                    case RestViewEditorStateCommand.ARGS_LABEL:
                        Map<String, String> args = BUILDER.fromJson(in, STRING_MAP_TYPE);
                        for (Map.Entry<String, String> entry : args.entrySet()) {
                            unit.addArgument(entry.getKey(), entry.getValue());
                        }
                        break;
                    default:
                        throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
                }
            }

            in.endObject();

            if ( !isComplete(unit) ) {
                throw new IOException( Messages.getString( INCOMPLETE_JSON, RestVdbImport.class.getSimpleName() ) );
            }

            return unit;
        }

        @Override
        public void write(JsonWriter out, RestViewEditorStateCmdUnit value) throws IOException {

            out.beginObject();

            out.name(RestViewEditorStateCommand.ID_LABEL);
            out.value(value.getId());

            Map<String, String> args = value.getArguments();
            if (args.size() != 0) {
                out.name(RestViewEditorStateCommand.ARGS_LABEL);
                out.beginObject();

                for (Entry<String, String> arg : args.entrySet()) {
                    out.name(arg.getKey());
                    out.value(arg.getValue());
                }

                out.endObject();
            }

            out.endObject();
        }
    }

    private boolean isComplete(RestViewEditorStateCommand entity) {
        return entity.getUndoId() != null && entity.getRedoId() != null;
    }

    @Override
    public RestViewEditorStateCommand read(JsonReader in) throws IOException {
        final RestViewEditorStateCommand command = new RestViewEditorStateCommand();

        in.beginObject();

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestViewEditorStateCommand.UNDO_LABEL.equals(name)) {
                RestViewEditorStateCmdUnit unit = BUILDER.fromJson(in, RestViewEditorStateCmdUnit.class);
                command.setUndoUnit(unit);
            } else if (RestViewEditorStateCommand.REDO_LABEL.equals(name)) {
                RestViewEditorStateCmdUnit unit = BUILDER.fromJson(in, RestViewEditorStateCmdUnit.class);
                command.setRedoUnit(unit);
            }
        }

        if (!isComplete( command)) {
            throw new IOException(Messages.getString( INCOMPLETE_JSON, getClass().getSimpleName()));
        }

        in.endObject();

        return command;
    }

    @Override
    public void write(JsonWriter out, RestViewEditorStateCommand value) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestViewEditorStateCommand.class.getSimpleName() ) );
        }

        out.beginObject();

        out.name(RestViewEditorStateCommand.UNDO_LABEL);
        BUILDER.toJson(value.getUndoUnit(), RestViewEditorStateCmdUnit.class, out);

        out.name(RestViewEditorStateCommand.REDO_LABEL);
        BUILDER.toJson(value.getRedoUnit(), RestViewEditorStateCmdUnit.class, out);

        out.endObject();
    }
}
