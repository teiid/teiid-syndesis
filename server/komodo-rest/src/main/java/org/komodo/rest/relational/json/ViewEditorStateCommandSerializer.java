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
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate.RestStateCommand;
import com.google.gson.TypeAdapter;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public class ViewEditorStateCommandSerializer extends TypeAdapter<RestStateCommandAggregate> implements JsonConstants {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    public static class ViewEditorStateCmdUnitSerializer extends TypeAdapter<RestStateCommand> implements JsonConstants {

        private boolean isComplete(RestStateCommand entity) {
            return entity.getId() != null;
        }

        @Override
        public RestStateCommand read(JsonReader in) throws IOException {
            final RestStateCommand unit = new RestStateCommand();
            in.beginObject();

            while ( in.hasNext() ) {
                final String name = in.nextName();

                switch (name) {
                    case RestStateCommand.ID_LABEL:
                        String id = in.nextString();
                        unit.setId(id);
                        break;
                    case RestStateCommand.ARGS_LABEL:
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
        public void write(JsonWriter out, RestStateCommand value) throws IOException {

            out.beginObject();

            out.name(RestStateCommand.ID_LABEL);
            out.value(value.getId());

            Map<String, String> args = value.getArguments();
            if (args.size() != 0) {
                out.name(RestStateCommand.ARGS_LABEL);
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

    private boolean isComplete(RestStateCommandAggregate agg) {
        RestStateCommand undo = agg.getUndo();
        RestStateCommand redo = agg.getRedo();
        if (undo == null || redo == null)
            return false;

        if (undo.getId() == null || redo.getId() == null)
            return false;

        return true;
    }

    @Override
    public RestStateCommandAggregate read(JsonReader in) throws IOException {
        final RestStateCommandAggregate command = new RestStateCommandAggregate();

        in.beginObject();

        while (in.hasNext()) {
            final String name = in.nextName();

            if (RestStateCommandAggregate.UNDO_LABEL.equals(name)) {
                RestStateCommand unit = BUILDER.fromJson(in, RestStateCommand.class);
                command.setUndo(unit);
            } else if (RestStateCommandAggregate.REDO_LABEL.equals(name)) {
                RestStateCommand unit = BUILDER.fromJson(in, RestStateCommand.class);
                command.setRedo(unit);
            }
        }

        if (!isComplete( command)) {
            throw new IOException(Messages.getString( INCOMPLETE_JSON, getClass().getSimpleName()));
        }

        in.endObject();

        return command;
    }

    @Override
    public void write(JsonWriter out, RestStateCommandAggregate value) throws IOException {
        if ( !isComplete( value ) ) {
            throw new IOException( Messages.getString( INCOMPLETE_JSON, RestStateCommandAggregate.class.getSimpleName() ) );
        }

        out.beginObject();

        out.name(RestStateCommandAggregate.UNDO_LABEL);
        BUILDER.toJson(value.getUndo(), RestStateCommand.class, out);

        out.name(RestStateCommandAggregate.REDO_LABEL);
        BUILDER.toJson(value.getRedo(), RestStateCommand.class, out);

        out.endObject();
    }
}
