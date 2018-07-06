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
package org.komodo.rest.relational.response.vieweditorstate;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;
import javax.ws.rs.core.MediaType;
import org.komodo.relational.profile.StateCommand;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class RestStateCommandAggregate implements KRestEntity {

    /**
     * Label used for view editor state command undo phase
     */
    public static final String UNDO_LABEL = "undo";

    /**
     * Label used for view editor state command redo phase
     */
    public static final String REDO_LABEL = "redo";

    public static class RestStateCommand implements KRestEntity {

        /**
         * Label used for view editor state command id
         */
        public static final String ID_LABEL = "id";

        /**
         * Label used for view editor state command arguments
         */
        public static final String ARGS_LABEL = "args";

        private String id;

        private Map<String, String> args = new LinkedHashMap<>();

        /**
         * @return the id
         */
        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public void addArgument(String name, String value) {
            args.put(name, value);
        }

        public Map<String, String> getArguments() {
            return args;
        }

        @Override
        public boolean supports(MediaType mediaType) {
            return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
        }

        @Override
        public Object getXml() {
            throw new UnsupportedOperationException();
        }
    }

    private RestStateCommand undoCommand;

    private RestStateCommand redoCommand;

    /**
     * Constructor for use when deserializing
     */
    public RestStateCommandAggregate() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param stateCmdAgg the view editor state command aggregate
     *
     * @throws KException if error occurs
     */
    public RestStateCommandAggregate(URI baseUri, StateCommandAggregate stateCmdAgg, UnitOfWork transaction) throws KException {

        StateCommand undo = stateCmdAgg.getUndo(transaction);
        StateCommand redo = stateCmdAgg.getRedo(transaction);

        if (undo != null) {
            RestStateCommand undoStateCmd = getOrCreateUndoStateCommand();
            undoStateCmd.setId(undo.getId(transaction));
            Map<String, String> undoArgs = undo.getArguments(transaction);
            for (Map.Entry<String, String> arg : undoArgs.entrySet()) {
                undoStateCmd.addArgument(arg.getKey(), arg.getValue());
            }
        }

        if (redo != null) {
            RestStateCommand redoStateCmd = getOrCreateRedoStateCommand();
            redoStateCmd.setId(redo.getId(transaction));
            Map<String, String> redoArgs = redo.getArguments(transaction);
            for (Map.Entry<String, String> arg : redoArgs.entrySet()) {
                redoStateCmd.addArgument(arg.getKey(), arg.getValue());
            }
        }
    }

    private RestStateCommand getOrCreateUndoStateCommand() {
        RestStateCommand unit = getUndo();
        if (unit == null) {
            unit = new RestStateCommand();
            this.setUndo(unit);
        }

        return unit;
    }

    private RestStateCommand getOrCreateRedoStateCommand() {
        RestStateCommand unit = getRedo();
        if (unit == null) {
            unit = new RestStateCommand();
            this.setRedo(unit);
        }

        return unit;
    }

    public RestStateCommand getUndo() {
        return undoCommand;
    }

    public void setUndo(RestStateCommand undoCommand) {
        this.undoCommand = undoCommand;
    }

    public RestStateCommand getRedo() {
        return redoCommand;
    }

    public void setRedo(RestStateCommand redoCommand) {
        this.redoCommand = redoCommand;
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }
}
