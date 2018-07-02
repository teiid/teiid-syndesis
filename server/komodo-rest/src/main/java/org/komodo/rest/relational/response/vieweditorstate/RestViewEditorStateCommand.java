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
import org.komodo.relational.profile.ViewEditorStateCommand;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class RestViewEditorStateCommand implements KRestEntity {

    /**
     * Label used for view editor state command id
     */
    public static final String ID_LABEL = "id";

    /**
     * Label used for view editor state command undo phase
     */
    public static final String UNDO_LABEL = "undo";

    /**
     * Label used for view editor state command redo phase
     */
    public static final String REDO_LABEL = "redo";

    /**
     * Label used for view editor state command arguments
     */
    public static final String ARGS_LABEL = "args";

    public static class RestViewEditorStateCmdUnit implements KRestEntity {

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

    private RestViewEditorStateCmdUnit undoUnit;

    private RestViewEditorStateCmdUnit redoUnit;

    /**
     * Constructor for use when deserializing
     */
    public RestViewEditorStateCommand() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param viewEditorStateCommand the view editor state command
     *
     * @throws KException if error occurs
     */
    public RestViewEditorStateCommand(URI baseUri, ViewEditorStateCommand viewEditorStateCmd, UnitOfWork transaction) throws KException {

        setUndoId(viewEditorStateCmd.getUndoId(transaction));
        Map<String, String> undoArgs = viewEditorStateCmd.getUndoArguments(transaction);
        for (Map.Entry<String, String> arg : undoArgs.entrySet()) {
            addUndoArgument(arg.getKey(), arg.getValue());
        }

        setRedoId(viewEditorStateCmd.getRedoId(transaction));
        Map<String, String> redoArgs = viewEditorStateCmd.getRedoArguments(transaction);
        for (Map.Entry<String, String> arg : redoArgs.entrySet()) {
            addRedoArgument(arg.getKey(), arg.getValue());
        }
    }

    private RestViewEditorStateCmdUnit getOrCreateUndoUnit() {
        RestViewEditorStateCmdUnit unit = getUndoUnit();
        if (unit == null) {
            unit = new RestViewEditorStateCmdUnit();
            this.setUndoUnit(unit);
        }

        return unit;
    }

    private RestViewEditorStateCmdUnit getOrCreateRedoUnit() {
        RestViewEditorStateCmdUnit unit = getRedoUnit();
        if (unit == null) {
            unit = new RestViewEditorStateCmdUnit();
            this.setRedoUnit(unit);
        }

        return unit;
    }

    /**
     * @return the undo id
     */
    public String getUndoId() {
        return undoUnit != null ? undoUnit.getId() : null;
    }

    public void setUndoId(String id) {
        RestViewEditorStateCmdUnit undoUnit = getOrCreateUndoUnit();
        undoUnit.setId(id);
    }

    /**
     * @return the redo id
     */
    public String getRedoId() {
        return redoUnit != null ? redoUnit.getId() : null;
    }

    public void setRedoId(String id) {
        RestViewEditorStateCmdUnit redoUnit = getOrCreateRedoUnit();
        redoUnit.setId(id);
    }

    public RestViewEditorStateCmdUnit getUndoUnit() {
        return undoUnit;
    }

    public void setUndoUnit(RestViewEditorStateCmdUnit unit) {
        this.undoUnit = unit;
    }

    public Map<String, String> getUndoArguments() {
        RestViewEditorStateCmdUnit unit = getOrCreateUndoUnit();
        return unit.getArguments();
    }

    public void addUndoArgument(String name, String value) {
        RestViewEditorStateCmdUnit unit = getOrCreateUndoUnit();
        unit.addArgument(name, value);
    }

    public RestViewEditorStateCmdUnit getRedoUnit() {
        return redoUnit;
    }

    public void setRedoUnit(RestViewEditorStateCmdUnit unit) {
        this.redoUnit = unit;
    }

    public Map<String, String> getRedoArguments() {
        RestViewEditorStateCmdUnit unit = getOrCreateRedoUnit();
        return unit.getArguments();
    }

    public void addRedoArgument(String name, String value) {
        RestViewEditorStateCmdUnit unit = getOrCreateRedoUnit();
        unit.addArgument(name, value);
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
