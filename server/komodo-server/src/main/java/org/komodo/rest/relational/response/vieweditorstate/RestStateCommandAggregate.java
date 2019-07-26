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
package org.komodo.rest.relational.response.vieweditorstate;

import java.net.URI;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.ws.rs.core.MediaType;

import org.komodo.relational.dataservice.StateCommand;
import org.komodo.relational.dataservice.StateCommandAggregate;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.KException;

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
     * @throws KException if error occurs
     */
    public RestStateCommandAggregate(URI baseUri, StateCommandAggregate stateCmdAgg) throws KException {

        StateCommand undo = stateCmdAgg.getUndo();
        StateCommand redo = stateCmdAgg.getRedo();

        if (undo != null) {
            RestStateCommand undoStateCmd = getOrCreateUndoStateCommand();
            undoStateCmd.setId(undo.getId());
            Map<String, String> undoArgs = undo.getArguments();
            for (Map.Entry<String, String> arg : undoArgs.entrySet()) {
                undoStateCmd.addArgument(arg.getKey(), arg.getValue());
            }
        }

        if (redo != null) {
            RestStateCommand redoStateCmd = getOrCreateRedoStateCommand();
            redoStateCmd.setId(redo.getId());
            Map<String, String> redoArgs = redo.getArguments();
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
