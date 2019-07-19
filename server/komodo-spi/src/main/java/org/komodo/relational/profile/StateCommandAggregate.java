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
package org.komodo.relational.profile;

import java.util.Map;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents the configuration of a view editor state command
 */
public interface StateCommandAggregate extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = StateCommandAggregate.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.STATE_COMMAND_AGGREGATE;

    /**
     * An empty array of view editor state commands.
     */
    StateCommandAggregate[] NO_STATE_COMMAND_AGGREGATES = new StateCommandAggregate[0];


    /**
     * @return the undo command
     * @throws KException
     *          if an error occurs
     */
    StateCommand getUndo() throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param commandId the id of the command
     * @param arguments the map of the arguments
     * @return the new undo command
     * @throws Exception
     *          if an error occurs
     */
    StateCommand setUndo( String commandId, Map<String, String> arguments) throws Exception;

    /**
     * @return the redo command
     * @throws KException
     *          if an error occurs
     */
    StateCommand getRedo() throws KException;

    /**
     * @param commandId the id of the command
     * @param arguments the map of the arguments
     * @return the new redo command
     * @throws Exception
     *          if an error occurs
     */
    StateCommand setRedo( String commandId, Map<String, String> arguments) throws Exception;

}
