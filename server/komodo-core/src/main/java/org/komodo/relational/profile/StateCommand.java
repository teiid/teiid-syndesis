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
import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.profile.internal.StateCommandImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents the configuration of a view editor state command
 */
public interface StateCommand extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = StateCommand.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.STATE_COMMAND;

    /**
     * An empty array of view editor state commands.
     */
    StateCommand[] NO_STATE_COMMANDS = new StateCommand[0];

    /**
     * The resolver of a {@link StateCommand}.
     */
    TypeResolver<StateCommand> RESOLVER = new TypeResolver<StateCommand>() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class<StateCommandImpl> owningClass() {
            return StateCommandImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(transaction, kobject.getRepository(), kobject, KomodoLexicon.StateCommand.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public StateCommand resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == StateCommand.TYPE_ID) {
                return (StateCommand)kobject;
            }

            return new StateCommandImpl(transaction, kobject.getRepository(), kobject.getAbsolutePath());
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the id of the command
     * @throws KException
     *          if an error occurs
     */
    String getId(final UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param id the id of the command
     * @throws Exception
     *          if an error occurs
     */
    void setId(final UnitOfWork transaction, String id) throws Exception;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the arguments of the command
     * @throws KException
     *         if an error occurs
     */
    Map<String, String> getArguments(final UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param arguments
     *        the new map of arguments
     * @throws KException
     *         if an error occurs
     */
    void setArguments(final UnitOfWork transaction, final Map<String, String> arguments) throws KException;
}
