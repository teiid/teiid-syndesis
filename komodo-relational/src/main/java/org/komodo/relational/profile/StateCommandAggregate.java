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
package org.komodo.relational.profile;

import java.util.Map;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.profile.internal.StateCommandAggregateImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

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
     * The resolver of a {@link StateCommandAggregate}.
     */
    TypeResolver<StateCommandAggregate> RESOLVER = new TypeResolver<StateCommandAggregate>() {

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
        public Class<StateCommandAggregateImpl> owningClass() {
            return StateCommandAggregateImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(transaction, kobject.getRepository(), kobject, KomodoLexicon.StateCommandAggregate.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public StateCommandAggregate resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == StateCommandAggregate.TYPE_ID) {
                return (StateCommandAggregate)kobject;
            }

            return new StateCommandAggregateImpl(transaction, kobject.getRepository(), kobject.getAbsolutePath());
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the undo command
     * @throws KException
     *          if an error occurs
     */
    StateCommand getUndo(final UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param commandId the id of the command
     * @param arguments the map of the arguments
     * @return the new undo command
     * @throws Exception
     *          if an error occurs
     */
    StateCommand setUndo(final UnitOfWork transaction, String commandId, Map<String, String> arguments) throws Exception;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the redo command
     * @throws KException
     *          if an error occurs
     */
    StateCommand getRedo(final UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param commandId the id of the command
     * @param arguments the map of the arguments
     * @return the new redo command
     * @throws Exception
     *          if an error occurs
     */
    StateCommand setRedo(final UnitOfWork transaction, String commandId, Map<String, String> arguments) throws Exception;

}
