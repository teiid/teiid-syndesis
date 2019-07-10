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
package org.komodo.relational.profile.internal;

import java.util.Map;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.profile.StateCommand;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a view editor state object.
 */
public class StateCommandAggregateImpl extends RelationalObjectImpl implements StateCommandAggregate {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param path
     *        the path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public StateCommandAggregateImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return StateCommandAggregate.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    private StateCommand getStateCommand(UnitOfWork transaction, String stateCmdType) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
    
        KomodoObject stateCmdObject = null;

        if (hasChild(transaction, stateCmdType)) {
            stateCmdObject = getChild(transaction, stateCmdType,
                                                    KomodoLexicon.StateCommand.NODE_TYPE);
        }
    
        if (stateCmdObject == null)
            return null;
    
        return StateCommand.RESOLVER.resolve(transaction, stateCmdObject);
    }

    private StateCommand setStateCommand(UnitOfWork transaction, String stateCmdType,
                                                                 String commandId, Map<String, String> arguments) throws KException {
        return RelationalModelFactory.createStateCommand(transaction, getRepository(), this,
                                                                                                                 stateCmdType, commandId, arguments);
    }

    @Override
    public StateCommand getUndo(UnitOfWork transaction) throws KException {
        return getStateCommand(transaction, KomodoLexicon.StateCommandAggregate.UNDO);
    }

    @Override
    public StateCommand setUndo(UnitOfWork transaction, String commandId, Map<String, String> arguments) throws Exception {
        return setStateCommand(transaction, KomodoLexicon.StateCommandAggregate.UNDO,
                                                               commandId, arguments);
    }

    @Override
    public StateCommand getRedo(UnitOfWork transaction) throws KException {
        return getStateCommand(transaction, KomodoLexicon.StateCommandAggregate.REDO);
    }

    @Override
    public StateCommand setRedo(UnitOfWork transaction, String commandId, Map<String, String> arguments) throws Exception {
        return setStateCommand(transaction, KomodoLexicon.StateCommandAggregate.REDO,
                                                               commandId, arguments);
    }
}
