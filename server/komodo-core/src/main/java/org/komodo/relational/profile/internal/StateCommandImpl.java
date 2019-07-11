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

import java.util.HashMap;
import java.util.Map;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.profile.StateCommand;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * An implementation of a view editor state object.
 */
public class StateCommandImpl extends RelationalChildRestrictedObject implements StateCommand {
	
    /**
     * The resolver of a {@link StateCommand}.
     */
    public static final TypeResolver<StateCommand> RESOLVER = new TypeResolver<StateCommand>() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class<StateCommandImpl> owningClass() {
            return StateCommandImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(transaction, kobject.getRepository(), kobject, KomodoLexicon.StateCommand.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
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
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param path
     *        the path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public StateCommandImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return StateCommand.IDENTIFIER;
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

    @Override
    public String getId(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getId",
                                                                     KomodoLexicon.StateCommand.ID);
    }

    @Override
    public void setId(UnitOfWork transaction, String id) throws Exception {
        setObjectProperty(transaction, "setId",
                                                                      KomodoLexicon.StateCommand.ID, id);
    }

    @Override
    public Map<String, String> getArguments(UnitOfWork transaction) throws KException {
        Map<String, String> args = new HashMap<>();

        String[] propertyNames = getPropertyNames(transaction);
        if (propertyNames == null)
            return args;

        for (String propertyName : propertyNames) {
            if (! propertyName.startsWith(KomodoLexicon.StateCommand.ARGS_PREFIX))
                continue;

            String value = getObjectProperty(transaction, PropertyValueType.STRING, "getArgument", propertyName);
            String name = propertyName.replace(
                                                                   KomodoLexicon.StateCommand.ARGS_PREFIX, EMPTY_STRING);
            args.put(name,  value);
        }

        return args;
    }

    @Override
    public void setArguments(UnitOfWork transaction, Map<String, String> arguments) throws KException {
        if (arguments == null)
            arguments = new HashMap<>();

        for (Map.Entry<String, String> entry : arguments.entrySet()) {
            String name = KomodoLexicon.StateCommand.ARGS_PREFIX + entry.getKey();
            String value = entry.getValue();
            setObjectProperty(transaction, "setArgument", name, value); //$NON-NLS-1$
        }
    }
}
