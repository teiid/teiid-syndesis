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
package org.komodo.relational.profile.internal;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.ViewEditorStateCommand;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a view editor state object.
 */
public class ViewEditorStateCommandImpl extends RelationalChildRestrictedObject implements ViewEditorStateCommand {

    private static final String ARGS_PREFIX = "ARGS_";

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
    public ViewEditorStateCommandImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return ViewEditorState.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Profile getParent(final UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state must be NOT_STARTED"); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent(transaction);
        final Profile result = Profile.RESOLVER.resolve(transaction, grouping.getParent(transaction));
        return result;
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
    public Map<String, String> getArguments(UnitOfWork transaction) throws KException {
        Map<String, String> args = new HashMap<>();

        String[] propertyNames = getPropertyNames(transaction);
        if (propertyNames == null)
            return args;

        for (String propertyName : propertyNames) {
            if (! propertyName.startsWith(ARGS_PREFIX))
                continue;

            String value = getObjectProperty(transaction, PropertyValueType.STRING, "getArgument", propertyName);
            String name = propertyName.replace(ARGS_PREFIX, EMPTY_STRING);
            args.put(name,  value);
        }

        return args;
    }

    @Override
    public void setArguments(UnitOfWork transaction, Map<String, String> arguments) throws KException {
        if (arguments == null)
            arguments = new HashMap<>();

        for (Map.Entry<String, String> entry : arguments.entrySet()) {
            String name = ARGS_PREFIX + entry.getKey();
            String value = entry.getValue();
            setObjectProperty(transaction, "setArgument", name, value); //$NON-NLS-1$
        }
    }
}
