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
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.profile.ViewEditorStateCommand;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * An implementation of a view editor state object.
 */
public class ViewEditorStateCommandImpl extends RelationalChildRestrictedObject implements ViewEditorStateCommand {

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
        return ViewEditorStateCommand.IDENTIFIER;
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
    public String getUndoId(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getUndoId",
                                                                     KomodoLexicon.ViewEditorStateCommand.UNDO_ID);
    }

    @Override
    public void setUndoId(UnitOfWork transaction, String id) throws Exception {
        setObjectProperty(transaction, "setUndoId",
                                                                      KomodoLexicon.ViewEditorStateCommand.UNDO_ID, id);
    }

    @Override
    public Map<String, String> getUndoArguments(UnitOfWork transaction) throws KException {
        Map<String, String> args = new HashMap<>();

        String[] propertyNames = getPropertyNames(transaction);
        if (propertyNames == null)
            return args;

        for (String propertyName : propertyNames) {
            if (! propertyName.startsWith(KomodoLexicon.ViewEditorStateCommand.UNDO_ARGS_PREFIX))
                continue;

            String value = getObjectProperty(transaction, PropertyValueType.STRING, "getUndoArgument", propertyName);
            String name = propertyName.replace(
                                                                   KomodoLexicon.ViewEditorStateCommand.UNDO_ARGS_PREFIX, EMPTY_STRING);
            args.put(name,  value);
        }

        return args;
    }

    @Override
    public void setUndoArguments(UnitOfWork transaction, Map<String, String> arguments) throws KException {
        if (arguments == null)
            arguments = new HashMap<>();

        for (Map.Entry<String, String> entry : arguments.entrySet()) {
            String name = KomodoLexicon.ViewEditorStateCommand.UNDO_ARGS_PREFIX + entry.getKey();
            String value = entry.getValue();
            setObjectProperty(transaction, "setUndoArgument", name, value); //$NON-NLS-1$
        }
    }

    @Override
    public String getRedoId(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getRedoId",
                                                                 KomodoLexicon.ViewEditorStateCommand.REDO_ID);
    }

    @Override
    public void setRedoId(UnitOfWork transaction, String id) throws Exception {
        setObjectProperty(transaction, "setRedoId",
                                                                  KomodoLexicon.ViewEditorStateCommand.REDO_ID, id);
    }

    @Override
    public Map<String, String> getRedoArguments(UnitOfWork transaction) throws KException {
        Map<String, String> args = new HashMap<>();

        String[] propertyNames = getPropertyNames(transaction);
        if (propertyNames == null)
            return args;

        for (String propertyName : propertyNames) {
            if (! propertyName.startsWith(KomodoLexicon.ViewEditorStateCommand.REDO_ARGS_PREFIX))
                continue;

            String value = getObjectProperty(transaction, PropertyValueType.STRING, "getRedoArgument", propertyName);
            String name = propertyName.replace(
                                                                   KomodoLexicon.ViewEditorStateCommand.REDO_ARGS_PREFIX, EMPTY_STRING);
            args.put(name,  value);
        }

        return args;
    }

    @Override
    public void setRedoArguments(UnitOfWork transaction, Map<String, String> arguments) throws KException {
        if (arguments == null)
            arguments = new HashMap<>();

        for (Map.Entry<String, String> entry : arguments.entrySet()) {
            String name = KomodoLexicon.ViewEditorStateCommand.REDO_ARGS_PREFIX + entry.getKey();
            String value = entry.getValue();
            setObjectProperty(transaction, "setUndoArgument", name, value); //$NON-NLS-1$
        }
    }
}
