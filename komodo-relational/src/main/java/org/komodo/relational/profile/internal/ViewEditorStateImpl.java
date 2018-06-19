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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.ViewEditorStateCommand;
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
public class ViewEditorStateImpl extends RelationalObjectImpl implements ViewEditorState {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { ViewEditorStateCommand.IDENTIFIER };

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
    public ViewEditorStateImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public ViewEditorStateCommand addCommand( final UnitOfWork transaction,
                                  final String commandId, Map<String, String> arguments ) throws KException {
        return RelationalModelFactory.createViewEditorStateCommand(transaction, getRepository(), this, commandId, arguments);
    }

    @Override
    public ViewEditorStateCommand[] getCommands(UnitOfWork transaction) throws KException {
        KomodoObject[] commands = getChildrenOfType(transaction, KomodoLexicon.ViewEditorStateCommand.NODE_TYPE);
        if (commands == null)
            return ViewEditorStateCommand.NO_VIEW_EDITOR_STATE_COMMANDS;

        List<ViewEditorStateCommand> cmdList = new ArrayList<>();
        for (KomodoObject cmd : commands) {
            cmdList.add(new ViewEditorStateCommandImpl(transaction, getRepository(), cmd.getAbsolutePath()));
        }

        return cmdList.toArray(new ViewEditorStateCommand[0]);
    }
}
