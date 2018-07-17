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

import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.profile.GitRepository;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of an user profile.
 */
public class ProfileImpl extends RelationalObjectImpl implements Profile {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { GitRepository.IDENTIFIER, ViewEditorState.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param path
     *        the path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a model
     */
    public ProfileImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Profile.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        if ( KomodoLexicon.Profile.VIEW_EDITOR_STATES.equals( name ) ) {
            return false; // use hasRawChild
        }

        if ( KomodoLexicon.Profile.GIT_REPOSITORIES.equals( name ) ) {
            return false; // use hasRawChild
        }

        return ( super.hasChild( transaction, name ) ||
                            ( getGitRepositories( transaction, name ).length != 0 ) ) ||
                            ( getViewEditorStates( transaction, name ).length != 0 );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        final KomodoObject[] gitRepositories = getGitRepositories(transaction, namePatterns);
        final KomodoObject[] viewEditorStates = getViewEditorStates(transaction, namePatterns);

        final KomodoObject[] result = new KomodoObject[gitRepositories.length + viewEditorStates.length];
        System.arraycopy(gitRepositories, 0, result, 0, gitRepositories.length);
        System.arraycopy(viewEditorStates, 0, result, gitRepositories.length, viewEditorStates.length);

        return result;
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

    private KomodoObject getGitRepositoriesGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, KomodoLexicon.Profile.GIT_REPOSITORIES );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.profile.GitRepository#getGitRepositories(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public GitRepository[] getGitRepositories(final UnitOfWork transaction, final String... namePatterns) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getGitRepositoriesGroupingNode( transaction);

        if ( grouping != null ) {
            final List< GitRepository > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final GitRepository gitRepo = new GitRepositoryImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( gitRepo );
            }

            return temp.toArray( new GitRepository[ temp.size() ] );
        }

        return GitRepository.NO_GIT_REPOSITORIES;
    }

    @Override
    public GitRepository addGitRepository(UnitOfWork transaction, String repoName,
                                                                                  URL url, String user, String password) throws KException {
        return RelationalModelFactory.createGitRepository( transaction, getRepository(), this, repoName, url, user, password );
    }

    @Override
    public void removeGitRepository(UnitOfWork transaction, String gitRepoToRemove) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(gitRepoToRemove, "gitRepoToRemove"); //$NON-NLS-1$

        final GitRepository[] gitRepos = getGitRepositories(transaction, gitRepoToRemove);

        if (gitRepos.length == 0) {
            throw new KException(Messages.getString(Relational.GIT_REPO_NOT_FOUND_TO_REMOVE, gitRepoToRemove));
        }

        // remove first occurrence
        gitRepos[0].remove(transaction);
    }

    @Override
    public ViewEditorState addViewEditorState(UnitOfWork transaction, String stateId) throws KException {
        // first delete if already exists
        if ( getViewEditorStates( transaction, stateId ).length != 0 ) {
            removeViewEditorState( transaction, stateId );
        }

        return RelationalModelFactory.createViewEditorState( transaction, getRepository(), this, stateId );
    }

    private KomodoObject getViewEditorStatesGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, KomodoLexicon.Profile.VIEW_EDITOR_STATES );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    @Override
    public ViewEditorState[] getViewEditorStates(UnitOfWork transaction, String... namePatterns) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getViewEditorStatesGroupingNode( transaction);

        if ( grouping != null ) {
            final List< ViewEditorState > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final ViewEditorState gitRepo = new ViewEditorStateImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( gitRepo );
            }

            return temp.toArray( new ViewEditorState[ temp.size() ] );
        }

        return ViewEditorState.NO_VIEW_EDITOR_STATES;
    }

    @Override
    public void removeViewEditorState(UnitOfWork transaction, String viewEditorStateId) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(viewEditorStateId, "viewEditorStateId"); //$NON-NLS-1$

        final ViewEditorState[] states = getViewEditorStates(transaction, viewEditorStateId);

        if (states.length == 0) {
            throw new KException(Messages.getString(Relational.VIEW_EDITOR_STATE_NOT_FOUND_TO_REMOVE, viewEditorStateId));
        }

        // remove first occurrence
        states[0].remove(transaction);
    }
}
