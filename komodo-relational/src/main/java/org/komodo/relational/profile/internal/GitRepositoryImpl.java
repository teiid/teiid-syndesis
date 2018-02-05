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

import java.net.MalformedURLException;
import java.net.URL;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.profile.GitRepository;
import org.komodo.relational.profile.Profile;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a git repository configuration object.
 */
public class GitRepositoryImpl extends RelationalChildRestrictedObject implements GitRepository {

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
    public GitRepositoryImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return GitRepository.IDENTIFIER;
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
    public URL getUrl(UnitOfWork transaction) throws KException {
        Object value = getObjectProperty(transaction, PropertyValueType.STRING, "getUrl", KomodoLexicon.GitRepository.URL); //$NON-NLS-1$
        if (value == null)
            return null;

        try {
            return new URL(value.toString());
        } catch (MalformedURLException ex) {
            throw new KException(ex);
        }
    }

    @Override
    public void setUrl(UnitOfWork transaction, URL url) throws KException {
        setObjectProperty(transaction, "setUrl", KomodoLexicon.GitRepository.URL, url.toString()); //$NON-NLS-1$
    }

    @Override
    public String getBranch(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getBranch", KomodoLexicon.GitRepository.BRANCH); //$NON-NLS-1$
    }

    @Override
    public void setBranch(UnitOfWork transaction, String branch) throws KException {
        if (branch == null)
            branch = DEFAULT_BRANCH;

        setObjectProperty(transaction, "setBranch", KomodoLexicon.GitRepository.BRANCH, branch); //$NON-NLS-1$
    }

    @Override
    public String getUser(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getUser", KomodoLexicon.GitRepository.USER); //$NON-NLS-1$
    }

    @Override
    public void setUser(UnitOfWork transaction, String user) throws KException {
        setObjectProperty(transaction, "setUser", KomodoLexicon.GitRepository.USER, user); //$NON-NLS-1$
    }

    @Override
    public String getPassword(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getPassword", KomodoLexicon.GitRepository.PASSWORD); //$NON-NLS-1$
    }

    @Override
    public void setPassword(UnitOfWork transaction, String password) throws KException {
        setObjectProperty(transaction, "setPassword", KomodoLexicon.GitRepository.PASSWORD, password); //$NON-NLS-1$
    }

    @Override
    public String getCommitAuthor(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getCommitAuthor", KomodoLexicon.GitRepository.COMMIT_AUTHOR); //$NON-NLS-1$
    }

    @Override
    public void setCommitAuthor(UnitOfWork transaction, String commitAuthor) throws KException {
        if (commitAuthor == null)
            commitAuthor = DEFAULT_COMMIT_AUTHOR;

        setObjectProperty(transaction, "setCommitAuthor", KomodoLexicon.GitRepository.COMMIT_AUTHOR, commitAuthor); //$NON-NLS-1$
    }

    @Override
    public String getCommitEmail(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getCommitEmail", KomodoLexicon.GitRepository.COMMIT_EMAIL); //$NON-NLS-1$
    }

    @Override
    public void setCommitEmail(UnitOfWork transaction, String commitEmail) throws KException {
        if (commitEmail == null)
            commitEmail = DEFAULT_COMMIT_EMAIL;

        setObjectProperty(transaction, "setCommitEmail", KomodoLexicon.GitRepository.COMMIT_EMAIL, commitEmail); //$NON-NLS-1$
    }

    @Override
    public String getTargetDirectory(UnitOfWork transaction) throws KException {
        return getObjectProperty(transaction, PropertyValueType.STRING, "getTargetDirectory", KomodoLexicon.GitRepository.TARGET_DIRECTORY); //$NON-NLS-1$
    }

    @Override
    public void setTargetDirectory(UnitOfWork transaction, String targetDirectory) throws KException {
        setObjectProperty(transaction, "setTargetDirectory", KomodoLexicon.GitRepository.TARGET_DIRECTORY, targetDirectory); //$NON-NLS-1$
    }
}
