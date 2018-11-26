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

import java.net.URL;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.profile.internal.GitRepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents the configuration of a git repository
 */
public interface GitRepository extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = GitRepository.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.GIT_REPOSITORY;

    /**
     * An empty array of git repositories.
     */
    GitRepository[] NO_GIT_REPOSITORIES = new GitRepository[0];

    /**
     * The default value of the branch property
     */
    String DEFAULT_BRANCH = "master";

    /**
     * The default value of the commit author property
     */
    String DEFAULT_COMMIT_AUTHOR = "komodo";

    /**
     * The default value of the commit email property
     */
    String DEFAULT_COMMIT_EMAIL = "komodo@komodo.openshift.org";

    /**
     * The resolver of a {@link GitRepository}.
     */
    TypeResolver<GitRepository> RESOLVER = new TypeResolver<GitRepository>() {

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
        public Class<GitRepositoryImpl> owningClass() {
            return GitRepositoryImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(transaction, kobject.getRepository(), kobject, KomodoLexicon.GitRepository.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public GitRepository resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == GitRepository.TYPE_ID) {
                return (GitRepository)kobject;
            }

            return new GitRepositoryImpl(transaction, kobject.getRepository(), kobject.getAbsolutePath());
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>url</code> property
     * @throws KException
     *         if an error occurs
     */
    URL getUrl(final UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param url
     *        the new value for the <code>url</code> property
     * @throws KException
     *         if an error occurs
     */
    void setUrl(final UnitOfWork transaction, final URL url) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>branch</code> property
     * @throws KException
     *         if an error occurs
     */
    String getBranch(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>branch</code> property
     * @throws KException
     *         if an error occurs
     */
    void setBranch(UnitOfWork transaction, String branch) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>user</code> property
     * @throws KException
     *         if an error occurs
     */
    String getUser(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param user
     *        the new value for the <code>user</code> property
     * @throws KException
     *         if an error occurs
     */
    void setUser(UnitOfWork transaction, String branch) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>pswd</code> property
     * @throws KException
     *         if an error occurs
     */
    String getPassword(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param password
     *        the new value for the <code>pswd</code> property
     * @throws KException
     *         if an error occurs
     */
    void setPassword(UnitOfWork transaction, String password) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>commitAuthor</code> property
     * @throws KException
     *         if an error occurs
     */
    String getCommitAuthor(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param commitAuthor
     *        the new value for the <code>commitAuthor</code> property
     * @throws KException
     *         if an error occurs
     */
    void setCommitAuthor(UnitOfWork transaction, String commitAuthor) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>commitEmail</code> property
     * @throws KException
     *         if an error occurs
     */
    String getCommitEmail(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param commitEmail
     *        the new value for the <code>commitEmail</code> property
     * @throws KException
     *         if an error occurs
     */
    void setCommitEmail(UnitOfWork transaction, String commitEmail) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the relative destination parent directory path
     * @throws KException
     *         if an error occurs
     */
    String getTargetDirectory(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param targetDirectory
     *        the new relative destination parent directory path
     * @throws KException
     *         if an error occurs
     */
    void setTargetDirectory(UnitOfWork transaction, String targetDirectory) throws KException;
}
