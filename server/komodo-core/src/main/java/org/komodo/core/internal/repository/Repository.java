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
package org.komodo.core.internal.repository;

import java.net.URL;
import java.util.List;

import org.komodo.core.KEngineImpl;
import org.komodo.core.KObserver;
import org.komodo.core.repository.KPropertyFactory;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.OperationType;
import org.komodo.core.repository.RepositoryClientEvent;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.metadata.MetadataInstance;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;

/**
 * A repository is a data store containing artifacts generated while modeling VDBs
 */
public interface Repository {

    /**
     * A repository identifier.
     */
    interface Id {

        /**
         * @return the repository configuration location
         */
        URL getConfiguration();

        /**
         * @return the repository URL (cannot be empty)
         */
        String getUrl();

        /**
         * @return the repository workspace name (cannot be empty)
         */
        String getWorkspaceName();

    }

    /**
     * The repository state.
     */
    enum State {

        /**
         * The repository cannot be reached.
         */
        NOT_REACHABLE,

        /**
         * The repository can be communicated with.
         */
        REACHABLE

    }

    /**
     * Prepares the given object to be acted upon by the transaction, including testing if
     * such operation violates any security constraints and ensuring that a user-space is
     * available in the workspace.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     *
     * @param object the object to be acted upon
     * @param requestType the nature of the request being submitted, eg. read-only or writeable.
     *
     * @throws KException if security failure occurs
     */
    void provision(UnitOfWork transaction, KomodoObject object, OperationType requestType) throws KException;

    /**
     * @return the factory for handling node implementations
     */
    KObjectFactory getObjectFactory();

    /**
     * @return the factory for handling property implementations
     */
    KPropertyFactory getPropertyFactory();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     * @param parentPath
     *        the parent path where the workspace object is created (can be empty if adding at the root of the workspace)
     * @param name
     *        the name of the object (cannot be empty)
     * @param primaryType
     *        the primary type of the child or <code>null</code> if type is <code>nt:unstructured</code>
     * @return the new workspace object (never <code>null</code>)
     * @throws KException
     *         if the parent path does not exist or an error occurs
     */
    KomodoObject add( final UnitOfWork transaction,
                      final String parentPath,
                      final String name,
                      final String primaryType ) throws KException;

    /**
     * @param observer
     *        the observer to be added
     */
    void addObserver( KObserver observer );

    /**
     * @param userName
     *       the user name of the transaction initiator
     * @param name
     *        a name for the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if the transaction should only be rolled back
     * @param callback
     *        a listener that is notified when the transaction is finished (can be <code>null</code>
     * @return a unit of work transaction that must be either committed or rolled back (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    UnitOfWork createTransaction(final String userName, final String name,
                                  final boolean rollbackOnly,
                                  final SynchronousCallback callback,
                                  final String repoUser) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     * @param query
     *        the SQL query
     *
     * @return the {@link KomodoObject}s resulting from the search
     * @throws KException if error occurs
     */
    List<KomodoObject> query(UnitOfWork transaction, String query) throws KException;

    /**
     * Get an object from the workspace part of the repository.
     *
     * The path can be workspace relative or absolute.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     * @param path
     *        the path to the workspace object being requested (can be empty if the workspace root)
     * @return the requested workspace Komodo object (can be <code>null</code> if it does not exist)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject getFromWorkspace( final UnitOfWork transaction,
                                   final String path ) throws KException;

    /**
     * Gets the {@link KomodoObject} with the specified identifier.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     * @param jcrUuid
     *        the value of the <code>jcr:uuid</code> property (cannot be empty)
     * @return the requested workspace Komodo object or <code>null</code> if not found
     * @throws KException
     *         if an error occurs
     */
    KomodoObject getUsingId( final UnitOfWork transaction,
                             final String jcrUuid ) throws KException;

    /**
     * @return the repository identifier (never <code>null</code>)
     */
    Id getId();

    /**
     * @return the repository's running state (never <code>null</code>)
     */
    State getState();

    /**
     * Notify the repository of the given {@link RepositoryClientEvent}
     *
     * @param event
     */
    void notify( RepositoryClientEvent event );

    /**
     * @return <code>true</code> if the repository can be communicated with
     */
    boolean ping();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     * @param paths
     *        the paths of the workspace objects being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if a workspace path does not exist or an error occurs
     */
    void remove( final UnitOfWork transaction,
                 final String... paths ) throws KException;

    /**
     * Remove an {@link KClient} that we no longer wish to receive notifications from
     *
     * @param client
     */
    void removeClient( KEngineImpl client );

    /**
     * @param observer
     *        the observer to be removed
     */
    void removeObserver( KObserver observer );

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     * @param artifactPaths
     *        the paths of the the artifacts being removed (cannot be <code>null</code> or empty)
     * @throws KException
     *         if an artifact does not exist in the library or an error occurs
     */
    void unpublish( final UnitOfWork transaction,
                    final String... artifactPaths ) throws KException;

    /**
     * The root of the Komodo environment area in the repository (i.e., /tko:komodo/tko:environment).
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     * @return the Komodo environment root (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject komodoEnvironment( final UnitOfWork transaction ) throws KException;

    /**
     * The komodo library in the repository, ie. /tko:komodo/tko:library
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     *
     * @return the komodo library
     * @throws KException if an error occurs
     */
    KomodoObject komodoLibrary( final UnitOfWork transaction) throws KException;

    /**
     * The komodo user's workspace in the repository, ie. /tko:komodo/tko:workspace/${user}
     * where ${user} is the user owning the given transaction
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     *
     * @return the komodo user workspace
     * @throws KException if an error occurs
     */
    KomodoObject komodoWorkspace( final UnitOfWork transaction) throws KException;

    /**
     * The komodo user's profile in the repository, ie. /tko:komodo/tko:environment/tko:profiles/${user}
     * where ${user} is the user owning the given transaction
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     *
     * @return the komodo user profile
     * @throws KException if an error occurs
     */
    KomodoObject komodoProfile(final UnitOfWork uow) throws KException;

    /**
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     *
     * @return the komodo validation root
     * @throws KException if an error occurs
     */
    KomodoObject komodoValidationRoot(final UnitOfWork transaction) throws KException;

    /**
    *
    * @param transaction
    *        the transaction (cannot be <code>null</code> or have a state that is not
    *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
    *
    * @return the komodo user profiles node
    * @throws KException if an error occurs
    */
    KomodoObject komodoProfiles(UnitOfWork transaction) throws KException;

    /**
     * Get Metadata Instance
     * @return
     */
    MetadataInstance getMetadataInstance() throws KException;
}
