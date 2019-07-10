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
package org.komodo.spi.repository;

import java.net.URL;
import java.util.List;
import org.komodo.spi.KClient;
import org.komodo.spi.KException;
import org.komodo.spi.metadata.MetadataInstance;

/**
 * A repository is a data store containing artifacts generated while modeling VDBs
 */
public interface Repository {

    /**
     * System user for transactions to be executed internally
     */
    String SYSTEM_USER = "SYSTEM";

    /**
     * The nature of the operation being conducted
     * and to be vetted by the security system
     */
    enum OperationType {
        /**
         * Can read a node's attributes and get its children
         */
        READ_OPERATION,

        /**
         * Can add/remove children from a node but
         * cannot modify the node itself
         */
        CHILD_OPERATION,

        /**
         * Can modify a node's attributes
         */
        MODIFY_OPERATION,

        /**
         * Can a node be removed
         */
        REMOVE_OPERATION
    }

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
     * Library and workspace searches using keywords will use one of these criteria.
     */
    enum KeywordCriteria {

        /**
         * All keywords must be present in the search result.
         */
        ALL,

        /**
         * At least one of the keywords must be present in the search result. This is the default.
         */
        ANY,

        /**
         * None of the keywords may be present in the search result.
         */
        NONE;

        /**
         * @return the default keyword to use (never <code>null</code>)
         */
        public static KeywordCriteria getDefault() {
            return ANY;
        }

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
     * The repository type.
     */
    enum Type {

        /**
         * The local workspace repository.
         */
        LOCAL,

        /**
         * A shared repository.
         */
        SHARED

    }

    /**
     * Represents one or more operations grouped together forming a {@link Repository repository} transaction.
     */
    interface UnitOfWork {

        /**
         * The transaction states.
         */
        public enum State {

            /**
             * The transaction has been committed.
             */
            COMMITTED,

            /**
             * There was an error running the transaction.
             */
            ERROR,

            /**
             * The transaction has not been run (neither commit or rollback has been called).
             */
            NOT_STARTED,

            /**
             * The transaction has been rolled back.
             */
            ROLLED_BACK,

            /**
             * The transaction is currently being committed or rolled back.
             */
            RUNNING;

            /**
             * @return <code>true</code> if this state is the final, not intermediate, state
             */
            public boolean isFinal() {
                return ( ( this == COMMITTED ) || ( this == ERROR ) || ( this == ROLLED_BACK ) );
            }

        }

        /**
         * Saves all changes made during the transaction. If this is a roll back transaction then {@link #rollback()} is called.
         */
        void commit();

        /**
         * @return the listener being notified when the transaction is finished (can be <code>null</code>)
         */
        UnitOfWorkListener getCallback();

        /**
         * @return the session delegate of this transaction
         */
        UnitOfWorkDelegate getDelegate();

        /**
         * @return an error caught during the transaction (can be <code>null</code>)
         * @see State#ERROR
         */
        KException getError();

        /**
         * @return the name of the user who initiated the transaction (never <code>null</code>)
         */
        String getUserName();

        /**
         * @return the name of the transaction (never <code>null</code>)
         */
        String getName();

        /**
         * @return the transaction state (never <code>null</code>)
         */
        State getState();

        /**
         * @return <code>true</code> if the transaction contains operations that change the state of the repository
         * @throws KException
         *         if there is an error determining if there are unsaved changeds
         */
        boolean hasChanges() throws KException;

        /**
         * @return <code>true</code> if only rollback is allowed
         */
        boolean isRollbackOnly();

        /**
         * Discards all current changes made during this transaction.
         */
        void rollback();
        
        /**
         * repository user. Always SYSTEM
         * @return
         */
        String getRepositoryUser();
    }

    /**
     * A listener notified when a unit of work completes.
     */
    public interface UnitOfWorkListener {

        /**
         * @param error
         *        the error that occurred processing the transaction (never <code>null</code>)
         */
        void errorOccurred( final Throwable error );

        /**
         * @param results
         *        the results of the work (can be <code>null</code>)
         */
        void respond( final Object results );

    }

    /**
     * Prepares the given object to be acted upon by the transaction, including testing if
     * such operation violates any security constraints and ensuring that a user-space is
     * available in the workspace.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     * Add an {@link KClient} to send/receive notifications to/from
     *
     * @param client
     */
    void addClient( KClient client );

    /**
     * @param observer
     *        the observer to be added
     */
    void addObserver( RepositoryObserver observer );

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
                                  final UnitOfWorkListener callback,
                                  final String repoUser) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     * @return the repository's type (never <code>null</code>)
     */
    Type getType();

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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
    void removeClient( KClient client );

    /**
     * @param observer
     *        the observer to be removed
     */
    void removeObserver( RepositoryObserver observer );

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param artifactPaths
     *        the paths of the artifacts being requested (cannot be <code>null</code> or empty)
     * @return the requested artifacts (never <code>null</code>)
     * @throws KException
     *         if an artifact does not exist or an error occurs
     */
    Artifact[] retrieve( final UnitOfWork transaction,
                         final String... artifactPaths ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     *
     * @return the komodo user profile
     * @throws KException if an error occurs
     */
    KomodoObject komodoProfile(final UnitOfWork uow) throws KException;

    /**
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     *
     * @return the komodo validation root
     * @throws KException if an error occurs
     */
    KomodoObject komodoValidationRoot(final UnitOfWork transaction) throws KException;

    /**
    *
    * @param transaction
    *        the transaction (cannot be <code>null</code> or have a state that is not
    *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
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
