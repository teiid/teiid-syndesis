package org.komodo.spi.repository;

import java.util.concurrent.Future;

/**
 * Represents one or more operations grouped together forming a transaction.
 */
public interface UnitOfWork {

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
    Future<Void> commit();

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
    UnitOfWork.State getState();

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