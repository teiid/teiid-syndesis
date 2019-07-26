package org.komodo.spi.repository;

/**
 * Represents one or more operations grouped together forming a transaction.
 */
public interface UnitOfWork {
	
	public class TimeoutException extends Exception {
		
		public TimeoutException(Exception cause) {
			super(cause);
		}
		
	}

    /**
     * Saves all changes made during the transaction. If this is a roll back transaction then {@link #rollback()} is called.
     */
    void commit() throws TimeoutException;

    /**
     * @return the name of the user who initiated the transaction (never <code>null</code>)
     */
    String getUserName();

    /**
     * @return the name of the transaction (never <code>null</code>)
     */
    String getName();

    /**
     * @return true if committed or rolledback
     */
    boolean isCompleted();

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