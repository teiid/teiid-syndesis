package org.komodo.spi.repository;

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