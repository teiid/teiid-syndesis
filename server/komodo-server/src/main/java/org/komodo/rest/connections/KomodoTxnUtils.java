package org.komodo.rest.connections;

import static org.komodo.rest.Messages.Error.COMMIT_TIMEOUT;

import java.util.concurrent.TimeUnit;

import org.komodo.core.KEngine;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.Messages;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;

public class KomodoTxnUtils implements V1Constants {
	
    private static final int TIMEOUT = 30;
    private static final TimeUnit UNIT = TimeUnit.SECONDS;
    /**
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @param callback the callback to fire when the transaction is committed
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected static UnitOfWork createTransaction(final KEngine engine, final String username, final String txnName,
                                            final boolean rollbackOnly, final UnitOfWorkListener callback) throws KException {
    	Repository repo = engine.getDefaultRepository();
        final UnitOfWork result = repo.createTransaction( username,
                                                               (KomodoTxnUtils.class.getSimpleName() + COLON + txnName + COLON + System.currentTimeMillis()),
                                                               rollbackOnly, callback );
         return result;
    }

    /**
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    protected static UnitOfWork createTransaction(final KEngine engine, final String username, final String txnName,
                                            final boolean rollbackOnly ) throws KException {
    	Repository repo = engine.getDefaultRepository();
        final SynchronousCallback callback = new SynchronousCallback();
        final UnitOfWork result = repo.createTransaction(username,
                                                               (KomodoTxnUtils.class.getSimpleName() + COLON + txnName + COLON + System.currentTimeMillis()),
                                                               rollbackOnly, callback );
        return result;
    }
    
    protected static void commit(UnitOfWork transaction) throws Exception {
        assert( transaction.getCallback() instanceof SynchronousCallback );
        final int timeout = TIMEOUT;
        final TimeUnit unit = UNIT;

        final SynchronousCallback callback = ( SynchronousCallback )transaction.getCallback();
        transaction.commit();

        if ( ! callback.await( timeout, unit ) ) {
            // callback timeout occurred
            String errorMessage = Messages.getString( COMMIT_TIMEOUT, transaction.getName(), timeout, unit );
            throw new KException(errorMessage);
        }

        Throwable error = transaction.getError();
        if ( error != null ) {
            // callback was called because of an error condition
        	throw new KException(error);
        }

        error = callback.error();
        if ( error != null ) {
         // callback was called because of an error condition
        	throw new KException(error);
        }
    }
	
    protected static WorkspaceManager getWorkspaceManager(final KEngine engine, UnitOfWork transaction) throws KException {
    	Repository repo = engine.getDefaultRepository();
        return WorkspaceManager.getInstance(repo, transaction);
    }
}
