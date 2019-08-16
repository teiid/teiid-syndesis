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

package org.komodo.rest;

import java.util.concurrent.Callable;

import org.komodo.KEngine;
import org.komodo.KException;
import org.komodo.UnitOfWork;
import org.komodo.utils.KLog;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Base transactional functionality 
 * - TODO: could be built into KEngine instead, just needs a different naming convention
 * other than class name.
 * 
 * In many of the rollback/read only scenarios, a transaction is not necessary
 * 
 * Eventually we'll probably replace with the Transactional annotation
 */
public abstract class AbstractTransactionService implements V1Constants {
	
    protected static final KLog LOGGER = KLog.getLogger();
	
    public static final String REPO_USER = "anonymous";
    
    @Autowired
    protected KEngine kengine;
    
    /**
     * @param user
     *        the user initiating the transaction
     * @param name
     *        the name of the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if transaction must be rolled back
     * @param callback the callback to fire when the transaction is committed
     * @return the new transaction (never <code>null</code>)
     * @throws KException
     *         if there is an error creating the transaction
     */
    private UnitOfWork createTransaction(final String user, final String name,
                                            final boolean rollbackOnly) throws KException {
    	final UnitOfWork result = this.kengine.createTransaction( user,
                                                               (getClass().getSimpleName() + COLON + name + COLON + System.currentTimeMillis()),
                                                               rollbackOnly, REPO_USER);
        LOGGER.debug( "createTransaction:created '%s', rollbackOnly = '%b'", result.getName(), result.isRollbackOnly() ); //$NON-NLS-1$
        return result;
    }
    
    public <T> T runInTransaction(String user, String txnName, boolean rollbackOnly, Callable<T> callable) throws Exception {
		UnitOfWork uow = null;

        try {
            uow = createTransaction(user, txnName, rollbackOnly ); //$NON-NLS-1$
            T result = callable.call();
            commit(uow);
            return result;
        } catch ( final Exception e ) {
            if ( ( uow != null ) && !uow.isCompleted()) {
                uow.rollback();
            }
            throw e;
        }
	}
    
    private void commit(UnitOfWork transaction) throws Exception {
        boolean rollbackOnly = false;
    	if (transaction.isRollbackOnly()) {
    		rollbackOnly = true;
    		transaction.rollback();
    	} else {
    		transaction.commit();
    	}

        LOGGER.debug( "commit: successfully committed '%s', rollbackOnly = '%b'", //$NON-NLS-1$
                transaction.getName(),
                rollbackOnly);
    }
    
}
