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

package org.komodo.repository;

import org.komodo.KEngine;
import org.komodo.KException;
import org.komodo.UnitOfWork;
import org.komodo.WorkspaceManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.TransactionTimedOutException;
import org.springframework.transaction.support.DefaultTransactionDefinition;

/**
 * Provides the repository facade and transaction handling
 */
@Component
public class KEngineImpl implements KEngine {

    private class UnitOfWorkImpl implements UnitOfWork {

        private String userName;
        private String name;
        private String repositoryUser;
        private TransactionStatus status;

        public UnitOfWorkImpl(String userName, String name, String repoUser, TransactionStatus transactionStatus) {
            this.userName = userName;
            this.name = name;
            this.repositoryUser = repoUser;
            this.status = transactionStatus;
        }

        @Override
        public void commit() throws TimeoutException {
            try {
                platformTransactionManager.commit(status);
            } catch (TransactionTimedOutException e) {
                throw new TimeoutException(e);
            }
            //any other exceptions can be unchecked
        }

        @Override
        public String getUserName() {
            return userName;
        }

        @Override
        public String getName() {
            return name;
        }

        @Override
        public boolean isCompleted() {
            return status.isCompleted();
        }

        @Override
        public boolean isRollbackOnly() {
            return status.isRollbackOnly();
        }

        @Override
        public void rollback() {
            platformTransactionManager.rollback(status);
        }

        @Override
        public String getRepositoryUser() {
            return this.repositoryUser;
        }

    }

    private static DefaultTransactionDefinition NEW_TRANSACTION_DEFINITION = new DefaultTransactionDefinition();
    static {
        NEW_TRANSACTION_DEFINITION.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRED);
    }

    @Autowired
    private WorkspaceManagerImpl workspaceManagerImpl;

    @Autowired
    private PlatformTransactionManager platformTransactionManager;

    @Override
    public boolean startAndWait() throws Exception {
        this.workspaceManagerImpl.findDataVirtualization("x");
        return true;
    }

    @Override
    public WorkspaceManager getWorkspaceManager() throws KException {
        return workspaceManagerImpl;
    }

    @Override
    public UnitOfWork createTransaction(String userName, String name, boolean rollbackOnly, String repoUser)
            throws KException {
        TransactionStatus transactionStatus = platformTransactionManager.getTransaction(NEW_TRANSACTION_DEFINITION);
        if (rollbackOnly) {
            transactionStatus.setRollbackOnly();
        }
        return new UnitOfWorkImpl(userName, name, repoUser, transactionStatus);
    }

}
