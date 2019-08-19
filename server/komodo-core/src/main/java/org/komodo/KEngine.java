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

package org.komodo;

import java.util.concurrent.Callable;

public interface KEngine {

    public class TimeoutException extends Exception {

        private static final long serialVersionUID = -3492466153109760780L;

        public TimeoutException(Exception cause) {
            super(cause);
        }

    }

    /**
     * Start and wait for the engine to be ready
     * @return true if start was successful
     * @throws Exception
     */
    boolean startAndWait() throws Exception;

    /**
     * Get the {@link WorkspaceManager} for manipulating the repository
     * @return
     * @throws KException
     */
    WorkspaceManager getWorkspaceManager() throws KException;

    /**
     * Run the callable in the given transaction
     * @param <T>
     * @param txnName
     * @param rollbackOnly
     * @param callable
     * @return
     * @throws Exception
     */
    <T> T runInTransaction(String txnName, boolean rollbackOnly, Callable<T> callable) throws Exception;
}
