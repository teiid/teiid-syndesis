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

public interface KEngine {
	
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
     * Creates and associates a transaction with the current thread.
     * @param userName
     *       the user name of the transaction initiator
     * @param name
     *        a name for the transaction (cannot be empty)
     * @param rollbackOnly
     *        <code>true</code> if the transaction should only be rolled back
     * @param callback
     *        a listener that is notified when the transaction is finished (can be <code>null</code>
     * @param repoUser       
     * @return a unit of work transaction that must be either committed or rolled back (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
	UnitOfWork createTransaction(String userName, String name, boolean rollbackOnly, String repoUser) throws KException;
	
}
