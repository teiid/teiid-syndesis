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

import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.nodetype.NodeTypeManager;

import org.komodo.core.repository.KomodoObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.spi.repository.UnitOfWorkDelegate;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrSession;

public class AbstractJcrFactory {

    private static KLog LOGGER = KLog.getLogger();

    public AbstractJcrFactory() {
        super();
    }

    void checkTransaction(UnitOfWork transaction) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
    }

    Session getSession(UnitOfWork transaction) {
        checkTransaction(transaction);
    
        UnitOfWorkDelegate delegate = transaction.getDelegate();
        if (!(delegate instanceof JcrUowDelegate))
            throw new UnsupportedOperationException();
    
        return ((JcrUowDelegate)delegate).getImplementation();
    }

    /**
     * Wraps error in a {@link KException} if necessary.
     *
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error (never <code>null</code>)
     */
    KException handleError(Exception e) {
        assert (e != null);
    
        if (e instanceof KException) {
            return (KException)e;
        }
    
        return new KException(e);
    }

    /**
     * Implementation specific conversion from an absolute path into {@link Node}
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param absPath absolute path
     *
     * @return the underlying object
     * @throws KException
     *         if an error occurs
     */
    Node node(UnitOfWork transaction, String absPath) throws KException {
        checkTransaction(transaction);
    
        PathNotFoundException throwEx = null;
        Node node = null;
        Session session = getSession(transaction);
    
        try {
            //
            // Try finding the node with the conventional path as given
            //
            try {
                node = session.getNode(absPath);
            } catch (PathNotFoundException ex) {
                // node cannot be found with conventional path as given
                throwEx = ex;
            }
    
            if (node == null && session instanceof JcrSession) {
                JcrSession jcrSession = (JcrSession)session;
    
                //
                // Try finding the node with the path decoded
                //
                try {
                    String decPath = jcrSession.decode(absPath);
                    node = session.getNode(decPath);
                } catch (PathNotFoundException ex) {
                    // node cannot be found with decoded path
                }
    
                if (node == null) {
                    //
                    // Try finding the node with the path encoded
                    //
                    try {
                        String encPath = jcrSession.encode(absPath);
                        node = session.getNode(encPath);
                    } catch (Exception ex) {
                        // node cannot be found with encoded path
                    }
                }
            }
    
            if (node == null) {
                // throw the original path not found exception
                throw throwEx;
            }
    
            // return the found node
            return node;
        } catch (Exception e) {
            throw handleError(e);
        }
    }

    /**
     * Implementation specific conversion from {@link KomodoObject} into {@link Node}
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param kObject
     *
     * @return the underlying object
     * @throws KException
     *         if an error occurs
     */
    Node node(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
    
        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-node: transaction = {0}, path = {1}", transaction.getName(), kObject.getAbsolutePath()); //$NON-NLS-1$
        }
    
        return node(transaction, kObject.getAbsolutePath());
    }

    /**
     * @param transaction
     * @return the {@link NodeTypeManager}
     * @throws RepositoryException
     */
    protected NodeTypeManager nodeTypeManager(UnitOfWork transaction) throws Exception {
        NodeTypeManager nodeTypeMgr = getSession(transaction).getWorkspace().getNodeTypeManager();
        return nodeTypeMgr;
    }
}
