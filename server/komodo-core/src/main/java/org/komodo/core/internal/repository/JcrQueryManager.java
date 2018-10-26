/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.core.internal.repository;

import java.util.ArrayList;
import java.util.List;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.query.Query;
import javax.jcr.query.QueryManager;
import javax.jcr.query.QueryResult;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.query.KQueryManager;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import javax.jcr.Session;

public class JcrQueryManager implements KQueryManager {

    private final JcrNodeFactory nodeFactory;

    public JcrQueryManager(JcrNodeFactory nodeFactory) {
        this.nodeFactory = nodeFactory;
    }

    @Override
    public List<KomodoObject> execute(UnitOfWork transaction, Repository repository, String queryStmt) throws KException {
        ArgCheck.isNotEmpty(queryStmt);

        Session session = nodeFactory.getSession(transaction);
        List<KomodoObject> results = new ArrayList<>();

        try {
            QueryManager queryMgr = session.getWorkspace().getQueryManager();
            Query query = queryMgr.createQuery(queryStmt, Query.JCR_SQL2);
            QueryResult result = query.execute();

            NodeIterator itr = result.getNodes();
            while (itr.hasNext()) {
                Node node = itr.nextNode();
                results.add(new ObjectImpl(repository, node.getPath(), node.getIndex()));
            }

            return results;
        } catch (final Exception e) {
            throw nodeFactory.handleError(e);
        }
    }

}
