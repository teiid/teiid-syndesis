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

import java.util.ArrayList;
import java.util.List;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.query.Query;
import javax.jcr.query.QueryManager;
import javax.jcr.query.QueryResult;

import org.komodo.core.repository.KQueryManager;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
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
