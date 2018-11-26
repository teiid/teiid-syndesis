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
package org.komodo.core.internal.repository.search;

import org.komodo.core.KomodoLexicon.Search;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Path Clause
 */
public class PathClause extends Clause implements TeiidSqlConstants.Reserved {

    private final static String PATH = "path"; //$NON-NLS-1$

    /**
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias of the selector
     * @param path path used in the clause
     */
    public PathClause(LogicalOperator operator, String alias, String path) {
        super(operator);

        ArgCheck.isNotEmpty(path, "Where Path clause requires a path"); //$NON-NLS-1$

        setAlias(alias);
        setPath(path);
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected PathClause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        super(uow, whereClause);

        if (whereClause.hasProperty(uow, Search.WherePathClause.PATH)) {
            setPath(whereClause.getProperty(uow, Search.WherePathClause.PATH).getStringValue(uow));
        }
    }

    /**
     * @return the path
     */
    public String getPath() {
        return properties.get(PATH);
    }

    /**
     * @param path the path to be added
     */
    public void setPath(String path) {
        properties.put(PATH, path);
    }

    @Override
    public String clauseString(int position) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(position, buffer);

        setAlias(checkWhereAlias(getAlias()));

        buffer.append("PATH"); //$NON-NLS-1$
        buffer.append(OPEN_BRACKET);

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias());
        }

        buffer.append(CLOSE_BRACKET);
        buffer.append(SPACE);
        buffer.append(LIKE);
        buffer.append(SPACE);
        buffer.append(QUOTE_MARK);
        buffer.append(getPath());
        buffer.append(QUOTE_MARK);

        return buffer.toString();
    }

    @Override
    void write(UnitOfWork uow, KomodoObject searchObject) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(searchObject, "searchObject"); //$NON-NLS-1$

        Repository repository = searchObject.getRepository();
        KomodoObject whereObject = repository.add(uow, searchObject.getAbsolutePath(),
                                                  Search.WHERE_CLAUSE,
                                                  Search.WherePathClause.NODE_TYPE);

        writeProperties(uow, whereObject);

        whereObject.setProperty(uow, Search.WherePathClause.PATH, getPath());
    }
}
