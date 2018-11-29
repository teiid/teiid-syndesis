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
import org.komodo.spi.lexicon.LexiconConstants.JcrLexicon;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Parent Path Clause
 */
public class ParentPathClause extends PathClause implements TeiidSqlConstants.Reserved {

    private boolean childrenOnly = false;

    /**
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias of the selector
     * @param path path used in the clause
     * @param childrenOnly true to return only the direct children
     */
    public ParentPathClause(LogicalOperator operator, String alias, String path, boolean childrenOnly) {
        super(operator, alias, path);
        setChildrenOnly(childrenOnly);
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected ParentPathClause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        super(uow, whereClause);

        if (whereClause.hasProperty(uow, Search.WhereParentPathClause.CHILDREN_ONLY)) {
            setChildrenOnly(whereClause.getProperty(uow, Search.WhereParentPathClause.CHILDREN_ONLY).getBooleanValue(uow));
        }
    }

    /**
     * @return the childrenOnly
     */
    public boolean isChildrenOnly() {
        return this.childrenOnly;
    }

    /**
     * @param childrenOnly the childrenOnly to set
     */
    public void setChildrenOnly(boolean childrenOnly) {
        this.childrenOnly = childrenOnly;
    }

    @Override
    public String clauseString(int position) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(position, buffer);

        setAlias(checkWhereAlias(getAlias()));

//        where e.[jcr:path] LIKE '/inf:patient[265]%'

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias())
                     .append(DOT);
        }

        buffer.append(OPEN_SQUARE_BRACKET)
                 .append(JcrLexicon.JCR_PATH)
                 .append(CLOSE_SQUARE_BRACKET)

                 .append(SPACE)
                 .append(LIKE)
                 .append(SPACE)

                 .append(QUOTE_MARK)
                 .append(getPath());

        if (! getPath().endsWith(CLOSE_SQUARE_BRACKET) &&
             ! getPath().endsWith(FORWARD_SLASH))
            buffer.append(FORWARD_SLASH);

        buffer.append(PERCENT)
                 .append(QUOTE_MARK);

        if (isChildrenOnly()) {

            String path = getPath();
            if (path.endsWith(PERCENT))
                path = path.substring(0, path.length() - 2);

            if (path.endsWith(FORWARD_SLASH))
                path = path.substring(0, path.length() - 2);

            buffer.append(SPACE)
                     .append(LogicalOperator.AND)
                     .append(SPACE)
                     .append("ISCHILDNODE") //$NON-NLS-1$
                     .append(OPEN_BRACKET);

            if (! StringUtils.isEmpty(getAlias())) {
                buffer.append(getAlias())
                         .append(COMMA)
                         .append(SPACE);
            }

            buffer.append(QUOTE_MARK)
                     .append(path)
                     .append(QUOTE_MARK)
                     .append(CLOSE_BRACKET);
        }

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
                                                  Search.WhereParentPathClause.NODE_TYPE);

        writeProperties(uow, whereObject);

        whereObject.setProperty(uow, Search.WherePathClause.PATH, getPath());
        whereObject.setProperty(uow, Search.WhereParentPathClause.CHILDREN_ONLY, isChildrenOnly());
    }
}
