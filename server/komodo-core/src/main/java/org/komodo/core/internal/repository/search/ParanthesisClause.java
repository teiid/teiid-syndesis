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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.core.KomodoLexicon.Search;
import org.komodo.spi.KException;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * A Paranthesis clause
 *
 * (clause1 OR/AND clause2)
 */
public class ParanthesisClause extends Clause {

    private List<Clause> childClauses = new ArrayList<>();

    /**
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param childClauses child clauses
     */
    public ParanthesisClause(LogicalOperator operator, Clause... childClauses) {
        super(operator);

        ArgCheck.isNotNull(childClauses);

        this.childClauses.addAll(Arrays.asList(childClauses));
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected ParanthesisClause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        super(uow, whereClause);

        KomodoObject[] children = whereClause.getChildren(uow, Search.WHERE_CLAUSE);
        ArgCheck.isNotNull(children);
        ArgCheck.isTrue(children.length > 0, "sub where clauses cannot be empty"); //$NON-NLS-1$

        for (KomodoObject whereClauseObject : children) {
            Clause clause = Clause.createClause(uow, whereClauseObject);
            if (clause != null)
                addWhereClause(clause);
        }
    }

    /**
     * @param childClause the child clause
     */
    public void addWhereClause(Clause childClause) {
        childClauses.add(childClause);
    }

    @Override
    public String clauseString(int index) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(index, buffer);

        buffer.append(OPEN_BRACKET);

        for (int i = 0; i < childClauses.size(); ++i) {
            buffer.append(childClauses.get(i).clauseString(i));
            if (i < (childClauses.size() - 1))
                buffer.append(SPACE);
        }

        buffer.append(CLOSE_BRACKET);

        return buffer.toString();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.childClauses == null) ? 0 : this.childClauses.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        ParanthesisClause other = (ParanthesisClause)obj;
        if (this.childClauses == null) {
            if (other.childClauses != null)
                return false;
        } else
            if (!this.childClauses.equals(other.childClauses))
                return false;
        return true;
    }

    @Override
    void write(UnitOfWork uow, KomodoObject searchObject) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(searchObject, "searchObject"); //$NON-NLS-1$

        Repository repository = searchObject.getRepository();
        KomodoObject whereObject = repository.add(uow, searchObject.getAbsolutePath(),
                                                  Search.WHERE_CLAUSE,
                                                  Search.WhereParanthesisClause.NODE_TYPE);

        for (Clause childClause : childClauses) {
            childClause.write(uow, whereObject);
        }
    }

}
