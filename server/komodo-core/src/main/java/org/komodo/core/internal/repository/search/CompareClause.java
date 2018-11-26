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
import org.komodo.core.KomodoLexicon.Search.WhereCompareClause;
import org.komodo.spi.KException;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Comparison clause
 *
 * tagged.[acme:tagName] = 'foo'
 *
 */
public class CompareClause extends Clause implements PropertyClause {

    private ComparisonOperator compareOperator;
    private String value;
    private boolean caseInsensitive = false;


    /**
     * Constructor
     * @param logicalOperator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias
     * @param property the property
     * @param compareOperator the comparison operator
     * @param value the value for comparison
     * @param caseInsensitive the case insensitive flag
     */
    public CompareClause(LogicalOperator logicalOperator,
                                         String alias, String property, ComparisonOperator compareOperator,
                                         String value, boolean caseInsensitive) {
        super(logicalOperator);

        ArgCheck.isNotNull(property);
        ArgCheck.isNotNull(compareOperator);
        ArgCheck.isNotNull(value);

        setAlias(alias);
        setProperty(PROPERTY, property);

        setCompareOperator(compareOperator);
        this.value = value;
        this.caseInsensitive = caseInsensitive;
    }

    /**
     * Constructor
     * @param logicalOperator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias
     * @param property the property
     * @param compareOperator the comparison operator
     * @param value the value for comparison
     */
    public CompareClause(LogicalOperator logicalOperator,
                                         String alias, String property, ComparisonOperator compareOperator,
                                         String value) {
        this(logicalOperator, alias, property, compareOperator, value, false);
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected CompareClause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        super(uow, whereClause);

        if (whereClause.hasProperty(uow, Search.WhereCompareClause.PROPERTY)) {
            setProperty(whereClause.getProperty(uow, Search.WhereCompareClause.PROPERTY).getStringValue(uow));
        }

        if (whereClause.hasProperty(uow, WhereCompareClause.COMPARE_OPERATOR)) {
            String compareOpValue = whereClause.getProperty(uow, Search.WhereCompareClause.COMPARE_OPERATOR).getStringValue(uow);
            setCompareOperator(ComparisonOperator.findOperator(compareOpValue));
        }

        if (whereClause.hasProperty(uow, WhereCompareClause.VALUE)) {
            setValue(whereClause.getProperty(uow, Search.WhereCompareClause.VALUE).getStringValue(uow));
        }

        if (whereClause.hasProperty(uow, WhereCompareClause.CASE_INSENSITIVE)) {
            setCaseInsensitive(whereClause.getProperty(uow, Search.WhereCompareClause.CASE_INSENSITIVE).getBooleanValue(uow));
        }
    }

    /**
     * @return the property
     */
    @Override
    public String getProperty() {
        return properties.get(PROPERTY);
    }

    protected void setProperty(String propertyValue) {
        properties.put(PROPERTY, propertyValue);
    }

    /**
     * @return the compareOperator
     */
    public ComparisonOperator getCompareOperator() {
        return this.compareOperator;
    }

    protected void setCompareOperator(ComparisonOperator compareOperator) {
        this.compareOperator = compareOperator;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return this.value;
    }

    protected void setValue(String value) {
        this.value = value;
    }

    /**
     * @return case insensitive flag
     */
    public boolean isCaseInsensitive() {
        return caseInsensitive;
    }

    protected void setCaseInsensitive(boolean caseInsensitive) {
        this.caseInsensitive = caseInsensitive;
    }

    /**
     * @return where the property is wrapped with functions such as NAME
     */
    private boolean isFunctionProperty() {
        return getProperty().contains(OPEN_BRACKET);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.caseInsensitive ? 1231 : 1237);
        result = prime * result + ((this.compareOperator == null) ? 0 : this.compareOperator.hashCode());
        result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
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
        CompareClause other = (CompareClause)obj;
        if (this.caseInsensitive != other.caseInsensitive)
            return false;
        if (this.compareOperator != other.compareOperator)
            return false;
        if (this.value == null) {
            if (other.value != null)
                return false;
        } else
            if (!this.value.equals(other.value))
                return false;
        return true;
    }

    @Override
    public String clauseString(int index) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(index, buffer);

        if (!isFunctionProperty())
            setAlias(checkWhereAlias(getAlias()));

        if (isCaseInsensitive()) {
            buffer.append("LOWER"); //$NON-NLS-1$
            buffer.append(OPEN_BRACKET);
        }

        if (isFunctionProperty()) {
            buffer.append(getProperty());
        } else {
            // Normal property

            if (! StringUtils.isEmpty(getAlias())) {
                buffer.append(getAlias());
                buffer.append(DOT);
            }
            buffer.append(OPEN_SQUARE_BRACKET);
            buffer.append(getProperty());
            buffer.append(CLOSE_SQUARE_BRACKET);
        }

        if (isCaseInsensitive()) {
            buffer.append(CLOSE_BRACKET);
        }

        buffer.append(SPACE);
        buffer.append(compareOperator);
        buffer.append(SPACE);

        buffer.append(QUOTE_MARK);

        if (isCaseInsensitive())
            buffer.append(value.toLowerCase());
        else
            buffer.append(value);

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
                                                  Search.WhereCompareClause.NODE_TYPE);

        writeProperties(uow, whereObject);

        whereObject.setProperty(uow, Search.WhereCompareClause.PROPERTY, getProperty());
        whereObject.setProperty(uow, Search.WhereCompareClause.COMPARE_OPERATOR, compareOperator.toString());
        whereObject.setProperty(uow, Search.WhereCompareClause.VALUE, value);
        whereObject.setProperty(uow, Search.WhereCompareClause.CASE_INSENSITIVE, caseInsensitive);
    }
}