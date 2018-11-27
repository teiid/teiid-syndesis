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

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import org.komodo.core.KomodoLexicon.Search;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Set Clause
 *
 * alias.property IN (value1, value2, value3)
 */
public class SetClause extends Clause implements PropertyClause, TeiidSqlConstants.Reserved {

    private final Set<String> values = new LinkedHashSet<String>();

    /**
     * Constructor
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias
     * @param property the property
     * @param values the value(s)
     */
    public SetClause(LogicalOperator operator, String alias, String property, String... values) {
        super(operator);

        ArgCheck.isNotNull(property);
        ArgCheck.isNotEmpty(values, "Where Set clause requires at least 1 value"); //$NON-NLS-1$

        setAlias(alias);
        setProperty(PROPERTY, property);

        for (String value : values)
            this.values.add(value);
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param whereClause the where clause object
     *
     * @throws KException if error occurs
     */
    protected SetClause(UnitOfWork uow, KomodoObject whereClause) throws KException {
        super(uow, whereClause);

        if (whereClause.hasProperty(uow, Search.WhereSetClause.PROPERTY)) {
            setProperty(whereClause.getProperty(uow, Search.WhereCompareClause.PROPERTY).getStringValue(uow));
        }

        if (whereClause.hasProperty(uow, Search.WhereSetClause.VALUES)) {
            Property valuesProp = whereClause.getProperty(uow, Search.WhereSetClause.VALUES);
            String[] values = valuesProp.getStringValues(uow);
            for (String value : values) {
                addValue(value);
            }
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
     * @return the values
     */
    public Set<String> getValues() {
        return this.values;
    }

    /**
     * Add a value to set of values
     *
     * @param value the value
     */
    public void addValue(String value) {
        values.add(value);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.values == null) ? 0 : this.values.hashCode());
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
        SetClause other = (SetClause)obj;
        if (this.values == null) {
            if (other.values != null)
                return false;
        } else if (!this.values.equals(other.values))
            return false;
        return true;
    }

    @Override
    public String clauseString(int position) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(position, buffer);

        setAlias(checkWhereAlias(getAlias()));

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias());
            buffer.append(DOT);
        }

        String property = getProperty();
        if (STAR.equals(property))
            buffer.append(property);
        else {
            buffer.append(OPEN_SQUARE_BRACKET);
            buffer.append(property);
            buffer.append(CLOSE_SQUARE_BRACKET);
        }

        buffer.append(SPACE);
        buffer.append(IN);
        buffer.append(SPACE);

        buffer.append(OPEN_BRACKET);
        Iterator<String> valuesIter = getValues().iterator();
        appendStringValues(buffer, valuesIter);
        buffer.append(CLOSE_BRACKET);

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
                                                  Search.WhereSetClause.NODE_TYPE);

        writeProperties(uow, whereObject);

        whereObject.setProperty(uow, Search.WhereSetClause.PROPERTY, getProperty());
        whereObject.setProperty(uow, Search.WhereSetClause.VALUES, getValues().toArray());
    }
}