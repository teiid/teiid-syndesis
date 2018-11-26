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
package org.komodo.relational.profile;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.profile.internal.SqlProjectedColumnImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Interface for SqlProjectedColumn
 */
public interface SqlProjectedColumn  extends RelationalObject, StringConstants {

    /**
     * The type identifier.
     */
    int TYPE_ID = SqlProjectedColumn.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.SQL_PROJECTED_COLUMN;
    
    /**
     * An empty array of sql compositions.
     */
    SqlProjectedColumn[] NO_SQL_PROJECTED_COLUMNS = new SqlProjectedColumn[0];


    /**
     * The resolver of a {@link ViewDefinition}.
     */
    TypeResolver<SqlProjectedColumn> RESOLVER = new TypeResolver<SqlProjectedColumn>() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class<SqlProjectedColumnImpl> owningClass() {
            return SqlProjectedColumnImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(transaction, kobject.getRepository(), kobject, KomodoLexicon.SqlComposition.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public SqlProjectedColumn resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == SqlProjectedColumn.TYPE_ID) {
                return (SqlProjectedColumn)kobject;
            }

            return new SqlProjectedColumnImpl(transaction, kobject.getRepository(), kobject.getAbsolutePath());
        }

    };
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the new value for the <code>name</code> property
     * @throws KException
     *         if an error occurs
     */
    void setName(UnitOfWork transaction, String name) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>name</code> property
     * @throws KException
     *         if an error occurs
     */
    @Override
    String getName(UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param type
     *        the new value for the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    void setType(UnitOfWork transaction, String type) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    String getType(UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param selected value for selected
     * @throws KException
     *         if an error occurs         
     */
    void setSelected(final UnitOfWork transaction, boolean selected) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return boolean value of selected
     * @throws KException
     *         if an error occurs         
     */
    boolean isSelected(final UnitOfWork transaction) throws KException;
    
}
