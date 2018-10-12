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
package org.komodo.relational.profile;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.profile.internal.SqlCompositionImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Interface for SqlComposition
 */
public interface SqlComposition  extends RelationalObject, StringConstants {

    /**
     * The type identifier.
     */
    int TYPE_ID = SqlComposition.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.SQL_COMPOSITION;
    
    /**
     * An empty array of sql compositions.
     */
    SqlComposition[] NO_SQL_COMPOSITIONS = new SqlComposition[0];


    /**
     * The resolver of a {@link ViewDefinition}.
     */
    TypeResolver<SqlComposition> RESOLVER = new TypeResolver<SqlComposition>() {

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
        public Class<SqlCompositionImpl> owningClass() {
            return SqlCompositionImpl.class;
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
        public SqlComposition resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == SqlComposition.TYPE_ID) {
                return (SqlComposition)kobject;
            }

            return new SqlCompositionImpl(transaction, kobject.getRepository(), kobject.getAbsolutePath());
        }

    };
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription(UnitOfWork transaction, String description) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    String getDescription(UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>leftSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLeftSourcePath(UnitOfWork transaction, String leftSource) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>leftSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    String getLeftSourcePath(UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>rightSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRightSourcePath(UnitOfWork transaction, String rightSource) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>rightSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    String getRightSourcePath(UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>leftCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLeftCriteriaColumn(UnitOfWork transaction, String leftCriteriaColumn) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>leftCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    String getLeftCriteriaColumn(UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>rightCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRightCriteriaColumn(UnitOfWork transaction, String rightCriteriaColumn) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>rightCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    String getRightCriteriaColumn(UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
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
     * @param branch
     *        the new value for the <code>operator</code> property
     * @throws KException
     *         if an error occurs
     */
    void setOperator(UnitOfWork transaction, String operator) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>operator</code> property
     * @throws KException
     *         if an error occurs
     */
    String getOperator(UnitOfWork transaction) throws KException;
    
    
}
