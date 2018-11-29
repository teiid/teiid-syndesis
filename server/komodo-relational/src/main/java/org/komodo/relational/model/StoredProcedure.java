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
package org.komodo.relational.model;

import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.model.internal.StoredProcedureImpl;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon.CreateProcedure;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a stored procedure (CREATE FOREIGN PROCEDURE).
 */
public interface StoredProcedure extends Procedure {

    /**
     * The default value for the <code>non-prepared</code> property. Value is {@value} .
     */
    boolean DEFAULT_NON_PREPARED = false;

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.STORED_PROCEDURE;

    /**
     * An empty array of stored procedures.
     */
    StoredProcedure[] NO_PROCEDURES = new StoredProcedure[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = StoredProcedure.class.hashCode();

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Model getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link StoredProcedure}.
     */
    TypeResolver< StoredProcedure > RESOLVER = new TypeResolver< StoredProcedure >() {

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
        public Class< StoredProcedureImpl > owningClass() {
            return StoredProcedureImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.PROCEDURE_STATEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
                                                        kobject,
                                                        TeiidDdlLexicon.SchemaElement.TYPE,
                                                        SchemaElementType.FOREIGN.name() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public StoredProcedure resolve( final UnitOfWork transaction,
                                        final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == StoredProcedure.TYPE_ID ) {
                return ( StoredProcedure )kobject;
            }

            return new StoredProcedureImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>native query</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNativeQuery( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the result set (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ProcedureResultSet getResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if non-prepared
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NON_PREPARED
     */
    boolean isNonPrepared( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @throws KException
     *         if a result set does not exist or an error occurs
     */
    void removeResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNativeQuery
     *        the new value of the <code>native query</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNativeQuery( final UnitOfWork transaction,
                         final String newNativeQuery ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNonPrepared
     *        the new value for the <code>non-prepared</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NON_PREPARED
     */
    void setNonPrepared( final UnitOfWork transaction,
                         final boolean newNonPrepared ) throws KException;

    /**
     * Deletes the current result set and returns a new one of the requested type.
     *
     * @param <T>
     *        the type of result set
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param resultSetType
     *        the type of result set being requested (cannot be <code>null</code>)
     * @return the new result set (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see TabularResultSet
     * @see DataTypeResultSet
     */
    < T extends ProcedureResultSet > T setResultSet( final UnitOfWork transaction,
                                                     final Class< T > resultSetType ) throws KException;

}
