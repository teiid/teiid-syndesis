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
import org.komodo.relational.model.internal.VirtualProcedureImpl;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon.CreateProcedure;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a virtual procedure (CREATE VIRTUAL PROCEDURE).
 */
public interface VirtualProcedure extends Procedure {

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.VIRTUAL_PROCEDURE;

    /**
     * An empty array of virtual procedures.
     */
    VirtualProcedure[] NO_PROCEDURES = new VirtualProcedure[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = VirtualProcedure.class.hashCode();

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Model getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link VirtualProcedure}.
     */
    TypeResolver< VirtualProcedure > RESOLVER = new TypeResolver< VirtualProcedure >() {

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
        public Class< VirtualProcedureImpl > owningClass() {
            return VirtualProcedureImpl.class;
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
                                                        SchemaElementType.VIRTUAL.name() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public VirtualProcedure resolve( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == VirtualProcedure.TYPE_ID ) {
                return ( VirtualProcedure )kobject;
            }

            return new VirtualProcedureImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the AS clause <code>statement</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getAsClauseStatement( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newStatement
     *        the new AS clause statement (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAsClauseStatement( final UnitOfWork transaction,
                               final String newStatement ) throws KException;

}
