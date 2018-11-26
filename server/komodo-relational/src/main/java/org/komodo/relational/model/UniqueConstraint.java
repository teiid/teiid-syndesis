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
import org.komodo.relational.model.internal.UniqueConstraintImpl;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon.Constraint;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model unique constraint.
 */
public interface UniqueConstraint extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = UniqueConstraint.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.UNIQUE_CONSTRAINT;

    /**
     * The constraint type for a unique constraint. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.UNIQUE;

    /**
     * An empty collection of unique constraints.
     */
    UniqueConstraint[] NO_UNIQUE_CONSTRAINTS = new UniqueConstraint[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Table getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link UniqueConstraint}.
     */
    TypeResolver< UniqueConstraint > RESOLVER = new TypeResolver< UniqueConstraint >() {

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
        public Class< UniqueConstraintImpl > owningClass() {
            return UniqueConstraintImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, Constraint.TABLE_ELEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
                                                        kobject,
                                                        Constraint.TYPE,
                                                        CONSTRAINT_TYPE.toValue() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public UniqueConstraint resolve( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == UniqueConstraint.TYPE_ID ) {
                return ( UniqueConstraint )kobject;
            }

            return new UniqueConstraintImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

}
