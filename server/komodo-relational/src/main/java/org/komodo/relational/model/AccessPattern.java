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
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;


/**
 * Represents a relational model access pattern.
 */
public interface AccessPattern extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = AccessPattern.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.ACCESS_PATTERN;

    /**
     * The constraint type for an access pattern. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.ACCESS_PATTERN;

    /**
     * An empty collection of access pattern constraints.
     */
    AccessPattern[] NO_ACCESS_PATTERNS = new AccessPattern[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Table getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link AccessPattern}.
     */
    TypeResolver< AccessPattern > RESOLVER = new TypeResolver< AccessPattern >() {

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
        public Class< AccessPatternImpl > owningClass() {
            return AccessPatternImpl.class;
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
            final Repository repository = kobject.getRepository();

            return ObjectImpl.validateType( transaction, repository, kobject, Constraint.TABLE_ELEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        repository,
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
        public AccessPattern resolve( final UnitOfWork transaction,
                                      final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == AccessPattern.TYPE_ID ) {
                return ( AccessPattern )kobject;
            }

            return new AccessPatternImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    };
}
