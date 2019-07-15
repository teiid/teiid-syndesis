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

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;

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

}
