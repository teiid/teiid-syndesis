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

/**
 * Represents a relational model foreign key.
 */
public interface ForeignKey extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = ForeignKey.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.FOREIGN_KEY;

    /**
     * The constraint type for a foreign key. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.FOREIGN_KEY;

    @Override
    Table getRelationalParent( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newReferencesColumn
     *        the references table columns being added (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void addReferencesColumn( 
                              final Column newReferencesColumn ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the columns referenced from the references table (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Column[] getReferencesColumns( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the references table (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Table getReferencesTable( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param referencesColumnToRemove
     *        the references table column being removed (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void removeReferencesColumn( 
                                 final Column referencesColumnToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newReferencesTable
     *        the new value for the references table (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setReferencesTable( 
                             final Table newReferencesTable ) throws KException;

}
