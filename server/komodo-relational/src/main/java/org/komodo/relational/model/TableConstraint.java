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

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.ddl.DdlConstants;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a relational model table constraint.
 */
public interface TableConstraint extends RelationalObject {

    /**
     * The types of table constraints.
     */
    enum ConstraintType {

        ACCESS_PATTERN( TeiidDdlConstants.TeiidNonReservedWord.ACCESSPATTERN.toDdl() ),
        FOREIGN_KEY( DdlConstants.FOREIGN_KEY ),
        INDEX( TeiidDdlConstants.TeiidNonReservedWord.INDEX.toDdl() ),
        PRIMARY_KEY( DdlConstants.PRIMARY_KEY ),
        UNIQUE( TeiidDdlConstants.TeiidReservedWord.UNIQUE.toDdl() );

        final String type;

        private ConstraintType( final String constraintType ) {
            this.type = constraintType;
        }

        /**
         * @return the Teiid value (never empty)
         */
        public String toValue() {
            return this.type;
        }

    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnToAdd
     *        the column being added (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void addColumn( final UnitOfWork transaction,
                    final Column columnToAdd ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the columns contained in this key (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Column[] getColumns( final UnitOfWork transaction ) throws KException;

    /**
     * @return the constraint type (never <code>null</code>)
     */
    ConstraintType getConstraintType();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the parent <code>table</code> (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Table getTable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnToRemove
     *        the column being removed (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void removeColumn( final UnitOfWork transaction,
                       final Column columnToRemove ) throws KException;

}
