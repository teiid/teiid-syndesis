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
package org.komodo.relational.vdb;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;

/**
 * Represents a referenced VDB.
 */
public interface VdbImport extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = VdbImport.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_IMPORT;

    /**
     * The default value indicating if the data policies should be imported. Value is {@value} .
     */
    boolean DEFAULT_IMPORT_DATA_POLICIES = true;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Vdb getParent( final UnitOfWork transaction ) throws KException;


    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see Vdb#DEFAULT_VERSION
     */
    int getVersion( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if data policies should be imported
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_IMPORT_DATA_POLICIES
     */
    boolean isImportDataPolicies( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newImportDataPolicies
     *        the new value for the <code>import data policies</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_IMPORT_DATA_POLICIES
     */
    void setImportDataPolicies( final UnitOfWork transaction,
                                final boolean newImportDataPolicies ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newVersion
     *        the new value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see Vdb#DEFAULT_VERSION
     */
    void setVersion( final UnitOfWork transaction,
                     final int newVersion ) throws KException;

}
