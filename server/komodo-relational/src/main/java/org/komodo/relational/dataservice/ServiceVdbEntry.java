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
package org.komodo.relational.dataservice;

import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a data service entry for a Service VDB.
 */
public interface ServiceVdbEntry extends VdbEntry, VdbEntryContainer {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param dependency
     *        the VDB dependency being added (cannot be <code>null</code>)
     * @return the VDB dependency entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addDependency( final UnitOfWork transaction,
                            final Vdb dependency ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param dependencyEntryName
     *        the name of the VDB dependency entry to create (cannot be empty)
     * @return the VDB dependency entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addDependencyEntry( final UnitOfWork transaction,
                                 final String dependencyEntryName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the entries corresponding to VDB dependency entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry[] getDependencies( final UnitOfWork transaction ) throws KException;
}
