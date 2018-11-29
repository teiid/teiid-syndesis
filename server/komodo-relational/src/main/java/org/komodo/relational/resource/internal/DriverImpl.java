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
package org.komodo.relational.resource.internal;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.dataservice.internal.DataServiceResourceImpl;
import org.komodo.relational.resource.Driver;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Implementation of driver instance model
 */
public class DriverImpl extends DataServiceResourceImpl< Driver > implements Driver {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public DriverImpl( final UnitOfWork uow,
                       final Repository repository,
                       final String path ) throws KException {
        super( uow, repository, path );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#getFilters()
     */
    @Override
    public Filter[] getFilters() {
        return RelationalObject.NO_FILTERS;
    }

    @Override
    public KomodoType getTypeIdentifier( UnitOfWork uow ) {
        return Driver.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

}
