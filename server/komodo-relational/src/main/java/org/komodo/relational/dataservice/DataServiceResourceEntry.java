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

import java.io.InputStream;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data service resource (i.e., a driver, UDF, or DDL file).
 *
 * @param <T>
 *        the type of data service resource
 */
public interface DataServiceResourceEntry< T extends DataServiceResource > extends DataServiceEntry< T > {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the resource (can be <code>null</code> if not yet assigned)
     * @throws KException
     *         if an error occurs
     */
    T getResource( final UnitOfWork uow ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the binary contents of this resource as an {@link InputStream} (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    default InputStream getContent( final UnitOfWork transaction ) throws KException {
        final T resource = getResource( transaction );

        if ( resource == null ) {
            return null;
        }

        return resource.getContent( transaction );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param resource
     *        the associated resource (can be <code>null</code> when removing current resource)
     * @throws KException
     *         if an error occurs
     */
    default void setResource( final UnitOfWork uow,
                              final T resource ) throws KException {
        setReference( uow, resource );
    }

}
