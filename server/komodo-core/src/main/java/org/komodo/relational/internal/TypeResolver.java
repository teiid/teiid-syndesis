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
package org.komodo.relational.internal;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * A class that determines if a {@link KomodoObject} can be converted into a strong typed relational object.
 *
 * @param <T>
 *        the {@link RelationalObject} subclass
 */
public interface TypeResolver< T extends RelationalObject > {

    /**
     * @return the identifier associated with this resolver (never <code>null</code>)
     */
    KomodoType identifier();

    /**
     * @return the class this type resolver pertains to (never <code>null</code>)
     */
    Class< ? extends KomodoObject > owningClass();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param kobject
     *        the {@link KomodoObject} being resolved (cannot be <code>null</code>)
     * @return <code>true</code> if object can be resolved to this resolver's type
     * @throws KException
     *         if an error occurs
     */
    boolean resolvable( final UnitOfWork transaction,
                        final KomodoObject kobject ) throws KException;

    /**
     * Converts the specified {@link KomodoObject} to this resolver's strong typed relational object. It is assumed that the
     * object has been {@link #resolvable(UnitOfWork, KomodoObject) resolved}.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if the operation should be automatically committed)
     * @param kobject
     *        the {@link KomodoObject} being resolved (cannot be <code>null</code>)
     * @return the strong typed {@link RelationalObject} (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see #resolvable(UnitOfWork, KomodoObject)
     */
    T resolve( final UnitOfWork transaction,
               final KomodoObject kobject ) throws KException;

}
