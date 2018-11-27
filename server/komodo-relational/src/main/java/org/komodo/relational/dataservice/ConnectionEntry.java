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

import org.komodo.relational.connection.Connection;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data service entry for a connection.
 */
public interface ConnectionEntry extends DataServiceEntry< Connection > {

    /**
     * The type identifier.
     */
    KomodoType IDENTIFIER = KomodoType.CONNECTION_ENTRY;

    /**
     * An empty collection of entries.
     */
    ConnectionEntry[] NO_ENTRIES = new ConnectionEntry[ 0 ];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the JNDI name (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getJndiName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param jndiName
     *        the value to use to set the JNDI name (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    void setJndiName( final UnitOfWork transaction,
                      final String jndiName ) throws KException;

}
