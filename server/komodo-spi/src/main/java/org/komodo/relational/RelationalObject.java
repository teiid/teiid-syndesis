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
package org.komodo.relational;

import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;

public interface RelationalObject {
	
	/**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED}
     * @return the parent RelationalObject (can be <code>null</code> if at the Komodo root)
     * @throws KException
     *         if an error occurs
     */
	RelationalObject getRelationalParent( final UnitOfWork transaction ) throws KException;
	
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED}
     * @return the last segment of the absolute path (never empty)
     * @throws KException
     *         if an error occurs
     * @see #getAbsolutePath()
     */
	String getName(UnitOfWork transaction) throws KException;
	
	/**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name of property being requested (cannot be empty)
     * @return the property string or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if an error occurs
     */
    String getPropertyValue( final UnitOfWork transaction,
                          final String name ) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED}))
     * @param name
     *        the name of property being set (cannot be empty)
     * @param value
     *        the property string or <code>null</code> if the property doesn't exist
     * @throws KException
     *         if an error occurs
     */
    void setPropertyValue( final UnitOfWork transaction,
            final String name, String value ) throws KException;
	
}
