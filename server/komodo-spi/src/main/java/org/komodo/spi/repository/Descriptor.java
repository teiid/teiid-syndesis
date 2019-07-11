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
package org.komodo.spi.repository;

import org.komodo.spi.KException;

/**
 * A {@link KomodoObject Komodo object} type definition.
 */
public interface Descriptor {

    /**
     * @return the property factory
     */
    KPropertyFactory getPropertyFactory();

    /**
     * @return the type name (never empty)
     */
    String getName();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the {@link KomodoObject Komodo object's} {@link Property property} {@link PropertyDescriptor descriptors} (never
     *         <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException;
}
