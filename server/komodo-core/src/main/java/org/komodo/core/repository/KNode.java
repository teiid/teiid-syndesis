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
package org.komodo.core.repository;

import org.komodo.core.internal.repository.Repository;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;

/**
 * A Komodo object.
 */
public interface KNode {

    /**
     * @return the {@link KomodoObject Komodo object's} absolute path (never empty)
     */
    String getAbsolutePath();

    /**
     * @return the last segment of the absolute path (never empty)
     * @throws KException
     *         if an error occurs
     * @see #getAbsolutePath()
     */
    String getName( ) throws KException;

    /**
     * @return the parent {@link KomodoObject Komodo object} (can be <code>null</code> if at the Komodo root)
     * @throws KException
     *         if an error occurs
     */
    KomodoObject getParent( ) throws KException;
    
    Repository getRepository();
    
    UnitOfWork getTransaction();

}
