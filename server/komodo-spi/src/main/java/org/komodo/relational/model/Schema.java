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
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;

/**
 * A schema fragment
 */
public interface Schema extends RelationalObject, Exportable {

    /**
     * The type identifier.
     */
    int TYPE_ID = Schema.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.SCHEMA;

    /**
     * An empty array of schemas.
     */
    Schema[] NO_SCHEMAS = new Schema[0];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return rendered content of this schema, eg. ddl string
     * @throws KException if error occurs
     */
    String getRendition(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param rendition rendered content of this schema, eg. ddl string
     * @throws KException if error occurs
     */
    void setRendition(UnitOfWork transaction, String rendition) throws KException;

}
