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
package org.komodo.relational.model.internal;

import org.komodo.core.repository.Property;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents a DDL statement option from a relational model.
 */
public interface StatementOption extends RelationalObject, Property {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.STATEMENT_OPTION;

    /**
     * The type identifier.
     */
    int TYPE_ID = StatementOption.class.hashCode();

    /**
     * @return the statement option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getOption() throws KException;

    /**
     * @param newOption
     *        the new value for the <code>statement option</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setOption(
                    final String newOption ) throws KException;

}
