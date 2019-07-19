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

import java.util.Collections;
import java.util.Map;

import org.komodo.core.repository.KomodoObject;
import org.komodo.relational.model.internal.StatementOption;
import org.komodo.spi.KException;

/**
 * Indicates the implementing class may have {@link StatementOption options}.
 */
public interface OptionContainer extends KomodoObject {

    /**
     * @return the user-defined and any other non-standard statement options (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    StatementOption[] getCustomOptions( ) throws KException;

    /**
     * @return the names of the standard options and their default value (never <code>null</code> but can be empty)
     */
    default Map< String, String > getStandardOptions() {
        return Collections.emptyMap();
    }

    /**
     * This result includes both the standard statement options and any custom options that have been set.
     *
     * @return the statement option names for this object (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getStatementOptionNames( ) throws KException;

    /**
     * @return the statement options that have been set (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    StatementOption[] getStatementOptions( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the name of the option being checked (cannot be empty)
     * @return <code>true</code> if the custom option exists
     * @throws KException
     *         if an error occurs
     */
    boolean isCustomOption(
                            final String name ) throws KException;

    /**
     * A standard option is a statement option that is built-in/well known.
     *
     * @param name
     *        the name of the option being checked (cannot be empty)
     * @return <code>true</code> if a standard option
     */
    boolean isStandardOption( final String name );

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param optionToRemove
     *        the name of the statement option being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeStatementOption(
                                final String optionToRemove ) throws KException;

    /**
     * @param optionName
     *        the name of the statement option being added (cannot be empty)
     * @param optionValue
     *        the statement option value (can be empty if removing the option)
     * @return the statement option (<code>null</code> if removed)
     * @throws KException
     *         if an error occurs
     */
    StatementOption setStatementOption( final String optionName,
                                        final String optionValue ) throws KException;

}
