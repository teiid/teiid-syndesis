/*************************************************************************************
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
package org.komodo.spi.query.proc.wsdl;

import org.komodo.spi.outcome.Outcome;

/**
 *
 */
public interface WsdlAttributeInfo {

    /**
     * Get the column name for display in the UI. This removes any quotes for
     * aesthetic reasons. Use {@link #getSymbolName()} for retrieving the 
     * fully validated column name.
     * 
     * @return the column name sans quotes.
     */
    String getName();

    /**
     * Get the fully validated column name. This should be used in SQL string
     * generation.
     *
     * @return name the column name
     */
    String getSymbolName();

    /**
     * 
     * @return name the attribute alias
     */
    String getAlias();

    String getSignature();

    /**
     * 
     * @return outcome the <code>IOutcome</code> representing the validity of the data in this info object
     */
    Outcome getOutcome();

}
