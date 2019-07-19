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

import java.util.Properties;
import org.komodo.spi.KException;

/**
 * Indicates the object has a string representation that can be exported. In many cases this is an XML represention.
 */
public interface Exportable {

    String USE_TABS_PROP_KEY = "useTabs";
	String EXCLUDE_TABLE_CONSTRAINTS_KEY = "excludeTableConstraints";

	/**
     * @return the name of this exportable
     * @throws KException
     *         if an error occurs
     */
    String getName() throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param properties (can be <code>null</code> or empty)
     * @return a byte array of the current object state (never empty)
     * @throws KException
     *         if an error occurs
     */
    byte[] export(  Properties properties ) throws KException;

}
