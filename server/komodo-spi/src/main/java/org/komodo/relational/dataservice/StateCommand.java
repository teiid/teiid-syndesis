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

import java.util.Map;

import org.komodo.spi.KException;

/**
 * Represents the configuration of a view editor state command
 */
public interface StateCommand {

    /**
     * @return the id of the command
     * @throws KException
     *          if an error occurs
     */
    String getId();

    /**
     * @param id the id of the command
     * @throws Exception
     *          if an error occurs
     */
    void setId( String id) throws Exception;

    /**
     * @return the arguments of the command
     * @throws KException
     *         if an error occurs
     */
    Map<String, String> getArguments();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param arguments
     *        the new map of arguments
     * @throws KException
     *         if an error occurs
     */
    void setArguments( final Map<String, String> arguments);
}
