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

import java.util.List;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;

/**
 * Represents the configuration of a view editor state
 */
public interface ViewEditorState extends RelationalObject, StringConstants {
	
    /**
     * @return the new command
     */
    StateCommandAggregate addCommand();

    /**
     * @return the state commands
     */
    List<StateCommandAggregate> getCommands();
        
    /**
     * @return the view definition
     */
    ViewDefinition getViewDefinition() throws KException;
}
