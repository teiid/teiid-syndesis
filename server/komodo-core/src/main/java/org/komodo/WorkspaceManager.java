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

package org.komodo;

import java.util.List;

import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.datavirtualization.SourceSchema;
import org.komodo.datavirtualization.ViewDefinition;

public interface WorkspaceManager {

    public static class EntityNotFoundException extends Exception {

        /**
         *
         */
        private static final long serialVersionUID = -3995911719208421687L;

    }

    SourceSchema findSchemaBySourceId(String id);

    boolean deleteSchemaBySourceId(String id);

    SourceSchema createSchema(String id, String name, String contents);

    List<String> findAllSchemaNames();


    DataVirtualization createDataVirtualization(String virtualizationName);

    DataVirtualization findDataVirtualization(String virtualizationName);

    public Iterable<? extends DataVirtualization> findDataVirtualizations();

    boolean deleteDataVirtualization(String virtualizationName);

    List<String> findDataVirtualizationNames();


    ViewDefinition findViewDefinitionByNameIgnoreCase(String dvName, String viewName);

    ViewDefinition findViewDefinition(String id);

    boolean deleteViewDefinition(String id);

    ViewDefinition createViewDefiniton(String dvName, String viewName);

    List<String> findViewDefinitionsNames(String dvName);

    List<? extends ViewDefinition> findViewDefinitions(String dvName);

    List<ViewDefinition> saveAllViewDefinitions(
            Iterable<ViewDefinition> entities);

    boolean isNameInUse(String name);

}
