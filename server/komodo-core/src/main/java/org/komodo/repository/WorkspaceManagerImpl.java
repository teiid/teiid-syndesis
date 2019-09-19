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

package org.komodo.repository;

import java.util.List;

import org.komodo.WorkspaceManager;
import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.datavirtualization.SourceSchema;
import org.komodo.datavirtualization.ViewDefinition;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.stereotype.Component;

@Component
public class WorkspaceManagerImpl implements WorkspaceManager {

    @Autowired
    private DataVirtualizationRepository dataVirtualizationRepository;
    @Autowired
    private SourceSchemaRepository schemaRepository;
    @Autowired
    private ViewDefinitionRepository viewDefinitionRepository;

    @Override
    public org.komodo.datavirtualization.SourceSchema findSchemaBySourceId(String id) {
        return this.schemaRepository.findBySourceId(id);
    }

    @Override
    public boolean deleteSchemaBySourceId(String sourceid) {
        try {
            if (this.schemaRepository.deleteBySourceId(sourceid) == 0) {
                return false;
            }
            return true;
        } catch (EmptyResultDataAccessException e) {
            return false;
        }
    }

    @Override
    public SourceSchema createSchema(String sourceId, String name, String contents) {
        SourceSchema schema = new SourceSchema();
        schema.setSourceId(sourceId);
        schema.setName(name);
        schema.setDdl(contents);
        return this.schemaRepository.save(schema);
    }

    @Override
    public List<String> findAllSchemaNames() {
        return dataVirtualizationRepository.findNamesByTypeLike("s"); //$NON-NLS-1$
    }

    @Override
    public boolean isNameInUse(String name) {
        return dataVirtualizationRepository.countByUpperName(name.toUpperCase()) > 0;
    }

    @Override
    public DataVirtualization createDataVirtualization(String virtualizationName) {
        DataVirtualization dataservice = new DataVirtualization(virtualizationName);
        return this.dataVirtualizationRepository.save(dataservice);
    }

    @Override
    public DataVirtualization findDataVirtualization(String virtualizationName) {
        return this.dataVirtualizationRepository.findByName(virtualizationName);
    }

    @Override
    public Iterable<? extends DataVirtualization> findDataVirtualizations() {
        return this.dataVirtualizationRepository.findAll();
    }

    @Override
    public List<String> findDataVirtualizationNames() {
        return dataVirtualizationRepository.findNamesByTypeLike("v"); //$NON-NLS-1$
    }

    @Override
    public boolean deleteDataVirtualization(String serviceName) {
        org.komodo.datavirtualization.DataVirtualization dv = this.dataVirtualizationRepository.findByName(serviceName);
        if (dv == null) {
            return false;
        }
        this.dataVirtualizationRepository.delete(dv);
        this.dataVirtualizationRepository.flush();
        return true;
    }

    @Override
    public boolean updateDataVirtualization(String virtualizationName, String sourceId) {
        org.komodo.datavirtualization.DataVirtualization dv = this.dataVirtualizationRepository
                .findByName(virtualizationName);
        if (dv == null) {
            return false;
        }
        dv.setSourceId(sourceId);
        this.dataVirtualizationRepository.save(dv);
        return true;
    }

    @Override
    public List<ViewDefinition> saveAllViewDefinitions(Iterable<ViewDefinition> entities) {
        return this.viewDefinitionRepository.saveAll(entities);
    }

    @Override
    public org.komodo.datavirtualization.ViewDefinition createViewDefiniton(String dvName, String viewName) {
        org.komodo.datavirtualization.ViewDefinition viewEditorState = new org.komodo.datavirtualization.ViewDefinition(dvName, viewName);
        return this.viewDefinitionRepository.save(viewEditorState);
    }

    @Override
    public List<String> findViewDefinitionsNames(String dvName) {
        return this.viewDefinitionRepository.findAllNamesByDataVirtualizationName(dvName);
    }

    @Override
    public List<org.komodo.datavirtualization.ViewDefinition> findViewDefinitions(String dvName) {
        return this.viewDefinitionRepository.findAllByDataVirtualizationName(dvName);
    }

    @Override
    public boolean deleteViewDefinition(String id) {
        try {
            this.viewDefinitionRepository.deleteById(id);
            this.viewDefinitionRepository.flush();
            return true;
        } catch (EmptyResultDataAccessException e) {
            return false;
        }
    }

    @Override
    public org.komodo.datavirtualization.ViewDefinition findViewDefinition(String id) {
        return this.viewDefinitionRepository.findById(id).orElse(null);
    }

    @Override
    public ViewDefinition findViewDefinitionByNameIgnoreCase(String dvName, String viewDefinitionName) {
        return this.viewDefinitionRepository.findByNameIgnoreCase(dvName, viewDefinitionName);
    }

    public void flush() {
        this.viewDefinitionRepository.flush();
    }
}
