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

import org.komodo.WorkspaceManager;
import org.komodo.datavirtualization.SourceSchema;
import org.komodo.datavirtualization.ViewDefinition;
import org.springframework.beans.factory.annotation.Autowired;
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
	public String findSchema(String id) {
		SourceSchema schema = this.schemaRepository.findOne(id);
		if (schema != null) {
			return schema.getDdl();
		}
		return null;
	}

	@Override
	public void deleteSchema(String id) {
		this.schemaRepository.delete(id);
	}

	@Override
	public void createOrUpdateSchema(String id, String contents) {
		org.komodo.repository.SourceSchema schema = this.schemaRepository.findOne(id);
		if (schema != null) {
			if (!contents.equals(schema.getDdl())) {
				schema.setDdl(contents);
			}
		} else {
			schema = new org.komodo.repository.SourceSchema(id);
			this.schemaRepository.save(schema);
		}
	}

	@Override
	public DataVirtualization createDataVirtualization(String serviceName) {
		DataVirtualization dataservice = new DataVirtualization(serviceName);
		return this.dataVirtualizationRepository.save(dataservice);
	}

	@Override
	public DataVirtualization findDataVirtualization(String dataserviceName) {
		return this.dataVirtualizationRepository.findByName(dataserviceName);
	}

	@Override
	public Iterable<? extends DataVirtualization> findDataVirtualizations() {
		return this.dataVirtualizationRepository.findAll();
	}
	
	@Override
	public boolean deleteDataVirtualization(String serviceName) {
		org.komodo.repository.DataVirtualization dv = this.dataVirtualizationRepository.findByName(serviceName);
		if (dv == null) {
			return false;
		}
		this.dataVirtualizationRepository.delete(dv);
		this.dataVirtualizationRepository.flush();
		return true;
	}
	
	@Override
	public ViewDefinition createViewDefiniton(String viewName) {
		org.komodo.repository.ViewDefinition viewEditorState = new org.komodo.repository.ViewDefinition(viewName);
		return this.viewDefinitionRepository.save(viewEditorState);
	}
	
	@Override
	public ViewDefinition getViewDefinition(String name) {
		return this.viewDefinitionRepository.findByName(name);
	}
	
	@Override
	public ViewDefinition[] getViewDefinitions(String viewDefinitionNamePrefix) {
		return this.viewDefinitionRepository
				.findAllByNameStartsWith(viewDefinitionNamePrefix).toArray(new ViewDefinition[0]);
	}
	
	@Override
	public boolean deleteViewDefinition(String viewDefinitionName) {
		org.komodo.repository.ViewDefinition vd = this.viewDefinitionRepository.findByName(viewDefinitionName);
		if (vd == null) {
			return false;
		}
		this.viewDefinitionRepository.delete(vd);
		this.viewDefinitionRepository.flush();
		return true;
	}
	
}
