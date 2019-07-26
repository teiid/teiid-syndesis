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
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.data.domain.ExampleMatcher.StringMatcher;
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
	public String findSchema(String name) {
		SourceSchema schema = this.schemaRepository.findByName(name);
		if (schema != null) {
			return schema.getDdl();
		}
		return null;
	}

	@Override
	public boolean deleteSchema(String name) {
		return this.schemaRepository.deleteByName(name) > 0;
	}

	@Override
	public void saveSchema(String name, String contents) {
		org.komodo.repository.SourceSchema schema = new org.komodo.repository.SourceSchema(name);
		this.schemaRepository.save(schema);
	}

	@Override
	public DataVirtualization createDataservice(String serviceName) {
		DataVirtualization dataservice = new DataVirtualization(serviceName);
		return this.dataVirtualizationRepository.save(dataservice);
	}

	@Override
	public DataVirtualization findDataservice(String dataserviceName) {
		return this.dataVirtualizationRepository.findByName(dataserviceName);
	}

	@Override
	public Iterable<? extends DataVirtualization> findDataservices() {
		return this.dataVirtualizationRepository.findAll();
	}
	
	@Override
	public boolean deleteDataservice(String serviceName) {
		return this.dataVirtualizationRepository.deleteByName(serviceName) > 0;
	}
	
	@Override
	public ViewDefinition addViewDefiniton(String viewName) {
		org.komodo.repository.ViewDefinition viewEditorState = new org.komodo.repository.ViewDefinition(viewName);
		return this.viewDefinitionRepository.save(viewEditorState);
	}
	
	@Override
	public ViewDefinition getViewDefinition(String name) {
		return this.viewDefinitionRepository.findByName(name);
	}
	
	@Override
	public ViewDefinition[] getViewDefinitions(String viewDefinitionNamePrefix) {
		ExampleMatcher exampleMatcher = ExampleMatcher.matching().withStringMatcher(StringMatcher.STARTING);
		return this.viewDefinitionRepository
				.findAll(
						Example.of(new org.komodo.repository.ViewDefinition(viewDefinitionNamePrefix), exampleMatcher))
				.toArray(new ViewDefinition[0]);
	}
	
	@Override
	public boolean removeViewDefinition(String viewDefinitionName) {
		return this.viewDefinitionRepository.deleteByName(viewDefinitionName) > 1;
	}
	
}
