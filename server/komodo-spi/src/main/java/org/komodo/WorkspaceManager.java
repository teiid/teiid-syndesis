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

import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.datavirtualization.ViewDefinition;

public interface WorkspaceManager {
	
	String findSchema(String name);
	
	boolean deleteSchema(String name);
	
	void saveSchema(String name, String contents);
	

	DataVirtualization createDataservice(String serviceName);

	DataVirtualization findDataservice(String dataserviceName);

	public Iterable<? extends DataVirtualization> findDataservices();

	boolean deleteDataservice(String serviceName);
	
	
	ViewDefinition addViewDefiniton(  String viewDefinitionName);

    ViewDefinition[] getViewDefinitions(String viewDefinitionNamePrefix);
    
	boolean removeViewDefinition(final String viewDefinitionName);

	ViewDefinition getViewDefinition(String viewDefinitionName);

}
