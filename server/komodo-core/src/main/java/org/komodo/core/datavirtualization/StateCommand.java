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
package org.komodo.core.datavirtualization;

import java.util.Map;

public class StateCommand implements org.komodo.relational.dataservice.StateCommand {

	private String id;
	private Map<String, String> arguments;
	
	public StateCommand() {
	}
	
	public StateCommand(String id, Map<String, String> arguments) {
		this.id = id;
		this.arguments = arguments;
	}
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public Map<String, String> getArguments() {
		return arguments;
	}
	public void setArguments(Map<String, String> arguments) {
		this.arguments = arguments;
	}

}
