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

import org.komodo.relational.dataservice.StateCommand;

public class StateCommandAggregate implements org.komodo.relational.dataservice.StateCommandAggregate {

	private StateCommand undo;
	private StateCommand redo;
	
	@Override
	public StateCommand setUndo(String commandId, Map<String, String> arguments) throws Exception {
		org.komodo.core.datavirtualization.StateCommand stateCommand = new org.komodo.core.datavirtualization.StateCommand(commandId, arguments);
		this.undo = stateCommand;
		return this.undo;
	}

	@Override
	public StateCommand setRedo(String commandId, Map<String, String> arguments) throws Exception {
		org.komodo.core.datavirtualization.StateCommand stateCommand = new org.komodo.core.datavirtualization.StateCommand(commandId, arguments);
		this.redo = stateCommand;
		return this.redo;
	}

	public StateCommand getUndo() {
		return undo;
	}

	public void setUndo(StateCommand undo) {
		this.undo = undo;
	}

	public StateCommand getRedo() {
		return redo;
	}

	public void setRedo(StateCommand redo) {
		this.redo = redo;
	}

}
