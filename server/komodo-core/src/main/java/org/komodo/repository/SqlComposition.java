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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize(as = SqlComposition.class)
@JsonInclude(Include.NON_NULL)
public class SqlComposition implements org.komodo.datavirtualization.SqlComposition {
	
	private String name;
	private String description;
	private String leftSourcePath;
	private String leftCriteriaColumn;
	private String rightSourcePath;
	private String rightCriteriaColumn;
	private String operator;
	private String type;
	
	protected SqlComposition() {
		
	}
	
	public SqlComposition(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	public String getLeftSourcePath() {
		return leftSourcePath;
	}
	public void setLeftSourcePath(String leftSourcePath) {
		this.leftSourcePath = leftSourcePath;
	}
	public String getLeftCriteriaColumn() {
		return leftCriteriaColumn;
	}
	public void setLeftCriteriaColumn(String leftCriteriaColumn) {
		this.leftCriteriaColumn = leftCriteriaColumn;
	}
	public String getRightSourcePath() {
		return rightSourcePath;
	}
	public void setRightSourcePath(String rightSourcePath) {
		this.rightSourcePath = rightSourcePath;
	}
	public String getRightCriteriaColumn() {
		return rightCriteriaColumn;
	}
	public void setRightCriteriaColumn(String rightCriteriaColumn) {
		this.rightCriteriaColumn = rightCriteriaColumn;
	}
	public String getOperator() {
		return operator;
	}
	public void setOperator(String operator) {
		this.operator = operator;
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type=type;
	}

}
