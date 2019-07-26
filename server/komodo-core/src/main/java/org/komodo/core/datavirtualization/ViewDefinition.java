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

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

import org.komodo.relational.dataservice.SqlComposition;
import org.komodo.relational.dataservice.SqlProjectedColumn;;

/**
 * Represents the configuration of a view editor state
 */
@Entity
public class ViewDefinition implements org.komodo.relational.dataservice.ViewDefinition {
	
	public static class State {
		private List<SqlComposition> sqlCompositions = new ArrayList<>(1);
		private List<SqlProjectedColumn> projectedColumns = new ArrayList<>(1);
		private List<String> sourcePaths = new ArrayList<>(1);
		
		public List<SqlComposition> getSqlCompositions() {
			return sqlCompositions;
		}
		public void setSqlCompositions(List<SqlComposition> sqlCompositions) {
			this.sqlCompositions = sqlCompositions;
		}
		public List<SqlProjectedColumn> getProjectedColumns() {
			return projectedColumns;
		}
		public void setProjectedColumns(List<SqlProjectedColumn> projectedColumns) {
			this.projectedColumns = projectedColumns;
		}
		public List<String> getSourcePaths() {
			return sourcePaths;
		}
		public void setSourcePaths(List<String> sourcePaths) {
			this.sourcePaths = sourcePaths;
		}
	}
	
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private String id;
	@Column(unique=true)
	private String name;
	private String ddl;
	
	private String viewName;
	private String description;
	private boolean complete;
	private boolean userDefined;
	
	@Convert(converter = JpaConverterJson.class)
	private State state = new State();
	
	public ViewDefinition(String name) {
		this.name = name;
	}

	public String getId() {
		return id;
	}

	@Override
	public String getName() {
		return name;
	}
	
	@Override
	public SqlComposition addSqlComposition(String compositionName) {
		org.komodo.core.datavirtualization.SqlComposition sqlComposition = new org.komodo.core.datavirtualization.SqlComposition(compositionName);
		state.sqlCompositions.add(sqlComposition);
		return sqlComposition;
	}

	@Override
	public List<SqlComposition> getSqlCompositions() {
		return state.sqlCompositions;
	}

	@Override
	public String getViewName() {
		return this.viewName;
	}

	@Override
	public void setViewName(String name) {
		this.viewName = name;
	}

	@Override
	public String getDescription() {
		return this.description;
	}

	@Override
	public void setDescription(String description) {
		this.description = description;
	}

	@Override
	public String getDdl() {
		return this.ddl;
	}

	@Override
	public void setDdl(String ddl) {
		this.ddl = ddl;
	}

	@Override
	public void setComplete(boolean complete) {
		this.complete = complete;
	}

	@Override
	public boolean isComplete() {
		return this.complete;
	}

	@Override
	public void setUserDefined(boolean userDefined) {
		this.userDefined = userDefined;
	}

	@Override
	public boolean isUserDefined() {
		return this.userDefined;
	}

	@Override
	public List<String> getSourcePaths() {
		return state.sourcePaths;
	}

	@Override
	public void addSourcePath(String sourcePath) {
		this.getSourcePaths().add(sourcePath);
	}

	@Override
	public SqlProjectedColumn addProjectedColumn(String columnName) {
		org.komodo.core.datavirtualization.SqlProjectedColumn sqlProjectedColumn = new org.komodo.core.datavirtualization.SqlProjectedColumn(columnName);
		this.getProjectedColumns().add(sqlProjectedColumn);
		return sqlProjectedColumn;
	}

	@Override
	public List<SqlProjectedColumn> getProjectedColumns() {
		return state.projectedColumns;
	}
	
	public void setSqlCompositions(List<SqlComposition> sqlCompositions) {
		state.sqlCompositions = sqlCompositions;
	}
	
	public void setSourcePaths(List<String> sourcePaths) {
		state.sourcePaths = sourcePaths;
	}

}
