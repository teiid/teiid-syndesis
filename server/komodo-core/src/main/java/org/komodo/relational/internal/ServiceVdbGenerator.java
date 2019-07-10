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
package org.komodo.relational.internal;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.internal.OptionContainerUtils;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.PathUtils;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * This class provides methods to generate data service vdbs containing a view model
 * and one or more source models
 * 
 * Each model is created via generating DDL and calling setModelDefinition() method that 
 * relies on komodo/modeshape framework to parse the DDL and construct the corresponding 
 * PHYSICAL and VIRTUAL relational models
 *
 */
public final class ServiceVdbGenerator implements TeiidSqlConstants.Tokens {
	
    private static final String SCHEMA_MODEL_NAME_PATTERN = "{0}schemamodel"; //$NON-NLS-1$
    private static final String SCHEMA_VDB_NAME_PATTERN = "{0}schemavdb"; //$NON-NLS-1$
    private static final char SQL_ESCAPE_CHAR = '\"';
    private static final char NEW_LINE = '\n';
    private static final String OPEN_SQUARE_BRACKET = "["; //$NON-NLS-1$
    private static final String CLOSE_SQUARE_BRACKET = "]"; //$NON-NLS-1$
    
    private static final String EQ_STR = "EQ"; //$NON-NLS-1$
    private static final String NE_STR = "NE"; //$NON-NLS-1$
    private static final String LT_STR = "LT"; //$NON-NLS-1$
    private static final String GT_STR = "GT"; //$NON-NLS-1$
    private static final String LE_STR = "LE"; //$NON-NLS-1$
    private static final String GE_STR = "GE"; //$NON-NLS-1$

    /**
     * Inner Join Type
     */
    public static final String JOIN_INNER = "INNER_JOIN"; //$NON-NLS-1$
    /**
     * Left Outer Join type
     */
    public static final String JOIN_LEFT_OUTER = "LEFT_OUTER_JOIN"; //$NON-NLS-1$
    /**
     * Right Outer Join type
     */
    public static final String JOIN_RIGHT_OUTER = "RIGHT_OUTER_JOIN"; //$NON-NLS-1$
    /**
     * Full Outer Join type
     */
    public static final String JOIN_FULL_OUTER = "FULL_OUTER_JOIN"; //$NON-NLS-1$
    
    /**
     * fqn table option key
     */
    String TABLE_OPTION_FQN = "teiid_rel:fqn"; //$NON-NLS-1$l
    
    private final WorkspaceManager wsManager;
    
    /**
     * Constructs a ServiceVdbGenerator instance
     *
     * @param wsManager
     *        the WorkspaceManager (cannot be <code>null</code> and must be started)
     */
    public ServiceVdbGenerator( final WorkspaceManager wsManager ) {
        this.wsManager = wsManager;
    }
    
    /**
     * This method creates a new service VDB given a list of editor states each containing a view definition.
     * 
     * All views will end up in a new view model added to the vdb
     * 1 or more source models will be generated and added to the vdb
     * 
     * @param uow
     * 		the transaction
     * @param serviceVdb the vdb
     * @param editorStates
     * 		the array of view editor states
     * @throws KException
     * 		if problem occurs
     */
    public void refreshServiceVdb(UnitOfWork uow, Vdb serviceVdb, ViewEditorState[] editorStates) throws KException {
        // Reset the viewModel using the valid ViewDefinition
        Model viewModel = getViewModel(uow, serviceVdb);
        // Keep track of unique list of sources needed
    	List<Table> allSourceTables = new ArrayList<Table>();
    	
        if ( editorStates.length > 0 ) {
        	// Generate new model DDL by appending all view DDLs
        	StringBuilder allViewDdl = new StringBuilder();

            for ( final ViewEditorState editorState : editorStates ) {
            	ViewDefinition viewDef = editorState.getViewDefinition(uow);
            	
            	// If the ViewDefinition is complete, generate view DDL from it and append
            	if( viewDef.isComplete(uow) ) {
            		TableInfo[] tableInfos = getSourceTableInfos(uow, viewDef);

            		// If the ViewDefinition is not user defined, regen the DDL
            		String viewDdl = null;
            		if(!viewDef.isUserDefined(uow)) {
                		viewDdl = getODataViewDdl(uow, viewDef, tableInfos);
                		viewDef.setDdl(uow, viewDdl);
            		} else {
            			viewDdl = viewDef.getDdl(uow);
            		}
            		allViewDdl.append(viewDdl).append(NEW_LINE);
   
            		// Add sources to list if not already present
            		for( TableInfo info : tableInfos) {
            			Table table = info.getTable();
            			if( !allSourceTables.contains(table) ) {
            				allSourceTables.add(table);
            			}
            		}
            	}
        	}
            // Set the generated DDL on the service VDB view model
            viewModel.setModelDefinition(uow, allViewDdl.toString());
        } else { 
        	viewModel.setModelDefinition(uow, ""); //$NON-NLS-1$
        }

        // remove all source models from current service VDB
        clearSourceModels(uow, serviceVdb);

        // Build a Mapping of the unique schemaModel to it's tables
        Map< Model, List<Table> > schemaTableMap = new HashMap<Model, List<Table>>();
        for ( Table tbl : allSourceTables ) {
        	Model schemaModel = tbl.getParent(uow);
        	// Map key doesnt yet exist for the model - add a new tables list
        	if (!schemaTableMap.containsKey(schemaModel)) {
        		List<Table> tbls = new ArrayList<Table>();
        		tbls.add(tbl);
        		schemaTableMap.put(schemaModel, tbls);
        	// Map key exists for the model - add table to existing list
        	} else {
        		schemaTableMap.get(schemaModel).add(tbl);
        	}
        }
        
        // Iterate each schemaModel, generating a source for it.
        for ( Model currentSchemaModel: schemaTableMap.keySet() ) {
        	// Iterate tables for this schema, generating DDL
        	StringBuilder sb = new StringBuilder();
        	int iTbls = 0;
        	for ( Table table: schemaTableMap.get(currentSchemaModel) ) {
    			byte[] ddlBytes = table.export(uow, new Properties());
    			
    			if (iTbls != 0) sb.append(NEW_LINE);
    			sb.append(new String(ddlBytes));
    			iTbls++;
        	}
        	
        	// Create a source model and set the DDL string via setModelDeinition(DDL)
            Model srcModel = serviceVdb.addModel(uow, currentSchemaModel.getName(uow));
            srcModel.setModelType(uow, Type.PHYSICAL);
            srcModel.setModelDefinition(uow, sb.toString());
        	
            // Add ModelSource based on currentSchemaModel ModelSource info
            ModelSource[] schemaModelSources = currentSchemaModel.getSources(uow);
            for( ModelSource srcModelSource : schemaModelSources) {
            	// create the ModelSource
	            ModelSource tgtModelSource = srcModel.addSource(uow, srcModelSource.getName(uow));
	            // set the jndi name and translator name
	            tgtModelSource.setJndiName(uow, srcModelSource.getJndiName(uow));
	            tgtModelSource.setTranslatorName(uow, srcModelSource.getTranslatorName(uow));
            }
        }
    }
    
    /*
     * Generates DDL for a view definition based on properties and supplied array of TableInfo from one or more sources
     */
    private String getODataViewDdl(UnitOfWork uow, ViewDefinition viewDef, TableInfo[] sourceTableInfos) throws KException {

    	// Need to construct DDL based on
    	//   * 1 or 2 source tables
    	//   * Join criteria in the form of left and right critieria column names
        
    	String viewName = viewDef.getViewName(uow);
    	
    	if (sourceTableInfos.length < 1) throw new KException("Error getting the ViewDefinition sources"); //$NON-NLS-1$
    	
        StringBuilder sb = new StringBuilder();
        
        // Generate the View DDL
        sb.append("CREATE VIEW "); //$NON-NLS-1$
        sb.append(escapeSQLName(viewName));
        sb.append(StringConstants.SPACE+StringConstants.OPEN_BRACKET);
        sb.append("RowId long PRIMARY KEY, "); //$NON-NLS-1$

        // Check for join and single or 2 source join
        // Disable join for Syndesis 7.4 DDL generation. 
        boolean isJoin = false; // sourceTableInfos.length > 1;
        boolean singleTable = true; // sourceTableInfos.length == 1;
        
        TableInfo lhTableInfo = sourceTableInfos[0];
        TableInfo rhTableInfo = null;
        if( isJoin ) {
        	rhTableInfo = sourceTableInfos[1];
        }
        // Need to create 2 lists of column info
        // 1) Projected symbol list including name + type
        // 2) FQN list for (SELECT xx, xxx, xxx ) clause
        // Note: need to filter out duplicate names from 2nd table (if join) 
        
        // So for the INNER JOIN we need to get create an ordered list of source table columns and types based on the 1 or 2 source tables

        List<String> srcTableColumnNameTypes = new ArrayList<String>();
        List<String> srcTableColumnNames = new ArrayList<String>();
        List<String> srcTableFqnColumnNames = new ArrayList<String>();
        
        // ------------------------------------------------------------------
        // Assemble all left and right source table columns, in order
        // Duplicate column names are omitted if found in right table
        // ------------------------------------------------------------------
        for( ColumnInfo info : lhTableInfo.getColumnInfos() ) {
        	if( !srcTableColumnNames.contains(info.getName())) {
        		srcTableColumnNames.add(info.getName());
        		srcTableColumnNameTypes.add(info.getNameAndType());
        		if(singleTable) {
        			srcTableFqnColumnNames.add(info.getName());
        		} else {
        			srcTableFqnColumnNames.add(info.getAliasedName());
        		}
        	}
        }
        
        // Add right table columns, if right table exists
        if( rhTableInfo != null ) {
        	for( ColumnInfo info : rhTableInfo.getColumnInfos() ) {
        		if( !srcTableColumnNames.contains(info.getName())) {
        			srcTableColumnNames.add(info.getName());
        			srcTableColumnNameTypes.add(info.getNameAndType());
        			srcTableFqnColumnNames.add(info.getAliasedName());
        		}
        	}
        }
        
        // ---------------------------------------------
        // Generate the View projected columns
        // ---------------------------------------------
        SqlProjectedColumn[] projectedColumns = viewDef.getProjectedColumns(uow);
        List<String> selectedProjColumnNames = new ArrayList<String>();
        
        // If "SELECT ALL" then include all of the source table columns
        if( projectedColumns.length == 1 && projectedColumns[0].getName(uow).equalsIgnoreCase("ALL")  ) { //$NON-NLS-1$
            for (int i = 0; i < srcTableColumnNameTypes.size(); i++) {
                // keep track of projected column names
                String colName = srcTableColumnNames.get(i);
                selectedProjColumnNames.add(colName);
                // append name and type
                sb.append(srcTableColumnNameTypes.get(i));
                if (i < srcTableColumnNameTypes.size()-1) {
                    sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
                }
            }
            
        // Not "SELECT ALL" - utilize the selected projected columns    
        } else {
            // Make list of only the selected columns
            List<SqlProjectedColumn> selectedProjColumns = new ArrayList<SqlProjectedColumn>();
            for (SqlProjectedColumn projCol: projectedColumns) {
                String colName = projCol.getName(uow);
                if(projCol.isSelected(uow) && !selectedProjColumnNames.contains(colName)) {
                    selectedProjColumns.add(projCol);
                    // keep track of projected column names
                    selectedProjColumnNames.add(colName);
                }
            }
            // generate selected column projection
            for (int i = 0; i < selectedProjColumns.size(); i++) {
                sb.append(selectedProjColumns.get(i).getName(uow)).append(StringConstants.SPACE).append(selectedProjColumns.get(i).getType(uow));
                if (i < selectedProjColumns.size()-1) {
                    sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
                }
            }
        }

        sb.append(") "); //$NON-NLS-1$
        sb.append(getTableAnnotation(viewDef.getDescription(uow)));
        sb.append("AS \nSELECT "); //$NON-NLS-1$
        sb.append("ROW_NUMBER() OVER (ORDER BY "); //$NON-NLS-1$
        sb.append(srcTableFqnColumnNames.get(0));
        sb.append("), "); //$NON-NLS-1$

        // Assemble list of source column names that should be included
        List<String> includedFqnColNames = new ArrayList<String>();
        for (int i = 0; i < srcTableFqnColumnNames.size(); i++) {
            String srcTableColName = srcTableColumnNames.get(i);
            if (selectedProjColumnNames.contains(srcTableColName)) {
                includedFqnColNames.add(srcTableFqnColumnNames.get(i));
            }
        }
        // Append column names
        for (int i = 0; i < includedFqnColNames.size(); i++) {
            sb.append(includedFqnColNames.get(i));
            if (i < includedFqnColNames.size()-1) {
                sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
            }
        }

        sb.append("\n"); //$NON-NLS-1$
        sb.append("FROM "); //$NON-NLS-1$
        
        // --------- JOIN ---------
        if( isJoin ) {
        	String lhTableName = lhTableInfo.getFQName() + " AS " + lhTableInfo.getAlias(); //$NON-NLS-1$
	        String rhTableName = rhTableInfo.getFQName() + " AS " + rhTableInfo.getAlias(); //$NON-NLS-1$

        	sb.append(lhTableName+StringConstants.SPACE);
        	
	        SqlComposition comp1 = viewDef.getSqlCompositions(uow)[0];
	        String joinType = comp1.getType(uow);


	        
	        if(JOIN_INNER.equals(joinType)) {
	            sb.append("\nINNER JOIN \n").append(rhTableName+StringConstants.SPACE); //$NON-NLS-1$
	        } else if(JOIN_LEFT_OUTER.equals(joinType)) {
	            sb.append("\nLEFT OUTER JOIN \n").append(rhTableName+StringConstants.SPACE); //$NON-NLS-1$
	        } else if(JOIN_RIGHT_OUTER.equals(joinType)) {
	            sb.append("\nRIGHT OUTER JOIN \n").append(rhTableName+StringConstants.SPACE); //$NON-NLS-1$
	        } else if(JOIN_FULL_OUTER.equals(joinType)) {
	            sb.append("\nFULL OUTER JOIN \n").append(rhTableName+StringConstants.SPACE); //$NON-NLS-1$
	        } else {
	            sb.append("\nINNER JOIN \n").append(rhTableName+StringConstants.SPACE); //$NON-NLS-1$
	        }
	        
	        sb.append("\nON \n"); //$NON-NLS-1$
	        
            String lhColumn = comp1.getLeftCriteriaColumn(uow);
            String rhColumn = comp1.getRightCriteriaColumn(uow);
            String operator = getOperator(uow, comp1);

        	sb.append(lhTableInfo.getAlias()+StringConstants.DOT).append(lhColumn)
              .append(StringConstants.SPACE+operator+StringConstants.SPACE)
              .append(rhTableInfo.getAlias()+StringConstants.DOT).append(rhColumn);
        	
        	sb.append(StringConstants.SEMI_COLON);
        // --------- Single Source ---------
        } else {
            sb.append(lhTableInfo.getFQName()).append(StringConstants.SEMI_COLON);
        }

        return sb.toString();
    }
    
    /**
     * Public method to generate the view DDL for a view definition
     * 
     * @param uow
     * 		the transaction
     * @param viewDef 
     * 		the view definition
     * @return the View DDL
     * @throws KException
     * 		if problem occurs
     */
    public String getODataViewDdl(UnitOfWork uow, ViewDefinition viewDef) throws KException {
    	TableInfo[] tableInfos = getSourceTableInfos(uow, viewDef);
    	if (tableInfos.length < 1) throw new KException("Error getting the ViewDefinition sources"); //$NON-NLS-1$
    	
    	return getODataViewDdl(uow, viewDef, tableInfos);
    }

    /**
     * Generate the table annotation for the supplied description
     * @param description the description
     * @return the table annotation
     */
    private String getTableAnnotation(final String description) {
    	if( description!=null && description.length()>0 ) {
    		return "OPTIONS (ANNOTATION '" + description + "') ";
    	}
    	return "";
    }
    
    /**
     * Method returns the view model for a service VDB
     * 
     * @param uow
     * 		the transaction
     * @param serviceVdb
     *		the service VDB
     * @return the view model
     * @throws KException
     *  	if problem occurs
     */
    public static Model getViewModel(final UnitOfWork uow, Vdb serviceVdb) throws KException {
    	for( Model model : serviceVdb.getModels(uow) ) {
    		if( model.getModelType(uow) == Type.VIRTUAL ) {
    			return model;
    		}
    	}
    	return null;
    }

    /*
     * Find and resolve source {@link TableInfo}s for a {@link ViewDefinition} object
     * @param uow
     * @param viewDefinition
     * @return {@link TableInfo} array
     * @throws KException
     */
	private TableInfo[] getSourceTableInfos(UnitOfWork uow, ViewDefinition viewDefinition) throws KException {
		if ( !viewDefinition.isComplete(uow) ) {
			return new TableInfo[0];
		}
		String[] sourceTablePaths = viewDefinition.getSourcePaths(uow);
		ArrayList<TableInfo> sourceTableInfos = new ArrayList<TableInfo>(sourceTablePaths.length);

		// Find and create TableInfo for each source Path
		int iTable = 0;
		for(String path : sourceTablePaths) {
			String connectionName = PathUtils.getOption(path, "connection"); //$NON-NLS-1$

			// Find schema model based on the connection name (i.e. connection=pgConn)
			final Model schemaModel = findSchemaModel( uow, connectionName );

			// Get the tables from the schema and match them with the table name
			if ( schemaModel != null ) {
			    final Table[] tables = schemaModel.getTables( uow );
			    String tableOption = PathUtils.getTableOption(path);

			    // Look thru schema tables for table with matching option.
			    for (Table table: tables) {
			        final String option = OptionContainerUtils.getOption( uow, table, TABLE_OPTION_FQN );
			        if( option != null ) {
			            // If table found, create the TableInfo and break
			            if( option.equals(tableOption) ) {
			                String alias = (iTable == 0) ? "A" : "B"; //$NON-NLS-1$ //$NON-NLS-2$
			                // create a new TableInfo object
			                sourceTableInfos.add(new TableInfo(uow, path, table, alias));
			                iTable++;
			                break;
			            }
			        }
			    }
			}
		}

		return sourceTableInfos.toArray(new TableInfo[0]);
	}
	
    
	/*
	 * Simple method that removes all source models from a VDB
	 */
    private void clearSourceModels(final UnitOfWork uow, Vdb serviceVdb) throws KException {
    	List<Model> modelsToRemove = new ArrayList<Model>();
    	
    	for( Model model : serviceVdb.getModels(uow) ) {
    		if( model.getModelType(uow) == Type.PHYSICAL ) {
    			modelsToRemove.add(model);
    		}
    	}
    	
    	for( Model model : modelsToRemove) {
    		model.remove(uow);
    	}
    }

    /*
     * constructs a simple column string for a view definition
     * 
     * i.e. "CUSTOMER_ID integer"
     */
    private String getColumnDatatypeString(UnitOfWork uow, Column col) throws KException {
    	String typeName = col.getDatatypeName(uow);
    	
    	// Determine if array type
        if (col.hasRawProperty(uow, StandardDdlLexicon.DATATYPE_ARRAY_DIMENSIONS)) {
            Property colArrDimsProp = col.getRawProperty(uow, StandardDdlLexicon.DATATYPE_ARRAY_DIMENSIONS);
            long colArrDims = colArrDimsProp != null ? colArrDimsProp.getLongValue(uow) : -1;

            for (long dims = colArrDims; dims > 0; dims--) {
                typeName = typeName.concat(OPEN_SQUARE_BRACKET).concat(CLOSE_SQUARE_BRACKET);
            }
        }
    
        return typeName;
    }

    /*
     * returns the string value for the operator key from a SqlComposition object
     */
    private String getOperator(UnitOfWork uow, SqlComposition sqlComposition) throws KException {
    	String type = sqlComposition.getOperator(uow);
        if( EQ_STR.equals(type)) {
            return EQ;
        } else if( LT_STR.equals(type)) {
            return LT;
        } else if( GT_STR.equals(type)) {
        	return GT;
        } else if( NE_STR.equals(type)) {
        	return NE;
        } else if( LE_STR.equals(type)) {
        	return LE;
        } else if( GE_STR.equals(type)) {
        	return GE;
        }
        
        return null;
    }
    
    /*
     * checks a sql statement word or segment and escapes the string if neccessary based
     * on included special characters
     */
    private String escapeSQLName(String part) {
        if (TeiidSqlConstants.isReservedWord(part)) {
            return SQL_ESCAPE_CHAR + part + SQL_ESCAPE_CHAR;
        }
        boolean escape = true;
        char start = part.charAt(0);
        if (start == '#' || start == '@' || StringUtils.isLetter(start)) {
            escape = false;
            for (int i = 1; !escape && i < part.length(); i++) {
                char c = part.charAt(i);
                escape = !StringUtils.isLetterOrDigit(c) && c != '_';
            }
        }
        if (escape) {
            return SQL_ESCAPE_CHAR + part + SQL_ESCAPE_CHAR;
        }
        return part;
    }

    /*
     * Finds a schema model for a given connectionName from the workspace manager
     */
    private Model findSchemaModel(final UnitOfWork uow, final String connectionName) throws KException {
		final Vdb vdb = findSchemaVdb(uow, connectionName);

		if (vdb != null) {
			final String schemaModelName = getSchemaModelName(connectionName);
			final Model[] models = vdb.getModels(uow, schemaModelName);

			if (models.length != 0) {
				return models[0];
			}
		}

		return null;
	}
    
    /*
     * Finds a schema VDB for a given connection from the workspace manager
     */
    private Vdb findSchemaVdb(final UnitOfWork uow, final String connectionName) throws KException {
		final String schemaVdbName = getSchemaVdbName(connectionName);
		
		return this.wsManager.findVdb(uow, schemaVdbName);
	}

    private static String getSchemaVdbName(final String connectionName) {
		return MessageFormat.format(SCHEMA_VDB_NAME_PATTERN, connectionName.toLowerCase());
	}

    private static String getSchemaModelName(final String connectionName) {
		return MessageFormat.format(SCHEMA_MODEL_NAME_PATTERN, connectionName.toLowerCase());
	}

    /*
     * Inner class to hold state for source table information and simplifies the DDL generating process
     */
    class TableInfo {
		final private String path;
    	final private String alias;
    	final private Table table;

    	private String name;
    	private String fqname;

		private List<ColumnInfo> columnInfos = new ArrayList<ColumnInfo>();
    	
    	private TableInfo(UnitOfWork uow, String path, Table table, String alias) throws KException {
    		this.path = path;
    		this.alias = alias;
    		this.table = table;
    		this.name = escapeSQLName(table.getName(uow));
    		Model schemaModel = table.getParent(uow);
    		this.fqname = escapeSQLName(schemaModel.getName(uow)) + DOT + this.name;
    		createColumnInfos(uow, table);
    	}
    	
    	private void createColumnInfos(UnitOfWork uow, Table table) throws KException {
    		// Walk through the columns and create an array of column + datatype strings
    		Column[] cols = table.getColumns(uow);
    		
    		for( Column col : cols) {
    			this.columnInfos.add(new ColumnInfo(uow, col, getFQName(), this.alias));
    		}
    	}
    	
    	public String getConnectionName() {
			return PathUtils.getOption(this.path, "connection"); //$NON-NLS-1$
		}

    	public String getSourceTablePath() {
			return this.path;
		}

		public String getName() {
			return this.name;
		}
		
		public Table getTable() {
			return this.table;
		}
		
		public String getAlias() {
			return this.alias;
		}
		
		public String getFQName() {
			return fqname;
		}

		public List<ColumnInfo> getColumnInfos() {
			return columnInfos;
		}
    }
    
    /*
     * Inner class to hold state for table column information and simplifies the DDL generating process
     */
    class ColumnInfo {
    	private String name;
    	private String fqname;
    	private String aliasedName;
    	private String nameAndType;
    	
    	private ColumnInfo(UnitOfWork uow, Column column, String tableFqn, String tblAlias) throws KException {
			this.name = escapeSQLName(column.getName(uow));
			this.nameAndType =  name + SPACE + getColumnDatatypeString(uow, column);
			this.aliasedName = name;
			if( tblAlias != null ) {
				this.aliasedName = tblAlias + DOT + name;
			}
			this.fqname = tableFqn + DOT + name;
    	}
    	
		public String getName() {
			return this.name;
		}
		
		public String getAliasedName() {
			return this.aliasedName;
		}

		public String getFqname() {
			return this.fqname;
		}

		public String getNameAndType() {
			return this.nameAndType;
		}
    }
}