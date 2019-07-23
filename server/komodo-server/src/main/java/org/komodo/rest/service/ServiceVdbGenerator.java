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
package org.komodo.rest.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.komodo.relational.WorkspaceManager;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.TeiidSqlConstants;
import org.komodo.utils.PathUtils;
import org.komodo.utils.StringUtils;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.adminapi.impl.VDBMetaData;

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
     * @param vdbName 
     * 
     * @param uow
     * 		the transaction
     * @param serviceVdb the vdb
     * @param editorStates
     * 		the array of view editor states
     * @throws KException
     * 		if problem occurs
     */
    public VDBMetaData refreshServiceVdb(String vdbName, ViewEditorState[] editorStates) throws KException {
        VDBMetaData vdb = new VDBMetaData();
        vdb.setName(vdbName);
        
    	// Reset the viewModel using the valid ViewDefinition
        ModelMetaData model = new ModelMetaData();
        model.setName(SERVICE_VDB_VIEW_MODEL);
        model.setModelType(org.teiid.adminapi.Model.Type.VIRTUAL);
    	vdb.addModel(model); 
         
        // Keep track of unique list of sources needed
    	List<Table> allSourceTables = new ArrayList<Table>();
    	
        if ( editorStates.length > 0 ) {
        	// Generate new model DDL by appending all view DDLs
        	StringBuilder allViewDdl = new StringBuilder();

            for ( final ViewEditorState editorState : editorStates ) {
            	ViewDefinition viewDef = editorState.getViewDefinition();
            	
            	// If the ViewDefinition is complete, generate view DDL from it and append
            	if( viewDef.isComplete() ) {
            		TableInfo[] tableInfos = getSourceTableInfos(viewDef);

            		// If the ViewDefinition is not user defined, regen the DDL
            		String viewDdl = null;
            		if(!viewDef.isUserDefined()) {
                		viewDdl = getODataViewDdl(viewDef, tableInfos);
                		viewDef.setDdl(viewDdl);
            		} else {
            			viewDdl = viewDef.getDdl();
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
            model.addSourceMetadata("DDL", allViewDdl.toString());
        }

        // Build a Mapping of the unique schemaModel to it's tables
        Map< Model, List<Table> > schemaTableMap = new HashMap<Model, List<Table>>();
        for ( Table tbl : allSourceTables ) {
        	Model schemaModel = tbl.getRelationalParent();
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
    			byte[] ddlBytes = table.export(new Properties());
    			
    			if (iTbls != 0) sb.append(NEW_LINE);
    			sb.append(new String(ddlBytes));
    			iTbls++;
        	}
        	
        	// Create a source model and set the DDL string via setModelDeinition(DDL)
        	ModelMetaData srcModel = new ModelMetaData();
        	srcModel.setName(currentSchemaModel.getName());
        	vdb.addModel(srcModel);
            srcModel.setModelType(org.teiid.adminapi.Model.Type.PHYSICAL);
            srcModel.addSourceMetadata("DDL", sb.toString());
        	
            // Add ModelSource based on currentSchemaModel ModelSource info
            ModelSource[] schemaModelSources = currentSchemaModel.getSources();
            for( ModelSource srcModelSource : schemaModelSources) {
            	// add the source mapping
	            srcModel.addSourceMapping(srcModelSource.getName(), srcModelSource.getTranslatorName(), srcModelSource.getJndiName());
            }
        }
        
        return vdb;
    }
    
    /*
     * Generates DDL for a view definition based on properties and supplied array of TableInfo from one or more sources
     */
    private String getODataViewDdl(ViewDefinition viewDef, TableInfo[] sourceTableInfos) throws KException {

    	// Need to construct DDL based on
    	//   * 1 or 2 source tables
    	//   * Join criteria in the form of left and right critieria column names
        
    	String viewName = viewDef.getViewName();
    	
    	if (sourceTableInfos.length < 1) throw new KException("Error getting the ViewDefinition sources"); //$NON-NLS-1$
    	
        StringBuilder sb = new StringBuilder();
        
        // Generate the View DDL
        sb.append("CREATE VIEW "); //$NON-NLS-1$
        sb.append(escapeSQLName(viewName));
        sb.append(StringConstants.SPACE+StringConstants.OPEN_BRACKET);

        // Check for join and single or 2 source join
        // Disable join for Syndesis 7.4 DDL generation. 
        boolean isJoin = sourceTableInfos.length > 1;
        boolean singleTable = sourceTableInfos.length == 1;
        
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

        LinkedHashMap<String, ColumnInfo> columns = new LinkedHashMap<>();
        
        // ------------------------------------------------------------------
        // Assemble all left and right source table columns, in order
        // Duplicate column names are omitted if found in right table
        // ------------------------------------------------------------------
        for( ColumnInfo info : lhTableInfo.getColumnInfos() ) {
    		columns.put(info.getName(), info);
        }
        
        // Add right table columns, if right table exists
        if( rhTableInfo != null ) {
        	for( ColumnInfo info : rhTableInfo.getColumnInfos() ) {
        		if( !columns.containsKey(info.getName())) {
        			columns.put(info.getName(), info);
        		}
        	}
        }
        
        // ---------------------------------------------
        // Generate the View projected columns
        // ---------------------------------------------
        SqlProjectedColumn[] projectedColumns = viewDef.getProjectedColumns();
        Set<String> selectedProjColumnNames = new LinkedHashSet<String>();
        
        // If "SELECT ALL" then include all of the source table columns
        if( projectedColumns.length == 1 && projectedColumns[0].getName().equalsIgnoreCase("ALL")  ) { //$NON-NLS-1$
        	for (Iterator<ColumnInfo> iter = columns.values().iterator(); iter.hasNext();) {
        		ColumnInfo info = iter.next();
                // keep track of projected column names
                String colName = info.getName();
                selectedProjColumnNames.add(colName);
                // append name and type
                sb.append(info.getNameAndType());
                if (iter.hasNext()) {
                    sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
                }
            }
            
        // Not "SELECT ALL" - utilize the selected projected columns    
        } else {
            // Make list of only the selected columns
            List<SqlProjectedColumn> selectedProjColumns = new ArrayList<SqlProjectedColumn>();
            for (SqlProjectedColumn projCol: projectedColumns) {
                String colName = projCol.getName();
                if(projCol.isSelected() && !selectedProjColumnNames.contains(colName)) {
                    selectedProjColumns.add(projCol);
                    // keep track of projected column names
                    selectedProjColumnNames.add(colName);
                }
            }
            // generate selected column projection
            for (int i = 0; i < selectedProjColumns.size(); i++) {
                sb.append(selectedProjColumns.get(i).getName()).append(StringConstants.SPACE).append(selectedProjColumns.get(i).getType());
                if (i < selectedProjColumns.size()-1) {
                    sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
                }
            }
        }
        
        if( singleTable ) {
        	TableConstraint constraint = lhTableInfo.getUniqueConstraint();
        	if (constraint != null) {
        		boolean usingKey = true;
        		Column[] keyCols = constraint.getColumns();
				for (Column c : keyCols) {
        			if (!selectedProjColumnNames.contains(c.getName())) {
        				usingKey = false;
        				break;
        			}
        		}
        		if (usingKey) {
        			sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
        			sb.append(constraint.getConstraintType().toValue()).append(StringConstants.OPEN_BRACKET);
        			for (int i = 0; i < keyCols.length; i++) {
        				if (i > 0) {
                            sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
                        }
        				sb.append(keyCols[i].getName());
            		}
        			sb.append(StringConstants.CLOSE_BRACKET);
        		}
        	}
        } else {
        	//TODO: needs analysis of key preservation
        }

        sb.append(") "); //$NON-NLS-1$
        sb.append(getTableAnnotation(viewDef.getDescription()));
        sb.append("AS \nSELECT "); //$NON-NLS-1$
        
        // Append column names
        for (Iterator<String> iter = selectedProjColumnNames.iterator(); iter.hasNext();) {
        	ColumnInfo col = columns.get(iter.next());
        	if (col == null) {
        		continue;
        	}
            sb.append(col.getAliasedName());
            if (iter.hasNext()) {
                sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
            }
        }

        sb.append("\n"); //$NON-NLS-1$
        sb.append("FROM "); //$NON-NLS-1$
        
        // --------- JOIN ---------
         
        if( isJoin ) {
        	String lhTableName = lhTableInfo.getFQName() + " AS " + lhTableInfo.getAlias(); //$NON-NLS-1$
        	sb.append(lhTableName).append(StringConstants.SEMI_COLON);
        	// Disable join for Syndesis 7.4 DDL generation.
	        /*String rhTableName = rhTableInfo.getFQName() + " AS " + rhTableInfo.getAlias(); //$NON-NLS-1$

        	sb.append(lhTableName+StringConstants.SPACE);
        	
	        SqlComposition comp1 = viewDef.getSqlCompositions()[0];
	        String joinType = comp1.getType();


	        
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
	        
            String lhColumn = comp1.getLeftCriteriaColumn();
            String rhColumn = comp1.getRightCriteriaColumn();
            String operator = getOperator(comp1);

        	sb.append(lhTableInfo.getAlias()+StringConstants.DOT).append(lhColumn)
              .append(StringConstants.SPACE+operator+StringConstants.SPACE)
              .append(rhTableInfo.getAlias()+StringConstants.DOT).append(rhColumn);
        	
        	sb.append(StringConstants.SEMI_COLON);*/
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
    public String getODataViewDdl(ViewDefinition viewDef) throws KException {
    	TableInfo[] tableInfos = getSourceTableInfos(viewDef);
    	if (tableInfos.length < 1) throw new KException("Error getting the ViewDefinition sources"); //$NON-NLS-1$
    	
    	return getODataViewDdl(viewDef, tableInfos);
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
    
    /*
     * Find and resolve source {@link TableInfo}s for a {@link ViewDefinition} object
     * @param uow
     * @param viewDefinition
     * @return {@link TableInfo} array
     * @throws KException
     */
	private TableInfo[] getSourceTableInfos(ViewDefinition viewDefinition) throws KException {
		if ( !viewDefinition.isComplete() ) {
			return new TableInfo[0];
		}
		String[] sourceTablePaths = viewDefinition.getSourcePaths();
		ArrayList<TableInfo> sourceTableInfos = new ArrayList<TableInfo>(sourceTablePaths.length);

		// Find and create TableInfo for each source Path
		int iTable = 0;
		for(String path : sourceTablePaths) {
			String connectionName = PathUtils.getOption(path, "connection"); //$NON-NLS-1$

			// Find schema model based on the connection name (i.e. connection=pgConn)
			final Model schemaModel = findSchemaModel( connectionName );

			// Get the tables from the schema and match them with the table name
			if ( schemaModel != null ) {
			    final Table[] tables = schemaModel.getTables( );
			    String tableOption = PathUtils.getTableOption(path);

			    // Look thru schema tables for table with matching option.
			    for (Table table: tables) {
			        final String option = table.getPropertyValue( TABLE_OPTION_FQN );
			        if( option != null ) {
			            // If table found, create the TableInfo and break
			            if( option.equals(tableOption) ) {
			                String alias = (iTable == 0) ? "A" : "B"; //$NON-NLS-1$ //$NON-NLS-2$
			                // create a new TableInfo object
			                sourceTableInfos.add(new TableInfo(path, table, sourceTablePaths.length>1?alias:null));
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
     * constructs a simple column string for a view definition
     * 
     * i.e. "CUSTOMER_ID integer"
     */
    private String getColumnDatatypeString(Column col) throws KException {
    	String typeName = col.getDatatypeName();
    	
    	Long colArrDims = col.getArrayDimensions();
    	
    	// Determine if array type
        if (colArrDims != null) {
            for (long dims = colArrDims; dims > 0; dims--) {
                typeName = typeName.concat(OPEN_SQUARE_BRACKET).concat(CLOSE_SQUARE_BRACKET);
            }
        }
    
        return typeName;
    }

    /*
     * returns the string value for the operator key from a SqlComposition object
     */
    private String getOperator(SqlComposition sqlComposition) throws KException {
    	String type = sqlComposition.getOperator();
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
    private Model findSchemaModel(final String connectionName) throws KException {
		final Vdb vdb = findSchemaVdb(connectionName);

		if (vdb != null) {
			final String schemaModelName = KomodoMetadataService.getSchemaModelName(connectionName);
			final Model[] models = vdb.getModels(schemaModelName);

			if (models.length != 0) {
				return models[0];
			}
		}

		return null;
	}
    
    /*
     * Finds a schema VDB for a given connection from the workspace manager
     */
    private Vdb findSchemaVdb(final String connectionName) throws KException {
		final String schemaVdbName = KomodoMetadataService.getSchemaVdbName(connectionName);
		
		return this.wsManager.findVdb(schemaVdbName);
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
		
		private TableConstraint constraint;
    	
    	private TableInfo(String path, Table table, String alias) throws KException {
    		this.path = path;
    		this.alias = alias;
    		this.table = table;
    		this.name = escapeSQLName(table.getName());
    		Model schemaModel = table.getRelationalParent();
    		this.fqname = escapeSQLName(schemaModel.getName()) + DOT + this.name;
    		createColumnInfos(table);
    		constraint = table.getPrimaryKey();
    		if (constraint == null) {
    			UniqueConstraint[] unique = table.getUniqueConstraints();
    			if (unique.length > 0) {
    				constraint = unique[0];
    			}
    		}
    	}
    	
    	private void createColumnInfos(Table table) throws KException {
    		// Walk through the columns and create an array of column + datatype strings
    		Column[] cols = table.getColumns();
    		for( Column col : cols) {
    			this.columnInfos.add(new ColumnInfo(col, getFQName(), this.alias));
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
		
		public TableConstraint getUniqueConstraint() {
			return constraint;
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
    	
    	private ColumnInfo(Column column, String tableFqn, String tblAlias) throws KException {
			this.name = escapeSQLName(column.getName());
			this.nameAndType =  name + SPACE + getColumnDatatypeString(column);
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