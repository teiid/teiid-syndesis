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
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Pattern;

import org.komodo.metadata.runtime.TeiidDataSource;
import org.komodo.relational.dataservice.SqlComposition;
import org.komodo.relational.dataservice.SqlProjectedColumn;
import org.komodo.relational.dataservice.ViewDefinition;
import org.komodo.relational.dataservice.ViewEditorState;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.TeiidSqlConstants;
import org.komodo.utils.PathUtils;
import org.teiid.adminapi.Admin.SchemaObjectType;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.language.SQLConstants;
import org.teiid.metadata.Column;
import org.teiid.metadata.KeyRecord;
import org.teiid.metadata.KeyRecord.Type;
import org.teiid.metadata.Schema;
import org.teiid.metadata.Table;
import org.teiid.query.metadata.DDLConstants;
import org.teiid.query.metadata.DDLStringVisitor;
import org.teiid.query.sql.visitor.SQLStringVisitor;

/**
 * This class provides methods to generate data service vdbs containing a view model
 * and one or more source models
 * 
 * Each model is created via generating DDL and calling setModelDefinition() method
 *
 */
public final class ServiceVdbGenerator implements TeiidSqlConstants.Tokens {
	
	public interface SchemaFinder {
		Schema findSchema(String connectionName) throws KException;

		TeiidDataSource findTeiidDatasource(String connectionName) throws KException;
	}
	
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
    
    private final SchemaFinder finder;
    
    /**
     * Constructs a ServiceVdbGenerator instance
     *
     * @param finder
     */
    public ServiceVdbGenerator( final SchemaFinder finder ) {
        this.finder = finder;
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
    	Map< Schema, LinkedHashSet<TableInfo> > schemaTableMap = new HashMap<Schema, LinkedHashSet<TableInfo>>();
        
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
            			Table tbl = info.getTable();
            			Schema schemaModel = tbl.getParent();
            			LinkedHashSet<TableInfo> tbls = schemaTableMap.get(schemaModel);
            			if (!schemaTableMap.containsKey(schemaModel)) {
                        	// Map key doesnt yet exist for the model - add a new tables list
                    		tbls = new LinkedHashSet<TableInfo>();
                    		schemaTableMap.put(schemaModel, tbls);
                    	}
                    	tbls.add(info);
            		}
            	}
        	}
            // Set the generated DDL on the service VDB view model
            model.addSourceMetadata("DDL", allViewDdl.toString());
        }

        
        // Iterate each schemaModel, generating a source for it.
        for ( Entry<Schema, LinkedHashSet<TableInfo>> entry: schemaTableMap.entrySet() ) {
        	// Iterate tables for this schema, generating DDL
        	String connectionName = null;

        	StringBuilder regex = new StringBuilder();
        	for ( TableInfo table: entry.getValue() ) {
    			connectionName = table.getConnectionName();
    			if (regex.length() > 0) {
    				regex.append("|");
    			}
    			regex.append(Pattern.quote(table.getName()));
        	}
        	
        	String ddl = DDLStringVisitor.getDDLString(entry.getKey(), EnumSet.of(SchemaObjectType.TABLES), regex.toString());
        	
        	// Create a source model and set the DDL string via setModelDeinition(DDL)
        	ModelMetaData srcModel = new ModelMetaData();
        	srcModel.setName(entry.getKey().getName());
        	vdb.addModel(srcModel);
            srcModel.setModelType(org.teiid.adminapi.Model.Type.PHYSICAL);
            srcModel.addSourceMetadata("DDL", ddl);
        	
            // Add ModelSource based on currentSchemaModel ModelSource info
            TeiidDataSource tds = finder.findTeiidDatasource(connectionName);
            
            if (tds != null) {
	        	// add the source mapping
	            srcModel.addSourceMapping(tds.getName(), tds.getType(), tds.getJndiName());
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
        sb.append(SQLStringVisitor.escapeSinglePart(viewName));
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
        List<SqlProjectedColumn> projectedColumns = viewDef.getProjectedColumns();
        Set<String> selectedProjColumnNames = new LinkedHashSet<String>();
        
        // If "SELECT ALL" then include all of the source table columns
        if( projectedColumns.size() == 1 && projectedColumns.get(0).getName().equalsIgnoreCase("ALL")  ) { //$NON-NLS-1$
        	for (Iterator<ColumnInfo> iter = columns.values().iterator(); iter.hasNext();) {
        		ColumnInfo info = iter.next();
                // keep track of projected column names
                String colName = info.getName();
                selectedProjColumnNames.add(colName);
                // append name and type
                sb.append(info.getName());
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
        	KeyRecord constraint = lhTableInfo.getUniqueConstraint();
        	if (constraint != null) {
        		boolean usingKey = true;
        		List<Column> keyCols = constraint.getColumns();
				for (Column c : keyCols) {
        			if (!selectedProjColumnNames.contains(c.getName())) {
        				usingKey = false;
        				break;
        			}
        		}
        		if (usingKey) {
        			sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
        			sb.append(constraint.getType()==Type.Primary?DDLConstants.PRIMARY_KEY:SQLConstants.Reserved.UNIQUE).append(StringConstants.OPEN_BRACKET);
        			for (int i = 0; i < keyCols.size(); i++) {
        				if (i > 0) {
                            sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
                        }
        				sb.append(keyCols.get(i).getName());
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
		List<String> sourceTablePaths = viewDefinition.getSourcePaths();
		ArrayList<TableInfo> sourceTableInfos = new ArrayList<TableInfo>(sourceTablePaths.size());

		// Find and create TableInfo for each source Path
		int iTable = 0;
		for(String path : sourceTablePaths) {
			String connectionName = PathUtils.getOption(path, "connection"); //$NON-NLS-1$

			// Find schema model based on the connection name (i.e. connection=pgConn)
			final Schema schemaModel = findSchemaModel( connectionName );

			// Get the tables from the schema and match them with the table name
			if ( schemaModel != null ) {
			    final Collection<Table> tables = schemaModel.getTables().values();
			    String tableOption = PathUtils.getTableOption(path);

			    // Look thru schema tables for table with matching option.
			    for (Table table: tables) {
			        final String option = table.getProperty( KomodoMetadataService.TABLE_OPTION_FQN, false );
			        if( option != null ) {
			            // If table found, create the TableInfo and break
			            if( option.equals(tableOption) ) {
			                String alias = (iTable == 0) ? "A" : "B"; //$NON-NLS-1$ //$NON-NLS-2$
			                // create a new TableInfo object
			                sourceTableInfos.add(new TableInfo(path, table, sourceTablePaths.size()>1?alias:null));
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
     * Finds a schema model for a given connectionName from the workspace manager
     */
    private Schema findSchemaModel(final String connectionName) throws KException {
		return finder.findSchema(connectionName);
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
		
		private KeyRecord constraint;
    	
    	private TableInfo(String path, Table table, String alias) throws KException {
    		this.path = path;
    		this.alias = alias;
    		this.table = table;
    		this.name = SQLStringVisitor.escapeSinglePart(table.getName());
    		Schema schemaModel = table.getParent();
    		this.fqname = SQLStringVisitor.escapeSinglePart(schemaModel.getName()) + DOT + this.name;
    		createColumnInfos(table);
    		constraint = table.getPrimaryKey();
    		if (constraint == null) {
    			List<KeyRecord> unique = table.getUniqueKeys();
    			if (!unique.isEmpty()) {
    				constraint = unique.get(0);
    			}
    		}
    	}
    	
    	private void createColumnInfos(Table table) throws KException {
    		// Walk through the columns and create an array of column + datatype strings
    		List<Column> cols = table.getColumns();
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
		
		public KeyRecord getUniqueConstraint() {
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
    	
    	private ColumnInfo(Column column, String tableFqn, String tblAlias) throws KException {
			this.name = SQLStringVisitor.escapeSinglePart(column.getName());
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
    }
}