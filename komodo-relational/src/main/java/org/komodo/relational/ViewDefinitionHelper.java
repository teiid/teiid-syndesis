/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.internal.OptionContainerUtils;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;
import org.komodo.spi.lexicon.ddl.StandardDdlLexicon;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.KLog;
import org.komodo.utils.PathUtils;

/**
 * This class provides methods to create data service vdbs containing a view model
 * and one or more source models
 * 
 * Each model is created via generating DDL and calling setModelDefinition() method that 
 * relies on komodo/modeshape framework to parse the DDL and construct the corresponding 
 * PHYSICAL and VIRTUAL relational models
 *
 */
public final class ViewDefinitionHelper implements TeiidSqlConstants.Tokens {
	
	protected static final KLog LOGGER = KLog.getLogger();
    protected static final String SCHEMA_MODEL_NAME_PATTERN = "{0}schemamodel"; //$NON-NLS-1$
    protected static final String SCHEMA_VDB_NAME_PATTERN = "{0}schemavdb"; //$NON-NLS-1$
    private static final char SQL_ESCAPE_CHAR = '\"'; //$NON-NLS-1$
    private static final char NEW_LINE = '\n'; //$NON-NLS-1$
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
    String TABLE_OPTION_FQN = "teiid_rel:fqn"; //$NON-NLS-1$l "teiid_relfqn"; //
    
    protected final WorkspaceManager wsManager;
    
    /**
     * Constructs a Komodo service.
     *
     * @param wsManager
     *        the WorkspaceManager (cannot be <code>null</code> and must be started)
     */
    public ViewDefinitionHelper( final WorkspaceManager wsManager ) {
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
     * @param editorStates
     * 		the array of view editor states
     * @return
     * 		nothing
     * @throws KException
     * 		if problem occurs
     */
    public void refreshServiceVdb(UnitOfWork uow, Vdb serviceVdb, ViewEditorState[] editorStates) throws KException {
        // Reset the viewModel using the valid ViewDefinition
        Model viewModel = getViewModel(uow, serviceVdb);
    	List<TableInfo> allSourceTableInfos = new ArrayList<TableInfo>();
    	
        if ( editorStates.length > 0 ) {
        	// Generate new model DDL by appending all view DDLs
        	StringBuilder allViewDdl = new StringBuilder();

            for ( final ViewEditorState editorState : editorStates ) {
            	ViewDefinition viewDef = editorState.getViewDefinition(uow);
            	
            	// If the ViewDefinition is complete, generate view DDL from it and append
            	if( viewDef.isComplete(uow) ) {
                	TableInfo[] tableInfos = getSourceTableInfos(uow, viewDef);

            		String viewDdl = getODataViewDdl(uow, viewDef, tableInfos);
            		allViewDdl.append(viewDdl).append(NEW_LINE); //$NON-NLS-1$
            		
            		// Load the running copy/leftover list
            		for( TableInfo info : tableInfos) {
            			allSourceTableInfos.add(info);
            		}
            		
            		// Check the table info objects and compile a list of all source table infos
            		for( TableInfo info : tableInfos) {
            			boolean exists = false;
            			for( TableInfo nextSrcTableInfo : allSourceTableInfos) {
            				if( nextSrcTableInfo.getTable().equals(info.getTable()) )  {
            					exists = true;
            					break;
            				}
            			}
            			
            			if(!exists) {
            				allSourceTableInfos.add(info);
            			}
            		}
            	}
        	}
            // Set the generated DDL on the service VDB view model
            viewModel.setModelDefinition(uow, allViewDdl.toString());
        } else { 
        	viewModel.setModelDefinition(uow, "");
        }
        
        // remove all source models from VDB
        clearSourceModels(uow, serviceVdb);

        // Now that we have all source TableInfo objects
        // find TableInfo's that share a common parent (schema)
        // Each TableInfo will have a Table KomodoObject. the table.getParent() will be the schema model, and hence schema model name
        
        List<TableInfo> remainingTableInfos = new ArrayList<TableInfo>();
        remainingTableInfos.addAll(allSourceTableInfos);
        
        // Starting with all table infos, process each one and process table info's for common schema source models
        // After each schema model, re-set remaining table infos and repeat until no remaining table infos
        
        while( !remainingTableInfos.isEmpty() ) {
        	boolean first = true;
        	StringBuilder sb = new StringBuilder();
        	Model currentSchemaModel = null;
        	List<TableInfo> leftOvers = new ArrayList<TableInfo>();
        	
        	// For each source model, generate DDL via Table.export() method and store in StingBuilder
        	
        	for( TableInfo nextTableInfo : remainingTableInfos ) {
        		if( first ) {
        			currentSchemaModel = nextTableInfo.getTable().getParent(uow);
        			byte[] ddlBytes = nextTableInfo.getTable().export(uow, new Properties());
        			sb.append(new String(ddlBytes));
        			first = false;
        		} else {
        			if( nextTableInfo.getTable().getParent(uow).equals(currentSchemaModel) ) {
        				byte[] ddlBytes = nextTableInfo.getTable().export(uow, new Properties());
        				sb.append(NEW_LINE).append(new String(ddlBytes));
        			} else {
        				leftOvers.add(nextTableInfo);
        			}
        		}
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

            // Clear the remaining table infos and reset with any leftovers
            
        	remainingTableInfos.clear();
        	remainingTableInfos.addAll(leftOvers);
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
    	
    	if (sourceTableInfos.length < 1) throw new KException("Error getting the ViewDefinition sources");
    	
        StringBuilder sb = new StringBuilder();
        
        // Generate the View DDL
        sb.append("CREATE VIEW "); //$NON-NLS-1$
        sb.append(viewName);
        sb.append(StringConstants.SPACE+StringConstants.OPEN_BRACKET);
        sb.append("RowId integer PRIMARY KEY, "); //$NON-NLS-1$

        // Check for join and single or 2 source join
        boolean isJoin = sourceTableInfos.length > 1;
        boolean singleSource = true;
        
        TableInfo lhTableInfo = sourceTableInfos[0];
        TableInfo rhTableInfo = null;
        if( isJoin ) {
        	rhTableInfo = sourceTableInfos[1];
        	singleSource =  lhTableInfo.getConnectionName().equals(rhTableInfo.getConnectionName());
        }
        
        // Need to create 2 lists of column info
        // 1) Projected symbol list including name + type
        // 2) FQN list for (SELECT xx, xxx, xxx ) clause
        // Note: need to filter out duplicate names from 2nd table (if join) 
        
        // So for the INNER JOIN we need to get create an ordered list of projected columns and types based on the 1 or 2 source tables

        List<String> projSymbols = new ArrayList<String>();
        List<String> colNames = new ArrayList<String>();
        List<String> fqnColNames = new ArrayList<String>();
        
        // Need to loop through actual columns to maintain order
        for( ColumnInfo info : lhTableInfo.getColumnInfos() ) {
        	if( !colNames.contains(info.getName())) {
        		colNames.add(info.getName());
        		projSymbols.add(info.getNameAndType());
        		if(singleSource) {
        			fqnColNames.add(info.getName());
        		} else {
        			fqnColNames.add(info.getAliasedName());
        		}
        	}
        }
        
        if( rhTableInfo != null ) {
        	for( ColumnInfo info : rhTableInfo.getColumnInfos() ) {
        		if( !colNames.contains(info.getName())) {
        			colNames.add(info.getName());
        			projSymbols.add(info.getNameAndType());
        			if(singleSource) {
        				fqnColNames.add(info.getName());
        			} else {
        				fqnColNames.add(info.getAliasedName());
        			}
        		}
        	}
        }
        
        for (int i = 0; i < projSymbols.size(); i++) {
            sb.append(projSymbols.get(i));
            if (i < projSymbols.size()-1) {
                sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
            }
        }
        
        sb.append(") AS \n"); //$NON-NLS-1$
        sb.append("SELECT "); //$NON-NLS-1$
        sb.append("ROW_NUMBER() OVER (ORDER BY "); //$NON-NLS-1$
        sb.append(fqnColNames.get(0));
        sb.append("), "); //$NON-NLS-1$
        
        for (int i = 0; i < fqnColNames.size(); i++) {
            sb.append(fqnColNames.get(i));
            if (i < fqnColNames.size()-1) {
                sb.append(StringConstants.COMMA).append(StringConstants.SPACE);
            }
        }

        sb.append("\n"); //$NON-NLS-1$
        sb.append("FROM "); //$NON-NLS-1$
        
        // --------- JOIN ---------
        if( isJoin ) {
        	String lhTableName = lhTableInfo.getFQName() + " AS " + lhTableInfo.getAlias();
	        String rhTableName = rhTableInfo.getFQName() + " AS " + rhTableInfo.getAlias();
        	if( singleSource ) {
        		lhTableName = lhTableInfo.getFQName();
    	        rhTableName = rhTableInfo.getFQName();
        	} 
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
            
            // single source = dont use table alias, otherwise alias the table
            String lhTblName = singleSource ? lhTableName : lhTableInfo.getAlias();
            String rhTblName = singleSource ? rhTableName : rhTableInfo.getAlias();
        	sb.append(lhTblName+StringConstants.DOT).append(lhColumn)
              .append(StringConstants.SPACE+operator+StringConstants.SPACE)
              .append(rhTblName+StringConstants.DOT).append(rhColumn);
        	
        	sb.append(StringConstants.SEMI_COLON);
        // --------- Single Source ---------
        } else {
            sb.append(lhTableInfo.getFQName()).append(StringConstants.SEMI_COLON);
        }

        return sb.toString();
    }
    
    /**
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
    	if (tableInfos.length < 1) throw new KException("Error getting the ViewDefinition sources");
    	
    	return getODataViewDdl(uow, viewDef, tableInfos);
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
			String connectionName = PathUtils.getOption(path, "connection");
			Connection connection = findConnection(uow, this.wsManager, connectionName);

			if (connection != null) {
				// Find Table objects in Komodo based on the connection name (i.e.    connection=pgConn
				final Model schemaModel = findSchemaModel( uow, this.wsManager, connection );

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
								String alias = (iTable == 0) ? "A" : "B";
								// create a new TableInfo object
								sourceTableInfos.add(new TableInfo(uow, path, table, alias));
								iTable++;
								break;
							}
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

    private static String getColumnDatatypeString(UnitOfWork uow, Column col) throws KException {
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
    
    private static String escapeSQLName(String part) {
        if (TeiidSqlConstants.isReservedWord(part)) {
            return SQL_ESCAPE_CHAR + part + SQL_ESCAPE_CHAR;
        }
        boolean escape = true;
        char start = part.charAt(0);
        if (start == '#' || start == '@' || isLetter(start)) {
            escape = false;
            for (int i = 1; !escape && i < part.length(); i++) {
                char c = part.charAt(i);
                escape = !isLetterOrDigit(c) && c != '_';
            }
        }
        if (escape) {
            return SQL_ESCAPE_CHAR + part + SQL_ESCAPE_CHAR;
        }
        return part;
    }

    private static boolean isLetter(char c) {
        return isBasicLatinLetter(c) || Character.isLetter(c);
    }

    private static boolean isLetterOrDigit(char c) {
        return isBasicLatinLetter(c) || isBasicLatinDigit(c) || Character.isLetterOrDigit(c);
    }

    private static boolean isBasicLatinLetter(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    private static boolean isBasicLatinDigit(char c) {
        return c >= '0' && c <= '9';
    }
	
    protected static Dataservice findDataservice(UnitOfWork uow, final WorkspaceManager wkspMgr, String dataserviceName) throws KException {
        if (! wkspMgr.hasChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE ) ) {
            return null;
        }

        final KomodoObject kobject = wkspMgr.getChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
        final Dataservice dataservice = wkspMgr.resolve( uow, kobject, Dataservice.class );

        LOGGER.debug( "Dataservice '{0}' was found", dataserviceName ); //$NON-NLS-1$
        return dataservice;
    }

    protected static Connection findConnection(UnitOfWork uow, final WorkspaceManager wkspMgr, String connectionName) throws KException {
        if (! wkspMgr.hasChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE ) ) {
            return null;
        }

        final KomodoObject kobject = wkspMgr.getChild( uow, connectionName, DataVirtLexicon.Connection.NODE_TYPE );
        final Connection connection = wkspMgr.resolve( uow, kobject, Connection.class );

        LOGGER.debug( "Connection '{0}' was found", connectionName ); //$NON-NLS-1$
        return connection;
    }
    
    protected static Model findSchemaModel(final UnitOfWork uow, final WorkspaceManager manager, final Connection connection) throws KException {
		final Vdb vdb = findSchemaVdb(uow, manager, connection);

		if (vdb != null) {
			final String connectionName = connection.getName(uow);
			final String schemaModelName = getSchemaModelName(connectionName);
			final Model[] models = vdb.getModels(uow, schemaModelName);

			if (models.length != 0) {
				return models[0];
			}
		}

		return null;
	}

    protected static Vdb findSchemaVdb(final UnitOfWork uow, final WorkspaceManager wkspMgr, final Connection connection) throws KException {
		final String connectionName = connection.getName(uow);

		final String schemaVdbName = getSchemaVdbName(connectionName);
		final KomodoObject[] vdbs = connection.getChildrenOfType(uow, VdbLexicon.Vdb.VIRTUAL_DATABASE, schemaVdbName);

		if (vdbs.length == 0) {
			return null;
		}

		return wkspMgr.resolve(uow, vdbs[0], Vdb.class);
	}

    protected static String getSchemaVdbName(final String connectionName) {
		return MessageFormat.format(SCHEMA_VDB_NAME_PATTERN, connectionName.toLowerCase());
	}

    protected static String getSchemaModelName(final String connectionName) {
		return MessageFormat.format(SCHEMA_MODEL_NAME_PATTERN, connectionName.toLowerCase());
	}
    
    public static Model getViewModel(final UnitOfWork uow, Vdb serviceVdb) throws KException {
    	for( Model model : serviceVdb.getModels(uow) ) {
    		if( model.getModelType(uow) == Type.VIRTUAL ) {
    			return model;
    		}
    	}
    	return null;
    }

    protected static Profile getUserProfile(final UnitOfWork transaction, final WorkspaceManager wkspMgr) throws KException {
    	Repository repo = wkspMgr.getRepository();
        KomodoObject userProfileObj = repo.komodoProfile(transaction);
        Profile userProfile = wkspMgr.resolve(transaction, userProfileObj, Profile.class);
        if (userProfile == null) {
            String msg = Messages.getString(Messages.Relational.NO_USER_PROFILE, transaction.getUserName());
            throw new KException(msg);
        }

        return userProfile;
    }
    
    class TableInfo {
		final private String path;
    	final private String alias;
    	final private Table table;

    	private String name;
    	private String fqname;

		private List<ColumnInfo> columnInfos = new ArrayList<ColumnInfo>();
    	
    	protected TableInfo(UnitOfWork uow, String path, Table table, String alias) throws KException {
    		this.path = path;
    		this.alias = alias;
    		this.table = table;
    		this.name = table.getName(uow);
    		Model schemaModel = table.getParent(uow);
    		this.fqname = schemaModel.getName(uow) + DOT + this.name;
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
			return PathUtils.getOption(this.path, "connection");
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
    
    class ColumnInfo {
    	private String name;
    	private String fqname;
    	private String aliasedName;
    	private String nameAndType;
    	
    	protected ColumnInfo(UnitOfWork uow, Column column, String tableFqn, String tblAlias) throws KException {
			this.name = column.getName(uow);
			this.nameAndType =  escapeSQLName(name) + SPACE + getColumnDatatypeString(uow, column);
			this.aliasedName = name;
			if( tblAlias != null ) {
				this.aliasedName = tblAlias + DOT + name;
			}
			this.fqname = tableFqn + DOT + escapeSQLName(name);
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