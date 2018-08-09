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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.internal.OptionContainerUtils;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.ViewDefinition;
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

public final class ViewDefinitionHelper implements TeiidSqlConstants.Tokens {
	
	protected static final KLog LOGGER = KLog.getLogger();
    protected static final String SCHEMA_MODEL_NAME_PATTERN = "{0}schemamodel"; //$NON-NLS-1$
    protected static final String SCHEMA_VDB_NAME_PATTERN = "{0}schemavdb"; //$NON-NLS-1$
    private static final char SQL_ESCAPE_CHAR = '\"'; //$NON-NLS-1$
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
    public static final String JOIN_INNER = "INNER"; //$NON-NLS-1$
    /**
     * Left Outer Join type
     */
    public static final String JOIN_LEFT_OUTER = "LEFT_OUTER"; //$NON-NLS-1$
    /**
     * Right Outer Join type
     */
    public static final String JOIN_RIGHT_OUTER = "RIGHT_OUTER"; //$NON-NLS-1$
    /**
     * Full Outer Join type
     */
    public static final String JOIN_FULL_OUTER = "FULL_OUTER"; //$NON-NLS-1$
    
    /**
     * fqn table option key
     */
    String TABLE_OPTION_FQN = "teiid_relfqn"; //$NON-NLS-1$l
    
    protected final WorkspaceManager wsManager;
    
    /**
     * Constructs a Komodo service.
     *
     * @param engine
     *        the Komodo Engine (cannot be <code>null</code> and must be started)
     */
    public ViewDefinitionHelper( final WorkspaceManager wsManager ) {
        this.wsManager = wsManager;
    }
    
    public String getODataViewDdl(UnitOfWork uow, ViewDefinition viewDef) throws KException {
    	// Need to construct DDL based on
    	//   * 1 or 2 source tables
    	//   * Join criteria in the form of left and right critieria column names
        
    	String viewName = viewDef.getViewName(uow);
    	
        SourceTableConnection[] tableConns = findSourceTables(uow, viewDef);
    	
        StringBuilder sb = new StringBuilder();
        
        // Generate the View DDL
        sb.append("CREATE VIEW "); //$NON-NLS-1$
        sb.append(viewName);
        sb.append(StringConstants.SPACE+StringConstants.OPEN_BRACKET);
        sb.append("RowId integer PRIMARY KEY, "); //$NON-NLS-1$
        
//        List<String> fullyQualifiedColNames = new ArrayList<String>();
        
        // Check for join and single or 2 source join
        boolean isJoin = tableConns.length > 1;
        boolean singleSource = false;
        
        SourceTableConnection lhTableConnection = tableConns[0];
        SourceTableConnection rhTableConnection = null;
        if( isJoin ) {
        	rhTableConnection = tableConns[1];
        	singleSource =  lhTableConnection.getConnection().equals(rhTableConnection.getConnection());
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
        for( Column column : lhTableConnection.getSourceTable().getColumns(uow) ) {
        	ColumnInfo info = lhTableConnection.getColumnInfoMap().get(column);
        	
        	// Column info may not exist ???
        	if( info != null ) {
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
        
        if( rhTableConnection != null ) {
            for( Column column : rhTableConnection.getSourceTable().getColumns(uow) ) {
            	ColumnInfo info = rhTableConnection.getColumnInfoMap().get(column);
            	
            	// Column info may not exist ???
            	if( info != null ) {
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
        
        if( isJoin ) {
        	String lhTableName = lhTableConnection.getFQName() + " AS " + lhTableConnection.getAlias();
	        String rhTableName = rhTableConnection.getFQName() + " AS " + rhTableConnection.getAlias();
        	if( singleSource ) {
        		lhTableName = lhTableConnection.getName();
    	        rhTableName = rhTableConnection.getName();
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
	        
	        ViewBuilderCriteriaPredicate predicate = new ViewBuilderCriteriaPredicate();
	        
	        predicate.setLhColumn(comp1.getLeftCriteriaColumn(uow));
	        predicate.setRhColumn(comp1.getRightCriteriaColumn(uow));
	        predicate.setOperator(getOperator(uow, comp1));
            
	        if( singleSource ) {
	        	 sb.append(lhTableName+StringConstants.DOT).append(predicate.getLhColumn())
	             .append(StringConstants.SPACE+predicate.getOperator()+StringConstants.SPACE)
	             .append(rhTableName+StringConstants.DOT).append(predicate.getRhColumn());
	        } else {
	            sb.append(lhTableConnection.getAlias()+StringConstants.DOT).append(predicate.getLhColumn())
	            .append(StringConstants.SPACE+predicate.getOperator()+StringConstants.SPACE)
	            .append(rhTableConnection.getAlias()+StringConstants.DOT).append(predicate.getRhColumn());
	        }
	                
        } else {
            sb.append(lhTableConnection.getName()).append("\n");
        }
        
        sb.append(StringConstants.SEMI_COLON);

        return sb.toString();
    }

    /**
     * Find and resolve source {@link Table}s for a {@link ViewDefinition} object
     * @param uow
     * @param viewDefinition
     * @return {@link Table} array
     * @throws KException
     */
	public SourceTableConnection[] findSourceTables(UnitOfWork uow, ViewDefinition viewDefinition) throws KException {
    	if( viewDefinition.isComplete(uow) ) {

        	String[] sourceTablePaths = viewDefinition.getSourcePaths(uow);
        	
        	ArrayList<Table> sourceTables = new ArrayList<Table>(sourceTablePaths.length);
        	ArrayList<SourceTableConnection> sourceTableConnections = new ArrayList<SourceTableConnection>(sourceTablePaths.length);
        	
        	// Find and create Table for each source Path
        	for(String path : sourceTablePaths) {
        		String connectionName = PathUtils.getOption(path, "connection");
        		Connection connection = findConnection(uow, this.wsManager, connectionName);
        	
            	// Find Table objects in Komodo based on the connection name (i.e.    connection=pgConn
                final Model schemaModel = findSchemaModel( uow, this.wsManager, connection );

                // Get the tables from the schema and match them with the table name
                if ( schemaModel != null ) {
                    final Table[] tables = schemaModel.getTables( uow );
                    String tableOption = PathUtils.getTableOption(path);
                    
                    for (Table table: tables) {
                    	final String option = OptionContainerUtils.getOption( uow, table, TABLE_OPTION_FQN );
                    	if( option != null ) {
	                    	if( option.equals(tableOption) && !sourceTables.contains(table)) {
	                    		// Add the table to a temp list to prevent duplicates?
	                    		sourceTables.add(table);
	                    		String alias = "A";
	                    		if( sourceTables.size() == 2 ) alias = "B";
	                    		// create a new table connection object to maintain reference/info to 
	                    		sourceTableConnections.add(new SourceTableConnection(uow, path, table, alias, connection));
	                    	}
                    	}            	
                    }

                }
        	}

        	SourceTableConnection[] results = new SourceTableConnection[sourceTableConnections.size()];
        	return sourceTableConnections.toArray(results);
    	} else {
    		// TODO: throw exception/log message that view definition is incomplete
    		return new SourceTableConnection[0];
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
    
    /*
     * Generates comma separated string of the supplied column names
     * Will be of form: "column1, column2, column3"
     */
//    private static String getColString(List<String> columnNames) {
//        StringBuilder sb = new StringBuilder();
//        for (int i = 0; i < columnNames.size(); i++) {
//            if (i != 0) {
//                sb.append(StringConstants.COMMA);
//            }
//            sb.append(StringConstants.SPACE + escapeSQLName(columnNames.get(i)));
//        }
//        return sb.toString();
//    }

    /*
     * Generates comma separated string of the supplied column name with corresponding type
     * Will be of form: "column1 string, column2 string, column3 long"
     */
//    private static String getColWithTypeString(List<String> columnNames,
//                                               List<String> typeNames) {
//        StringBuilder sb = new StringBuilder();
//        for (int i = 0; i < columnNames.size(); i++) {
//            if (i != 0) {
//                sb.append(StringConstants.COMMA);
//            }
//            sb.append(StringConstants.SPACE + escapeSQLName(columnNames.get(i)));
//            sb.append(StringConstants.SPACE);
//            sb.append(typeNames.get(i));
//        }
//        return sb.toString();
//    }
    
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

    /*
     * Generates comma separated string of the supplied column names with their corresponding type.
     * If columns are in the duplicateNames list, then prefix them with the supplied alias.
     * Will be of form: "alias_column1 string, alias_column2 string, column3 long"
     */
//    private static String getAliasedColWithTypeString(List<String> columnNames,
//                                                      List<String> typeNames, String alias, List<String> duplicateNames) {
//        StringBuilder sb = new StringBuilder();
//        for (int i = 0; i < columnNames.size(); i++) {
//            if (i != 0) {
//                sb.append(StringConstants.COMMA);
//            }
//
//            // If the columnName is a duplicate, prefix it with "alias_"
//            String colName = columnNames.get(i);
//            if(duplicateNames.contains(colName.toLowerCase())) {
//                sb.append(StringConstants.SPACE + alias + StringConstants.UNDERSCORE + escapeSQLName(colName));
//            } else {
//                sb.append(StringConstants.SPACE + escapeSQLName(colName));
//            }
//            sb.append(StringConstants.SPACE);
//            sb.append(typeNames.get(i));
//        }
//        return sb.toString();
//    }
	
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
    
    public static Model getViewModel(final UnitOfWork uow, Vdb serviceVdb ) throws KException {
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
    
    class SourceTableConnection {

		final private String sourceTablePath;
    	final private Table sourceTable;
    	final private Model schemaModel;
    	final private Connection connection;
    	final private String alias;

    	private String name;
    	private String fqname;

		private Map<Column, ColumnInfo> columnInfoMap;
    	
    	protected SourceTableConnection(UnitOfWork uow, String path, Table table, String alias, Connection conn) throws KException {
    		this.sourceTablePath = path;
    		this.sourceTable = table;
    		this.alias = alias;
    		this.schemaModel = table.getParent(uow);
    		this.connection = conn;
    		init(uow);
    	}
    	
    	private void init(UnitOfWork uow) throws KException {
    		this.name = this.sourceTable.getName(uow);
    		this.fqname = this.schemaModel.getName(uow) + DOT + this.name;
    		// Walk through the columns and create an array of column + datatype strings
    		Column[] cols = this.sourceTable.getColumns(uow);
    		
    		int nCols = cols.length;
    		
    		this.columnInfoMap = new HashMap<Column, ColumnInfo>(nCols);
    		
    		for( Column col : cols) {
    			this.columnInfoMap.put(col, new ColumnInfo(uow, col, getFQName(), this.alias));
    		}
    	}
    	
    	public String getSourceTablePath() {
			return this.sourceTablePath;
		}

		public Table getSourceTable() {
			return this.sourceTable;
		}

		public Model getSchemaModel() {
			return this.schemaModel;
		}

		public Connection getConnection() {
			return this.connection;
		}
		
		public String getName() {
			return this.name;
		}
		
		public String getAlias() {
			return this.alias;
		}
		
		public String getFQName() {
			return fqname;
		}

		public Map<Column, ColumnInfo> getColumnInfoMap() {
			return columnInfoMap;
		}
    }
    
    class ColumnInfo {
    	final private Column column;
    	
    	private String name;
    	private String fqname;
    	private String aliasedName;
    	private String nameAndType;
    	
    	protected ColumnInfo(UnitOfWork uow, Column column, String tableFqn, String tblAlias) throws KException {
    		this.column = column;
    		init(uow, tableFqn, tblAlias);
    	}
    	
    	private void init(UnitOfWork uow, String tableFqn, String tblAlias) throws KException {
			this.name = this.column.getName(uow);
			this.nameAndType =  escapeSQLName(name) + SPACE + getColumnDatatypeString(uow, this.column);
			this.aliasedName = name;
			if( tblAlias != null ) {
				this.aliasedName = tblAlias + DOT + name;
			}
			this.fqname = tableFqn + DOT + escapeSQLName(name);
    	}

		public Column getColumn() {
			return this.column;
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