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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.komodo.KException;
import org.komodo.StringConstants;
import org.komodo.datavirtualization.SqlComposition;
import org.komodo.datavirtualization.SqlProjectedColumn;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.metadata.internal.TeiidDataSourceImpl;
import org.komodo.metadata.runtime.TeiidDataSource;
import org.komodo.rest.service.ServiceVdbGenerator.SchemaFinder;
import org.komodo.utils.StringUtils;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.metadata.MetadataFactory;
import org.teiid.metadata.Schema;
import org.teiid.query.metadata.SystemMetadata;
import org.teiid.query.parser.QueryParser;

@SuppressWarnings({ "javadoc", "nls" })
public class ServiceVdbGeneratorTest {
	
    private static String viewDefinitionName = "orderInfoView";
    private static String description = "test view description text";
    private boolean isComplete = true;
    private static String sourceTablePath1 = "connection=pgconnection1/schema=public/table=orders";
    private static String sourceTablePath1b = "connection=pgconnection1/schema=public/table=orders2";
    private static String sourceTablePath2 = "connection=pgconnection1/schema=public/table=customers";
    private static String sourceTablePath3 = "connection=pgconnection2/schema=public/table=customers";
    private static String comp1Name = "comp1";
    private static String comp1Desc = "description for comp1";
    private static String comp1LeftSource = "pgconnection1schemamodel.orders";
    private static String comp1RightSource = "pgconnection1schemamodel.customers";
    private static String comp1LeftColumn = "ID";
    private static String comp1RightColumn = "ID";
    private static String comp1Operator = "EQ";
    private static String TIMESTAMP = "TIMESTAMP";
    private static String STRING = "STRING";
    private static String LONG = "LONG";
    private static String ID = "ID";
    private static String IDType = LONG;
    private static String ORDER_DATE = "orderDate";
    private static String NAME = "name";
    private static String CUSTOMER_NAME = "customerName";
    
    private static String FQN_TABLE_1 = "schema=public/table=orders";
    private static String FQN_TABLE_2 = "schema=public/table=orders2";
    private static String FQN_TABLE_3 = "schema=public/table=customers";
    
    private static final String DS_NAME = "pgconnection1";
    private static final String DS_JNDI_NAME = "java:/jndiName1";
    private static final String MODEL_NAME = "pgconnection1schemamodel";
    private static final String DS_NAME_2 = "pgconnection2";
    private static final String DS_JNDI_NAME2 = "java:/jndiName2";
    private static final String MODEL_NAME_2 = "pgconnection2schemamodel";
    private static final String TRANSLATOR_JDBC = "jdbc";
    
    private boolean doPrint = false;
    
    
    private final static String TABLE_OPTION_FQN = "teiid_rel:fqn"; //$NON-NLS-1$
    
    private final static String SET_NAMESPACE_STRING = "SET NAMESPACE 'http://www.teiid.org/ext/relational/2012' AS teiid_rel;\n\n";
    
    private final static String pgconnection1schemamodelDDL = 
    		SET_NAMESPACE_STRING +
    		"CREATE FOREIGN TABLE orders ( "
    		+ "ID long primary key, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" '" + FQN_TABLE_1 + "');\n" +
    		"CREATE FOREIGN TABLE orders2 ( "
    		+ "ID long primary key, \"year\" string, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" '" + FQN_TABLE_2 + "');\n" +
    		"CREATE FOREIGN TABLE customers ( "
    		+ "ID long primary key, name string) OPTIONS(\"" + TABLE_OPTION_FQN + "\" '" + FQN_TABLE_3 + "');";
    
    private final static String pgconnection2schemamodelDDL = 
    		SET_NAMESPACE_STRING +
    		"CREATE FOREIGN TABLE orders ( "
    		+ "ID long primary key, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" '" + FQN_TABLE_1 + "');\n" +
    		"CREATE FOREIGN TABLE orders2 ( "
    		+ "ID long primary key, \"year\" string, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" '" + FQN_TABLE_2 + "');\n" +
    		"CREATE FOREIGN TABLE customers ( "
    		+ "ID long primary key, customerName string) OPTIONS(\"" + TABLE_OPTION_FQN + "\" '" + FQN_TABLE_3 + "');";
    
    private final static String EXPECTED_JOIN_SQL_TWO_SOURCES_START =
            "CREATE VIEW orderInfoView (RowId long PRIMARY KEY, ID LONG, orderDate TIMESTAMP, customerName STRING) OPTIONS (ANNOTATION 'test view description text') AS \n"
          + "SELECT ROW_NUMBER() OVER (ORDER BY A.ID), A.ID, A.orderDate, B.customerName\n"
          + "FROM pgconnection1schemamodel.orders AS A \n";
    
    private final static String  EXPECTED_JOIN_SQL_TWO_SOURCES_END = "pgconnection2schemamodel.customers AS B \n"
            + "ON \n"
            + "A.ID = B.ID;";
    
    private final static String EXPECTED_JOIN_SQL_SINGE_SOURCE_START =
            "CREATE VIEW orderInfoView (RowId long PRIMARY KEY, ID LONG, orderDate TIMESTAMP, name STRING) OPTIONS (ANNOTATION 'test view description text') AS \n"
          + "SELECT ROW_NUMBER() OVER (ORDER BY A.ID), A.ID, A.orderDate, B.name\n"
          + "FROM pgconnection1schemamodel.orders AS A \n";
    
    private final static String  EXPECTED_JOIN_SQL_SINGLE_SOURCE_END = "pgconnection1schemamodel.customers AS B \n"
          + "ON \n"
          + "A.ID = B.ID;";
    
    private final static String EXPECTED_NO_JOIN_SQL_SINGE_SOURCE =
            "CREATE VIEW orderInfoView (ID, orderDate, PRIMARY KEY(ID)) OPTIONS (ANNOTATION 'test view description text') AS \n" + 
            "SELECT ID, orderDate\n" + 
            "FROM pgconnection1schemamodel.orders;";

    private final static String EXPECTED_NO_JOIN_SQL_SINGE_SOURCE_WITH_KEYWORD =
            "CREATE VIEW orderInfoView (ID, \"year\", orderDate, PRIMARY KEY(ID)) OPTIONS (ANNOTATION 'test view description text') AS \n" + 
            "SELECT ID, \"year\", orderDate\n" + 
            "FROM pgconnection1schemamodel.orders2;";
    
    // ===========================
    // orders
    //  - ID LONG
    //  - orderDate TIMESTAMP
    //  - orderName STRING
    //  - orderDesc STRING
    //
    // customers
    //  - ID LONG
    //  - customerName STRING
    //  - customerAddress STRING
    //  - customerState STRING
    
    // ===========================
    
    private final static String INNER_JOIN_STR = "INNER JOIN \n";
    private final static String LEFT_OUTER_JOIN_STR = "LEFT OUTER JOIN \n";
    private final static String RIGHT_OUTER_JOIN_STR = "RIGHT OUTER JOIN \n";
    private final static String FULL_OUTER_JOIN_STR = "FULL OUTER JOIN \n";
    
    private Map<String, TeiidDataSource> dataSources = new HashMap<>();
    private Map<String, Schema> schemas = new HashMap<>();

    @Before
    public void init() throws Exception {
    	addSourceInfo(DS_NAME, DS_JNDI_NAME, pgconnection1schemamodelDDL, MODEL_NAME);
    	addSourceInfo(DS_NAME_2, DS_JNDI_NAME2, pgconnection2schemamodelDDL, MODEL_NAME_2);
    }

	private void addSourceInfo(String connectionName, String jndiName, String ddl, String modelName) {
		Properties properties = new Properties();
		properties.setProperty(TeiidDataSource.DATASOURCE_DRIVERNAME, TRANSLATOR_JDBC);
		properties.setProperty(TeiidDataSource.DATASOURCE_JNDINAME, jndiName);
		dataSources.put(connectionName, new TeiidDataSourceImpl(connectionName, properties));
    	
		MetadataFactory mf = new MetadataFactory("x", 1, modelName, SystemMetadata.getInstance().getRuntimeTypeMap(), new Properties(), null);
        QueryParser.getQueryParser().parseDDL(mf, ddl);
        schemas.put(connectionName, mf.getSchema());
	}
    
	private String helpGenerateDdlForWithJoinType(String secondSourceTablePath, String joinType, boolean singleConnection, boolean useAll) throws KException {
        ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(schemaFinder());

        String[] sourceTablePaths = { sourceTablePath1, secondSourceTablePath };
        boolean twoTables = secondSourceTablePath != null && StringUtils.areDifferent(sourceTablePath1, secondSourceTablePath);
        
        ViewDefinition viewDef = mock(ViewDefinition.class);
        when(viewDef.getViewName()).thenReturn(viewDefinitionName);
        when(viewDef.getDescription()).thenReturn(description);
        when(viewDef.isComplete()).thenReturn(isComplete);
        when(viewDef.getSourcePaths()).thenReturn(Arrays.asList(sourceTablePaths));
        
        SqlComposition sqlComp1 = mock(SqlComposition.class);
        when(sqlComp1.getName()).thenReturn(comp1Name);
        when(sqlComp1.getDescription()).thenReturn(comp1Desc);
        when(sqlComp1.getLeftSourcePath()).thenReturn(comp1LeftSource);
        when(sqlComp1.getRightSourcePath()).thenReturn(comp1RightSource);
        when(sqlComp1.getLeftCriteriaColumn()).thenReturn(comp1LeftColumn);
        when(sqlComp1.getRightCriteriaColumn()).thenReturn(comp1RightColumn);
        when(sqlComp1.getType()).thenReturn(joinType);
        when(sqlComp1.getOperator()).thenReturn(comp1Operator);
        
        SqlComposition[] sqlComps = new SqlComposition[1];
        sqlComps[0] = sqlComp1;
        when(viewDef.getSqlCompositions()).thenReturn(Arrays.asList(sqlComps));
        
        if( useAll ) {
        	SqlProjectedColumn projCol = mock(SqlProjectedColumn.class);
	        when(projCol.getName()).thenReturn("ALL");
	        when(projCol.getType()).thenReturn("ALL");
	        when(projCol.isSelected()).thenReturn(true);
	        
	        SqlProjectedColumn[] projCols = new SqlProjectedColumn[1];
	        projCols[0] = projCol;
	        when(viewDef.getProjectedColumns()).thenReturn(Arrays.asList(projCols));
        } else {
        	SqlProjectedColumn[] projCols = new SqlProjectedColumn[3];
        	
        	SqlProjectedColumn projCol = mock(SqlProjectedColumn.class);
	        when(projCol.getName()).thenReturn(ID);
	        when(projCol.getType()).thenReturn(IDType);
	        when(projCol.isSelected()).thenReturn(true);
	        projCols[0] = projCol;
	        
	        projCol = mock(SqlProjectedColumn.class);
	        when(projCol.getName()).thenReturn(ORDER_DATE);
	        when(projCol.getType()).thenReturn(TIMESTAMP);
	        when(projCol.isSelected()).thenReturn(true);
	        projCols[1] = projCol;
	        
	        projCol = mock(SqlProjectedColumn.class);
	        if( twoTables && !singleConnection ) {
	        	when(projCol.getName()).thenReturn(CUSTOMER_NAME);
	        } else {
	        	when(projCol.getName()).thenReturn(NAME);
	        }
	        when(projCol.getType()).thenReturn(STRING);
	        when(projCol.isSelected()).thenReturn(true);
	        projCols[2] = projCol;
	        
	        when(viewDef.getProjectedColumns()).thenReturn(Arrays.asList(projCols));
        }

        return vdbGenerator.getODataViewDdl(viewDef);
    }
    
    private ViewDefinition[] helpCreateViewEditorState(int numSources) throws KException {

    	ViewDefinition[] stateArray = new ViewDefinition[1];

        ViewDefinition viewDef = mock(ViewDefinition.class);
        when(viewDef.getName()).thenReturn(viewDefinitionName);
        stateArray[0] = viewDef;
        if( numSources == 1 ) {
        	helpCreateViewDefinitionAll(viewDef, sourceTablePath2, false);
        } else {
        	helpCreateViewDefinitionAll(viewDef, sourceTablePath3, false);
        }
        
        return stateArray;
    }
    
    private ViewDefinition helpCreateViewDefinitionAll(ViewDefinition viewDef, String secondSourceTablePath, boolean useAll) throws KException {

        String[] sourceTablePaths = { sourceTablePath1, secondSourceTablePath };
        boolean twoTables = secondSourceTablePath != null && StringUtils.areDifferent(sourceTablePath1, secondSourceTablePath);
        
        when(viewDef.getViewName()).thenReturn(viewDefinitionName);
        when(viewDef.getDescription()).thenReturn(description);
        when(viewDef.isComplete()).thenReturn(isComplete);
        when(viewDef.getSourcePaths()).thenReturn(Arrays.asList(sourceTablePaths));
        
        SqlComposition sqlComp1 = mock(SqlComposition.class);
        when(sqlComp1.getName()).thenReturn(comp1Name);
        when(sqlComp1.getDescription()).thenReturn(comp1Desc);
        when(sqlComp1.getLeftSourcePath()).thenReturn(comp1LeftSource);
        when(sqlComp1.getRightSourcePath()).thenReturn(comp1RightSource);
        when(sqlComp1.getLeftCriteriaColumn()).thenReturn(comp1LeftColumn);
        when(sqlComp1.getRightCriteriaColumn()).thenReturn(comp1RightColumn);
        when(sqlComp1.getType()).thenReturn(ServiceVdbGenerator.JOIN_INNER);
        when(sqlComp1.getOperator()).thenReturn(comp1Operator);
        
        SqlComposition[] sqlComps = new SqlComposition[1];
        sqlComps[0] = sqlComp1;
        when(viewDef.getSqlCompositions()).thenReturn(Arrays.asList(sqlComps));
        
        if( useAll ) {
        	SqlProjectedColumn projCol = mock(SqlProjectedColumn.class);
	        when(projCol.getName()).thenReturn("ALL");
	        when(projCol.getType()).thenReturn("ALL");
	        when(projCol.isSelected()).thenReturn(true);
	        
	        SqlProjectedColumn[] projCols = new SqlProjectedColumn[1];
	        projCols[0] = projCol;
	        when(viewDef.getProjectedColumns()).thenReturn(Arrays.asList(projCols));
        } else {
        	SqlProjectedColumn[] projCols = new SqlProjectedColumn[3];
        	
        	SqlProjectedColumn projCol = mock(SqlProjectedColumn.class);
	        when(projCol.getName()).thenReturn(ID);
	        when(projCol.getType()).thenReturn(IDType);
	        when(projCol.isSelected()).thenReturn(true);
	        projCols[0] = projCol;
	        
	        projCol = mock(SqlProjectedColumn.class);
	        when(projCol.getName()).thenReturn(ORDER_DATE);
	        when(projCol.getType()).thenReturn(TIMESTAMP);
	        when(projCol.isSelected()).thenReturn(true);
	        projCols[1] = projCol;
	        
	        projCol = mock(SqlProjectedColumn.class);
	        if( twoTables ) {
	        	when(projCol.getName()).thenReturn(CUSTOMER_NAME);
	        } else {
	        	when(projCol.getName()).thenReturn(NAME);
	        }
	        when(projCol.getType()).thenReturn(STRING);
	        when(projCol.isSelected()).thenReturn(true);
	        projCols[2] = projCol;
	        
	        when(viewDef.getProjectedColumns()).thenReturn(Arrays.asList(projCols));
        }
        
        return viewDef;
    }
    
    private void printResults(String expected, String generated) {
    	if( doPrint ) {
    		System.out.println("\nServiceVdbGeneratorTest\n\tEXPECTED DDL = \n" + expected);
    		System.out.println("\nServiceVdbGeneratorTest\n\tGENERATED DDL = \n" + generated);
    	}
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_NoJoinOneTable() throws Exception {
    	String EXPECTED_DDL = EXPECTED_NO_JOIN_SQL_SINGE_SOURCE;
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(schemaFinder());

        String[] sourceTablePaths = { sourceTablePath1 };
        ViewDefinition viewDef = mock(ViewDefinition.class);
        when(viewDef.getViewName()).thenReturn(viewDefinitionName);
        when(viewDef.getDescription()).thenReturn(description);
        when(viewDef.isComplete()).thenReturn(isComplete);
        when(viewDef.getSourcePaths()).thenReturn(Arrays.asList(sourceTablePaths));
        
        SqlProjectedColumn projCol = mock(SqlProjectedColumn.class);
        when(projCol.getName()).thenReturn("ALL");
        when(projCol.getType()).thenReturn("ALL");
        when(projCol.isSelected()).thenReturn(true);
        
        SqlProjectedColumn[] projCols = new SqlProjectedColumn[1];
        projCols[0] = projCol;
        when(viewDef.getProjectedColumns()).thenReturn(Arrays.asList(projCols));

        String viewDdl = vdbGenerator.getODataViewDdl(viewDef);
        printResults(EXPECTED_DDL, viewDdl);
        assertEquals(EXPECTED_DDL, viewDdl);
    }

    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_NoJoinOneTable_withKeywordCol() throws Exception {
    	String EXPECTED_DDL = EXPECTED_NO_JOIN_SQL_SINGE_SOURCE_WITH_KEYWORD;
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(schemaFinder());

        String[] sourceTablePaths = { sourceTablePath1b };
        ViewDefinition viewDef = mock(ViewDefinition.class);
        when(viewDef.getViewName()).thenReturn(viewDefinitionName);
        when(viewDef.getDescription()).thenReturn(description);
        when(viewDef.isComplete()).thenReturn(isComplete);
        when(viewDef.getSourcePaths()).thenReturn(Arrays.asList(sourceTablePaths));
        
        SqlProjectedColumn projCol = mock(SqlProjectedColumn.class);
        when(projCol.getName()).thenReturn("ALL");
        when(projCol.getType()).thenReturn("ALL");
        when(projCol.isSelected()).thenReturn(true);
        
        SqlProjectedColumn[] projCols = new SqlProjectedColumn[1];
        projCols[0] = projCol;
        when(viewDef.getProjectedColumns()).thenReturn(Arrays.asList(projCols));

        String viewDdl = vdbGenerator.getODataViewDdl(viewDef);
        printResults(EXPECTED_DDL, viewDdl);
        assertEquals(EXPECTED_DDL, viewDdl);
    }

	@Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_InnerJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + INNER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_INNER, true, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_InnerJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + INNER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_INNER, true, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_LeftOuterJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + LEFT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_LEFT_OUTER, true, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_LeftOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + LEFT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_LEFT_OUTER, true, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_RightOuterJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + RIGHT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_RIGHT_OUTER, true, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_RightOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + RIGHT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_RIGHT_OUTER, true, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_FullOuterJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + FULL_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_FULL_OUTER, true, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_FullOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + FULL_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ServiceVdbGenerator.JOIN_FULL_OUTER, true, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_InnerJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + INNER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_INNER, false, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_InnerJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + INNER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_INNER, false, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_LeftOuterJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + LEFT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_LEFT_OUTER, false, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_LeftOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + LEFT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_LEFT_OUTER, false, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_RightOuterJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + RIGHT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_RIGHT_OUTER, false, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_RightOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + RIGHT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_RIGHT_OUTER, false, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_FullOuterJoinAll() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + FULL_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_FULL_OUTER, false, true);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_FullOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + FULL_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ServiceVdbGenerator.JOIN_FULL_OUTER, false, false);
        printResults(EXPECTED_DDL, viewDdl);
        // TODO Uncomment after JOINs are working
        // assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldRefreshServiceVdb_SingleSource() throws Exception {
    	ViewDefinition[] states = helpCreateViewEditorState(1);
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(schemaFinder());
    	
    	VDBMetaData serviceVdb = vdbGenerator.refreshServiceVdb(viewDefinitionName, states);

    	List<org.teiid.adminapi.Model> models = serviceVdb.getModels();
    	
    	assertThat(models.size(), is(2));
        ModelMetaData viewModel = serviceVdb.getModel(StringConstants.SERVICE_VDB_VIEW_MODEL);
        assertNotNull(viewModel);
        assertEquals("CREATE VIEW orderInfoView (ID LONG, orderDate TIMESTAMP, customerName STRING) OPTIONS (ANNOTATION 'test view description text') AS \n" + 
        		"SELECT A.ID, A.orderDate, \n" + 
        		"FROM pgconnection1schemamodel.orders AS A;\n", viewModel.getSourceMetadataText().get(0));
    	
//    	for( Model model : models ) {
//    		if( model.getModelType() == Type.PHYSICAL) {
//    			ModelSource[] modelSources = model.getSources();
//    			assertNotNull(modelSources);
//    			for( ModelSource ms : modelSources ) {
//    				assertNotNull(ms.getOriginConnection());
//    			}
//    		}
//    	}
    }
    
    @Test
    public void shouldRefreshServiceVdb_TwoSources() throws Exception {
    	ViewDefinition[] states = helpCreateViewEditorState(2);
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(schemaFinder());
    	
    	VDBMetaData serviceVdb = vdbGenerator.refreshServiceVdb(viewDefinitionName, states);
    	
    	List<org.teiid.adminapi.Model> models = serviceVdb.getModels();
    	
        assertThat(models.size(), is(3));
        ModelMetaData viewModel = serviceVdb.getModel(StringConstants.SERVICE_VDB_VIEW_MODEL);
        assertNotNull(viewModel);
        assertEquals("CREATE VIEW orderInfoView (ID LONG, orderDate TIMESTAMP, customerName STRING) OPTIONS (ANNOTATION 'test view description text') AS \n" + 
        		"SELECT A.ID, A.orderDate, B.customerName\n" + 
        		"FROM pgconnection1schemamodel.orders AS A;\n", viewModel.getSourceMetadataText().get(0));
    	
    	
//    	for( Model model : models ) {
//    		if( model.getModelType() == Type.PHYSICAL) {
//    			ModelSource[] modelSources = model.getSources();
//    			assertNotNull(modelSources);
//    			for( ModelSource ms : modelSources ) {
//    				assertNotNull(ms.getOriginConnection());
//    			}
//    		}
//    	}
    }
    
    protected ServiceVdbGenerator.SchemaFinder schemaFinder() throws KException {
		return new SchemaFinder() {
			
			@Override
			public TeiidDataSource findTeiidDatasource(String connectionName) throws KException {
				return dataSources.get(connectionName);
			}
			
			@Override
			public Schema findSchema(String connectionName) throws KException {
				return schemas.get(connectionName);
			}
		};
	}
    
}
