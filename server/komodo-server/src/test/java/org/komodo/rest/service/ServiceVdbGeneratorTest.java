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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.View;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.profile.internal.SqlCompositionImpl;
import org.komodo.relational.profile.internal.SqlProjectedColumnImpl;
import org.komodo.relational.profile.internal.ViewDefinitionImpl;
import org.komodo.relational.profile.internal.ViewEditorStateImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.workspace.WorkspaceManagerImpl;
import org.komodo.spi.KException;
import org.komodo.utils.StringUtils;

@SuppressWarnings({ "javadoc", "nls" })
public class ServiceVdbGeneratorTest extends RelationalModelTest {
	
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
    
    private static final String TEST_VIEW_NAME = "test_view_name";
    private static final String DS_NAME = "pgconnection1";
    private static final String DS_JNDI_NAME = "java:/jndiName1";
    private static final String VDB_NAME = "pgconnection1schemavdb";
    private static final String MODEL_NAME = "pgconnection1schemamodel";
    private static final String DS_NAME_2 = "pgconnection2";
    private static final String DS_JNDI_NAME2 = "java:/jndiName2";
    private static final String VDB_NAME_2 = "pgconnection2schemavdb";
    private static final String MODEL_NAME_2 = "pgconnection2schemamodel";
    private static final String TRANSLATOR_JDBC = "jdbc";
    
    private boolean doPrint = false;
    
    
    private final static String TABLE_OPTION_FQN = "teiid_rel:fqn"; //$NON-NLS-1$
    
    private final static String SET_NAMESPACE_STRING = "SET NAMESPACE 'http://www.teiid.org/ext/relational/2012' AS teiid_rel;\n\n";
    
    private final static String pgconnection1schemamodelDDL = 
    		SET_NAMESPACE_STRING +
    		"CREATE FOREIGN TABLE orders ( "
    		+ "ID long, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_1 + "\");\n" +
    		"CREATE FOREIGN TABLE orders2 ( "
    		+ "ID long, year string, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_2 + "\");\n" +
    		"CREATE FOREIGN TABLE customers ( "
    		+ "ID long, name string) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_3 + "\");";
    
    private final static String pgconnection2schemamodelDDL = 
    		SET_NAMESPACE_STRING +
    		"CREATE FOREIGN TABLE orders ( "
    		+ "ID long, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_1 + "\");\n" +
    		"CREATE FOREIGN TABLE orders2 ( "
    		+ "ID long, year string, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_2 + "\");\n" +
    		"CREATE FOREIGN TABLE customers ( "
    		+ "ID long, customerName string) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_3 + "\");";
    
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
            "CREATE VIEW orderInfoView (RowId long PRIMARY KEY, ID LONG, orderDate TIMESTAMP) OPTIONS (ANNOTATION 'test view description text') AS \n"
          + "SELECT ROW_NUMBER() OVER (ORDER BY ID), ID, orderDate\n"
          + "FROM pgconnection1schemamodel.orders;";

    private final static String EXPECTED_NO_JOIN_SQL_SINGE_SOURCE_WITH_KEYWORD =
            "CREATE VIEW orderInfoView (RowId long PRIMARY KEY, ID LONG, \"year\" STRING, orderDate TIMESTAMP) OPTIONS (ANNOTATION 'test view description text') AS \n"
          + "SELECT ROW_NUMBER() OVER (ORDER BY ID), ID, \"year\", orderDate\n"
          + "FROM pgconnection1schemamodel.orders2;";
    
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

    @Before
    public void init() throws Exception {
    	Vdb dsVdb = createVdb(viewDefinitionName, "originalFilePath");
    	Model viewModel = dsVdb.addModel(getTransaction(), TEST_VIEW_NAME);
    	viewModel.setModelType(getTransaction(), Type.VIRTUAL);
    	
//        Connection connection = createConnection( DS_NAME );
//        connection.setDescription(getTransaction(), description);
//
//        final String extLoc = "new-external-location";
//        connection.setExternalLocation(getTransaction(), extLoc );
//
//        connection.setJdbc(getTransaction(), false);
//        connection.setJndiName(getTransaction(), DS_JNDI_NAME);
//        connection.setDriverName(getTransaction(), "dsDriver");
//        connection.setClassName(getTransaction(), "dsClassname");
//        connection.setProperty(getTransaction(), "prop1", "prop1Value");
//        connection.setProperty(getTransaction(), "prop2", "prop2Value");

        // Create a schema VDB for connection1
        VdbImpl sourceVdb = createVdb(VDB_NAME, null, "originalFilePath");
        ModelImpl sourceModel = sourceVdb.addModel(getTransaction(), MODEL_NAME);
        sourceModel.setModelDefinition(getTransaction(), pgconnection1schemamodelDDL);
        ModelSourceImpl modelSource = sourceModel.addSource(getTransaction(), DS_NAME);
        modelSource.setJndiName(getTransaction(), DS_JNDI_NAME);
        modelSource.setTranslatorName(getTransaction(), TRANSLATOR_JDBC);
        //modelSource.setAssociatedConnection(getTransaction(), connection);

        commit();
        
//        Connection connection2 = createConnection( DS_NAME_2 );
//        connection2.setDescription(getTransaction(), description);
//        
//        connection2.setExternalLocation(getTransaction(), extLoc );
//        
//        connection2.setJdbc(getTransaction(), false);
//        connection2.setJndiName(getTransaction(), "DS_JNDI_NAME2");
//        connection2.setDriverName(getTransaction(), "dsDriver");
//        connection2.setClassName(getTransaction(), "dsClassname2");
//        connection2.setProperty(getTransaction(), "prop1", "prop1Value");
//        connection2.setProperty(getTransaction(), "prop2", "prop2Value");

        // Create a schema VDB for connection2
        sourceVdb = createVdb(VDB_NAME_2, null, "originalFilePath");
        sourceModel = sourceVdb.addModel(getTransaction(), MODEL_NAME_2);
        sourceModel.setModelDefinition(getTransaction(), pgconnection2schemamodelDDL);
        ModelSourceImpl modelSource2 = sourceModel.addSource(getTransaction(), DS_NAME_2);
        modelSource2.setJndiName(getTransaction(), DS_JNDI_NAME2);
        modelSource2.setTranslatorName(getTransaction(), TRANSLATOR_JDBC);
        //modelSource2.setAssociatedConnection(getTransaction(), connection2);
        
        commit();
    }
    
    private String helpGenerateDdlForWithJoinType(String secondSourceTablePath, String joinType, boolean singleConnection, boolean useAll) throws KException {
        ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(WorkspaceManagerImpl.getInstance(_repo, getTransaction()));

        String[] sourceTablePaths = { sourceTablePath1, secondSourceTablePath };
        boolean twoTables = secondSourceTablePath != null && StringUtils.areDifferent(sourceTablePath1, secondSourceTablePath);
        
        ViewDefinitionImpl viewDef = mock(ViewDefinitionImpl.class);
        when(viewDef.getName(getTransaction())).thenReturn("vdbDefinition");
        when(viewDef.getViewName(getTransaction())).thenReturn(viewDefinitionName);
        when(viewDef.getDescription(getTransaction())).thenReturn(description);
        when(viewDef.isComplete(getTransaction())).thenReturn(isComplete);
        when(viewDef.getSourcePaths(getTransaction())).thenReturn(sourceTablePaths);
        
        SqlCompositionImpl sqlComp1 = mock(SqlCompositionImpl.class);
        when(sqlComp1.getName(getTransaction())).thenReturn(comp1Name);
        when(sqlComp1.getDescription(getTransaction())).thenReturn(comp1Desc);
        when(sqlComp1.getLeftSourcePath(getTransaction())).thenReturn(comp1LeftSource);
        when(sqlComp1.getRightSourcePath(getTransaction())).thenReturn(comp1RightSource);
        when(sqlComp1.getLeftCriteriaColumn(getTransaction())).thenReturn(comp1LeftColumn);
        when(sqlComp1.getRightCriteriaColumn(getTransaction())).thenReturn(comp1RightColumn);
        when(sqlComp1.getType(getTransaction())).thenReturn(joinType);
        when(sqlComp1.getOperator(getTransaction())).thenReturn(comp1Operator);
        
        SqlCompositionImpl[] sqlComps = new SqlCompositionImpl[1];
        sqlComps[0] = sqlComp1;
        when(viewDef.getSqlCompositions(getTransaction())).thenReturn(sqlComps);
        
        if( useAll ) {
        	SqlProjectedColumnImpl projCol = mock(SqlProjectedColumnImpl.class);
	        when(projCol.getName(getTransaction())).thenReturn("ALL");
	        when(projCol.getType(getTransaction())).thenReturn("ALL");
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        
	        SqlProjectedColumnImpl[] projCols = new SqlProjectedColumnImpl[1];
	        projCols[0] = projCol;
	        when(viewDef.getProjectedColumns(getTransaction())).thenReturn(projCols);
        } else {
        	SqlProjectedColumnImpl[] projCols = new SqlProjectedColumnImpl[3];
        	
        	SqlProjectedColumnImpl projCol = mock(SqlProjectedColumnImpl.class);
	        when(projCol.getName(getTransaction())).thenReturn(ID);
	        when(projCol.getType(getTransaction())).thenReturn(IDType);
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        projCols[0] = projCol;
	        
	        projCol = mock(SqlProjectedColumnImpl.class);
	        when(projCol.getName(getTransaction())).thenReturn(ORDER_DATE);
	        when(projCol.getType(getTransaction())).thenReturn(TIMESTAMP);
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        projCols[1] = projCol;
	        
	        projCol = mock(SqlProjectedColumnImpl.class);
	        if( twoTables && !singleConnection ) {
	        	when(projCol.getName(getTransaction())).thenReturn(CUSTOMER_NAME);
	        } else {
	        	when(projCol.getName(getTransaction())).thenReturn(NAME);
	        }
	        when(projCol.getType(getTransaction())).thenReturn(STRING);
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        projCols[2] = projCol;
	        
	        when(viewDef.getProjectedColumns(getTransaction())).thenReturn(projCols);
        }

        return vdbGenerator.getODataViewDdl(getTransaction(), viewDef);
    }
    
    private ViewEditorStateImpl[] helpCreateViewEditorState(int numSources) throws KException {

        ViewEditorStateImpl[] stateArray = new ViewEditorStateImpl[1];

        ViewEditorStateImpl state = mock(ViewEditorStateImpl.class);
        when(state.getName(getTransaction())).thenReturn(viewDefinitionName);
        stateArray[0] = state;
        ViewDefinitionImpl viewDef = mock(ViewDefinitionImpl.class);
        when(state.setViewDefinition(getTransaction())).thenReturn(viewDef);
        when(state.getViewDefinition(getTransaction())).thenReturn(viewDef);
        if( numSources == 1 ) {
        	helpCreateViewDefinitionAll(viewDef, sourceTablePath2, false);
        } else {
        	helpCreateViewDefinitionAll(viewDef, sourceTablePath3, false);
        }
        
        return stateArray;
    }
    
    private ViewDefinitionImpl helpCreateViewDefinitionAll(ViewDefinitionImpl viewDef, String secondSourceTablePath, boolean useAll) throws KException {

        String[] sourceTablePaths = { sourceTablePath1, secondSourceTablePath };
        boolean twoTables = secondSourceTablePath != null && StringUtils.areDifferent(sourceTablePath1, secondSourceTablePath);
        
        when(viewDef.getName(getTransaction())).thenReturn("vdbDefinition");
        when(viewDef.getViewName(getTransaction())).thenReturn(viewDefinitionName);
        when(viewDef.getDescription(getTransaction())).thenReturn(description);
        when(viewDef.isComplete(getTransaction())).thenReturn(isComplete);
        when(viewDef.getSourcePaths(getTransaction())).thenReturn(sourceTablePaths);
        
        SqlCompositionImpl sqlComp1 = mock(SqlCompositionImpl.class);
        when(sqlComp1.getName(getTransaction())).thenReturn(comp1Name);
        when(sqlComp1.getDescription(getTransaction())).thenReturn(comp1Desc);
        when(sqlComp1.getLeftSourcePath(getTransaction())).thenReturn(comp1LeftSource);
        when(sqlComp1.getRightSourcePath(getTransaction())).thenReturn(comp1RightSource);
        when(sqlComp1.getLeftCriteriaColumn(getTransaction())).thenReturn(comp1LeftColumn);
        when(sqlComp1.getRightCriteriaColumn(getTransaction())).thenReturn(comp1RightColumn);
        when(sqlComp1.getType(getTransaction())).thenReturn(ServiceVdbGenerator.JOIN_INNER);
        when(sqlComp1.getOperator(getTransaction())).thenReturn(comp1Operator);
        
        SqlCompositionImpl[] sqlComps = new SqlCompositionImpl[1];
        sqlComps[0] = sqlComp1;
        when(viewDef.getSqlCompositions(getTransaction())).thenReturn(sqlComps);
        
        if( useAll ) {
        	SqlProjectedColumnImpl projCol = mock(SqlProjectedColumnImpl.class);
	        when(projCol.getName(getTransaction())).thenReturn("ALL");
	        when(projCol.getType(getTransaction())).thenReturn("ALL");
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        
	        SqlProjectedColumnImpl[] projCols = new SqlProjectedColumnImpl[1];
	        projCols[0] = projCol;
	        when(viewDef.getProjectedColumns(getTransaction())).thenReturn(projCols);
        } else {
        	SqlProjectedColumnImpl[] projCols = new SqlProjectedColumnImpl[3];
        	
        	SqlProjectedColumnImpl projCol = mock(SqlProjectedColumnImpl.class);
	        when(projCol.getName(getTransaction())).thenReturn(ID);
	        when(projCol.getType(getTransaction())).thenReturn(IDType);
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        projCols[0] = projCol;
	        
	        projCol = mock(SqlProjectedColumnImpl.class);
	        when(projCol.getName(getTransaction())).thenReturn(ORDER_DATE);
	        when(projCol.getType(getTransaction())).thenReturn(TIMESTAMP);
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        projCols[1] = projCol;
	        
	        projCol = mock(SqlProjectedColumnImpl.class);
	        if( twoTables ) {
	        	when(projCol.getName(getTransaction())).thenReturn(CUSTOMER_NAME);
	        } else {
	        	when(projCol.getName(getTransaction())).thenReturn(NAME);
	        }
	        when(projCol.getType(getTransaction())).thenReturn(STRING);
	        when(projCol.isSelected(getTransaction())).thenReturn(true);
	        projCols[2] = projCol;
	        
	        when(viewDef.getProjectedColumns(getTransaction())).thenReturn(projCols);
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
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(WorkspaceManagerImpl.getInstance(_repo, getTransaction()));

        String[] sourceTablePaths = { sourceTablePath1 };
        ViewDefinitionImpl viewDef = mock(ViewDefinitionImpl.class);
        when(viewDef.getName(getTransaction())).thenReturn("vdbDefinition");
        when(viewDef.getViewName(getTransaction())).thenReturn(viewDefinitionName);
        when(viewDef.getDescription(getTransaction())).thenReturn(description);
        when(viewDef.isComplete(getTransaction())).thenReturn(isComplete);
        when(viewDef.getSourcePaths(getTransaction())).thenReturn(sourceTablePaths);
        
        SqlProjectedColumnImpl projCol = mock(SqlProjectedColumnImpl.class);
        when(projCol.getName(getTransaction())).thenReturn("ALL");
        when(projCol.getType(getTransaction())).thenReturn("ALL");
        when(projCol.isSelected(getTransaction())).thenReturn(true);
        
        SqlProjectedColumnImpl[] projCols = new SqlProjectedColumnImpl[1];
        projCols[0] = projCol;
        when(viewDef.getProjectedColumns(getTransaction())).thenReturn(projCols);

        String viewDdl = vdbGenerator.getODataViewDdl(getTransaction(), viewDef);
        printResults(EXPECTED_DDL, viewDdl);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_NoJoinOneTable_withKeywordCol() throws Exception {
    	String EXPECTED_DDL = EXPECTED_NO_JOIN_SQL_SINGE_SOURCE_WITH_KEYWORD;
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(WorkspaceManagerImpl.getInstance(_repo, getTransaction()));

        String[] sourceTablePaths = { sourceTablePath1b };
        ViewDefinitionImpl viewDef = mock(ViewDefinitionImpl.class);
        when(viewDef.getName(getTransaction())).thenReturn("vdbDefinition");
        when(viewDef.getViewName(getTransaction())).thenReturn(viewDefinitionName);
        when(viewDef.getDescription(getTransaction())).thenReturn(description);
        when(viewDef.isComplete(getTransaction())).thenReturn(isComplete);
        when(viewDef.getSourcePaths(getTransaction())).thenReturn(sourceTablePaths);
        
        SqlProjectedColumnImpl projCol = mock(SqlProjectedColumnImpl.class);
        when(projCol.getName(getTransaction())).thenReturn("ALL");
        when(projCol.getType(getTransaction())).thenReturn("ALL");
        when(projCol.isSelected(getTransaction())).thenReturn(true);
        
        SqlProjectedColumnImpl[] projCols = new SqlProjectedColumnImpl[1];
        projCols[0] = projCol;
        when(viewDef.getProjectedColumns(getTransaction())).thenReturn(projCols);

        String viewDdl = vdbGenerator.getODataViewDdl(getTransaction(), viewDef);
        printResults(EXPECTED_DDL, viewDdl);
        assertThat(viewDdl, is(EXPECTED_DDL));
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
    	ViewEditorStateImpl[] states = helpCreateViewEditorState(1);
    	VdbImpl[] vdbs = WorkspaceManagerImpl.getInstance(_repo, getTransaction()).findVdbs(getTransaction());
    	
    	VdbImpl serviceVdb = null;
    	for( VdbImpl vdb : vdbs ) {
    		if( vdb.getName(getTransaction()).equals(viewDefinitionName)) {
    			serviceVdb = vdb;
    			break;
    		}
    	}
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(WorkspaceManagerImpl.getInstance(_repo, getTransaction()));
    	
    	vdbGenerator.refreshServiceVdb(getTransaction(), serviceVdb, states);
    	
    	commit();
    	
    	Model[] models = serviceVdb.getModels(getTransaction());
    	
        assertThat(models.length, is(2));
        ModelImpl viewModel = (ModelImpl)ServiceVdbGenerator.getViewModel(getTransaction(), serviceVdb);
        assertNotNull(viewModel);
        assertThat(viewModel.getViews(getTransaction()).length, is(1));
    	View view = viewModel.getViews(getTransaction())[0];
    	assertNotNull(view);
    	assertThat(view.getColumns(getTransaction()).length, is(4));
    	
//    	for( ModelImpl model : models ) {
//    		if( model.getModelType(getTransaction()) == Type.PHYSICAL) {
//    			ModelSource[] modelSources = model.getSources(getTransaction());
//    			assertNotNull(modelSources);
//    			for( ModelSourceImpl ms : modelSources ) {
//    				assertNotNull(ms.getOriginConnection(getTransaction()));
//    			}
//    		}
//    	}
    }
    
    @Test
    public void shouldRefreshServiceVdb_TwoSources() throws Exception {
    	ViewEditorStateImpl[] states = helpCreateViewEditorState(2);
    	VdbImpl[] vdbs = WorkspaceManagerImpl.getInstance(_repo, getTransaction()).findVdbs(getTransaction());
    	
    	VdbImpl serviceVdb = null;
    	for( VdbImpl vdb : vdbs ) {
    		if( vdb.getName(getTransaction()).equals(viewDefinitionName)) {
    			serviceVdb = vdb;
    			break;
    		}
    	}
    	
    	ServiceVdbGenerator vdbGenerator = new ServiceVdbGenerator(WorkspaceManagerImpl.getInstance(_repo, getTransaction()));
    	
    	vdbGenerator.refreshServiceVdb(getTransaction(), serviceVdb, states);
    	
    	commit();
    	
    	Model[] models = serviceVdb.getModels(getTransaction());
    	
        assertThat(models.length, is(3));
        ModelImpl viewModel = (ModelImpl)ServiceVdbGenerator.getViewModel(getTransaction(), serviceVdb);
        assertNotNull(viewModel);
        assertThat(viewModel.getViews(getTransaction()).length, is(1));
    	View view = viewModel.getViews(getTransaction())[0];
    	assertNotNull(view);
    	assertThat(view.getColumns(getTransaction()).length, is(4));
    	
    	
//    	for( ModelImpl model : models ) {
//    		if( model.getModelType(getTransaction()) == Type.PHYSICAL) {
//    			ModelSource[] modelSources = model.getSources(getTransaction());
//    			assertNotNull(modelSources);
//    			for( ModelSourceImpl ms : modelSources ) {
//    				assertNotNull(ms.getOriginConnection(getTransaction()));
//    			}
//    		}
//    	}
    }
    
}
