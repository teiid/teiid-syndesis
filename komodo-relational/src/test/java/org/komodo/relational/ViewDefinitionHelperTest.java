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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.model.Model;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;

public class ViewDefinitionHelperTest extends RelationalModelTest {
	
    private static String viewDefinitionName = "orderInfoView";
    private static String description = "test view description text";
    private boolean isComplete = true;
    private static String sourceTablePath1 = "connection=pgconnection1/schema=public/table=orders";
    private static String sourceTablePath2 = "connection=pgconnection1/schema=public/table=customers";
    private static String sourceTablePath3 = "connection=pgconnection2/schema=public/table=customers";
    private static String comp1Name = "comp1";
    private static String comp1Desc = "description for comp1";
    private static String comp1LeftSource = "pgconnection1schemamodel.orders";
    private static String comp1RightSource = "pgconnection1schemamodel.customers";
    private static String comp1LeftColumn = "ID";
    private static String comp1RightColumn = "ID";
    private static String comp1Operator = "EQ";
    
    private static String FQN_TABLE_1 = "schema=public/table=orders";
    private static String FQN_TABLE_2 = "schema=public/table=customers";
    
    private static final String DS_NAME = "pgconnection1";
    private static final String VDB_NAME = "pgconnection1schemavdb";
    private static final String MODEL_NAME = "pgconnection1schemamodel";
    private static final String DS_NAME_2 = "pgconnection2";
    private static final String VDB_NAME_2 = "pgconnection2schemavdb";
    private static final String MODEL_NAME_2 = "pgconnection2schemamodel";
    
    
    private final static String TABLE_OPTION_FQN = "teiid_relfqn"; //$NON-NLS-1$
    
    
    
    private final static String pgconnection1schemamodelDDL = 
    		"CREATE FOREIGN TABLE orders ( "
    		+ "ID long, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_1 + "\");\n" +
    		"CREATE FOREIGN TABLE customers ( "
    		+ "ID long, name string) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_2 + "\");";
    
    private final static String pgconnection2schemamodelDDL = 
    		"CREATE FOREIGN TABLE orders ( "
    		+ "ID long, orderDate timestamp) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_1 + "\");\n" +
    		"CREATE FOREIGN TABLE customers ( "
    		+ "ID long, customerName string) OPTIONS(\"" + TABLE_OPTION_FQN + "\" \"" + FQN_TABLE_2 + "\");";
    
    private final static String EXPECTED_JOIN_SQL_TWO_SOURCES_START =
            "CREATE VIEW orderInfoView (RowId integer PRIMARY KEY, ID LONG, orderDate TIMESTAMP, customerName STRING) AS \n"
          + "SELECT ROW_NUMBER() OVER (ORDER BY A.ID), A.ID, A.orderDate, B.customerName\n"
          + "FROM pgconnection1schemamodel.orders AS A \n";
    
    private final static String  EXPECTED_JOIN_SQL_TWO_SOURCES_END = "pgconnection2schemamodel.customers AS B \n"
            + "ON \n"
            + "A.ID = B.ID;";
    
    private final static String EXPECTED_JOIN_SQL_SINGE_SOURCE_START =
            "CREATE VIEW orderInfoView (RowId integer PRIMARY KEY, ID LONG, orderDate TIMESTAMP, name STRING) AS \n"
          + "SELECT ROW_NUMBER() OVER (ORDER BY ID), ID, orderDate, name\n"
          + "FROM orders \n";
    
    private final static String  EXPECTED_JOIN_SQL_SINGLE_SOURCE_END = "customers \n"
          + "ON \n"
          + "orders.ID = customers.ID;";
    
    String EXPECTED_DDL =
            "CREATE VIEW orderInfoView (RowId integer PRIMARY KEY, ID LONG, orderDate TIMESTAMP, customerName STRING) AS \n"
          + "SELECT ROW_NUMBER() OVER (ORDER BY A.ID), A.ID, A.orderDate, B.customerName\n"
          + "FROM pgconnection1schemamodel.orders AS A \n"
          + "INNER JOIN \n"
          + "pgconnection2schemamodel.customers AS B \n"
          + "ON \n"
          + "A.ID = B.ID;";
    
    private final static String INNER_JOIN_STR = "INNER JOIN \n";
    private final static String LEFT_OUTER_JOIN_STR = "LEFT OUTER JOIN \n";
    private final static String RIGHT_OUTER_JOIN_STR = "RIGHT OUTER JOIN \n";
    private final static String FULL_OUTER_JOIN_STR = "FULL OUTER JOIN \n";

    @Before
    public void init() throws Exception {
        Connection connection = createConnection( DS_NAME );
        connection.setDescription(getTransaction(), description);

        final String extLoc = "new-external-location";
        connection.setExternalLocation(getTransaction(), extLoc );

        connection.setJdbc(getTransaction(), false);
        connection.setJndiName(getTransaction(), "java:/jndiName");
        connection.setDriverName(getTransaction(), "dsDriver");
        connection.setClassName(getTransaction(), "dsClassname");
        connection.setProperty(getTransaction(), "prop1", "prop1Value");
        connection.setProperty(getTransaction(), "prop2", "prop2Value");

        // Create a VDB for this connection
        Vdb sourceVdb = createVdb(VDB_NAME, connection, "originalFilePath");
        Model sourceModel = sourceVdb.addModel(getTransaction(), MODEL_NAME);
        sourceModel.setModelDefinition(getTransaction(), pgconnection1schemamodelDDL);
        commit();
        
        connection = createConnection( DS_NAME_2 );
        connection.setDescription(getTransaction(), description);
        
        connection.setExternalLocation(getTransaction(), extLoc );
        
        connection.setJdbc(getTransaction(), false);
        connection.setJndiName(getTransaction(), "java:/jndiName2");
        connection.setDriverName(getTransaction(), "dsDriver");
        connection.setClassName(getTransaction(), "dsClassname2");
        connection.setProperty(getTransaction(), "prop1", "prop1Value");
        connection.setProperty(getTransaction(), "prop2", "prop2Value");

        // Create a VDB for this connection
        sourceVdb = createVdb(VDB_NAME_2, connection, "originalFilePath");
        sourceModel = sourceVdb.addModel(getTransaction(), MODEL_NAME_2);
        
        // Rather than creating model objects piecemeal, it's easier to have komodo & modeshape do it via
        // setting the model definition via Teiid DDL statements
        sourceModel.setModelDefinition(getTransaction(), pgconnection2schemamodelDDL);
        commit();
    }
    
    private String helpGenerateDdlForWithJoinType(String secondSourceTablePath, String joinType) throws KException {
    	
        ViewDefinitionHelper helper = new ViewDefinitionHelper(WorkspaceManager.getInstance(_repo, getTransaction()));

        String[] sourceTablePaths = { sourceTablePath1, secondSourceTablePath };
        ViewDefinition viewDef = mock(ViewDefinition.class);
        when(viewDef.getName(getTransaction())).thenReturn("vdbDefinition");
        when(viewDef.getViewName(getTransaction())).thenReturn(viewDefinitionName);
        when(viewDef.getDescription(getTransaction())).thenReturn(description);
        when(viewDef.isComplete(getTransaction())).thenReturn(isComplete);
        when(viewDef.getSourcePaths(getTransaction())).thenReturn(sourceTablePaths);
        
        SqlComposition sqlComp1 = mock(SqlComposition.class);
        when(sqlComp1.getName(getTransaction())).thenReturn(comp1Name);
        when(sqlComp1.getDescription(getTransaction())).thenReturn(comp1Desc);
        when(sqlComp1.getLeftSourcePath(getTransaction())).thenReturn(comp1LeftSource);
        when(sqlComp1.getRightSourcePath(getTransaction())).thenReturn(comp1RightSource);
        when(sqlComp1.getLeftCriteriaColumn(getTransaction())).thenReturn(comp1LeftColumn);
        when(sqlComp1.getRightCriteriaColumn(getTransaction())).thenReturn(comp1RightColumn);
        when(sqlComp1.getType(getTransaction())).thenReturn(joinType);
        when(sqlComp1.getOperator(getTransaction())).thenReturn(comp1Operator);
        
        SqlComposition[] sqlComps = new SqlComposition[1];
        sqlComps[0] = sqlComp1;
        when(viewDef.getSqlCompositions(getTransaction())).thenReturn(sqlComps);

        return helper.getODataViewDdl(getTransaction(), viewDef);
    }

    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_InnerJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + INNER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ViewDefinitionHelper.JOIN_INNER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_LeftOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + LEFT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ViewDefinitionHelper.JOIN_LEFT_OUTER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_RightOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + RIGHT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ViewDefinitionHelper.JOIN_RIGHT_OUTER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithSingleSourceViewDefinition_FullOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_SINGE_SOURCE_START + FULL_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_SINGLE_SOURCE_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath2, ViewDefinitionHelper.JOIN_FULL_OUTER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_InnerJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + INNER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ViewDefinitionHelper.JOIN_INNER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_LeftOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + LEFT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ViewDefinitionHelper.JOIN_LEFT_OUTER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_RightOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + RIGHT_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ViewDefinitionHelper.JOIN_RIGHT_OUTER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGenerateOdataViewDDL_WithTwoSourcesViewDefinition_FullOuterJoin() throws Exception {
    	String EXPECTED_DDL = EXPECTED_JOIN_SQL_TWO_SOURCES_START + FULL_OUTER_JOIN_STR + EXPECTED_JOIN_SQL_TWO_SOURCES_END;
        String viewDdl = helpGenerateDdlForWithJoinType(sourceTablePath3, ViewDefinitionHelper.JOIN_FULL_OUTER);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
}
