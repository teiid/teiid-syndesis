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
package org.komodo.rest.relational.json;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.Test;
import org.komodo.KException;
import org.komodo.datavirtualization.SqlComposition;
import org.komodo.datavirtualization.SqlProjectedColumn;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlProjectedColumn;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;

public class ViewEditorStateSerializerTest extends AbstractSerializerTest {

    private String viewName = "myNewView";
    private String untitledName = "untitled";
    private String oldNameKey = "oldName";
    private String newNameKey = "newName";
    private String viewDefinitionName = "testView";
    private String description = "test view description text";
    private boolean isComplete = true;
    private boolean isUserDefined = false;
    private String sourceTablePath1 = "path/to/source1";
    private String sourceTablePath2 = "path/to/source2";
    private String sourceTablePath3 = "path/to/source3";
    private String sourceTablePath4 = "path/to/source4";
    private String comp1Name = "comp1";
    private String comp1Desc = "description for comp1";
    private String comp1LeftSource = "path/to/source1";
    private String comp1RightSource = "path/to/source2";
    private String comp1LeftColumn = "column1";
    private String comp1RightColumn = "column2";
    private String comp1Type = "INNER_JOIN";
    private String comp1Operator = "EQ";
    private String comp2Name = "comp2";
    private String comp2Desc = "description for comp2";
    private String comp2LeftSource = "path/to/source3";
    private String comp2RightSource = "path/to/source4";
    private String comp2LeftColumn = "column3";
    private String comp2RightColumn = "column4";
    private String comp2Type = "LEFT_OUTER_JOIN";
    private String comp2Operator = "EQ";
    private String column1Name = "col1";
    private String column1Type = "string";
    private boolean column1Selected = false;
    private String column2Name = "col2";
    private String column2Type = "integer";
    private boolean column2Selected = true;
 
    private String createViewEditorState() {
        String state = EMPTY_STRING +
            OPEN_BRACE + NEW_LINE +
                TAB + q(RestViewEditorState.BASE_URI) + colon() + q(MY_BASE_URI) + pnl(COMMA) +
                TAB + q(RestViewEditorState.ID_LABEL) + colon() + q(viewName) + pnl(COMMA) +
                // viewDefinition child 
                TAB + q(RestViewEditorState.VIEW_DEFINITION_LABEL) + colon() + pnl(OPEN_BRACE ) + 
                	tab(2) + q(RestViewEditorState.BASE_URI) + colon() + q(MY_BASE_URI) + pnl(COMMA) +
                	tab(2) + q(RestViewEditorState.ID_VIEW_NAME) + colon() + q(viewDefinitionName) + pnl(COMMA) +
                	tab(2) + q(RestViewEditorState.DESCRIPTION) + colon() + q(description) + pnl(COMMA) +
                	tab(2) + q(RestViewEditorState.IS_COMPLETE) + colon() + isComplete + pnl(COMMA) +
                	tab(2) + q(RestViewEditorState.IS_USER_DEFINED) + colon() + isUserDefined + pnl(COMMA) +
                	tab(2) + q(RestViewEditorState.SOURCE_PATHS) + colon() + pnl(OPEN_SQUARE_BRACKET) +
                		tab(3) + q(sourceTablePath1) + pnl(COMMA) +
                		tab(3) + q(sourceTablePath2) + pnl(COMMA) +
                		tab(3) + q(sourceTablePath3) + pnl(COMMA) +
                		tab(3) + q(sourceTablePath4) + NEW_LINE +
                		tab(2) + pnl(CLOSE_SQUARE_BRACKET + COMMA) +
                
                        // compositions array
                        tab(2) + q(RestViewEditorState.COMPOSITIONS_LABEL) + colon() + pnl(OPEN_SQUARE_BRACKET) + 
                            tab(3) + OPEN_BRACE + NEW_LINE +
                                tab(4) + q(RestViewEditorState.ID_NAME) + colon() + q(comp1Name) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.ID_DESCRIPTION) + colon() + q(comp1Desc) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.LEFT_SOURCE_PATH_LABEL) + colon() + q(comp1LeftSource) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL) + colon() + q(comp1RightSource) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL) + colon() + q(comp1LeftColumn) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL) + colon() + q(comp1RightColumn) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.TYPE_LABEL) + colon() + q(comp1Type) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.OPERATOR_LABEL) + colon() + q(comp1Operator) +
                            tab(3) + CLOSE_BRACE + pnl(COMMA) +
                            tab(3) + OPEN_BRACE + NEW_LINE +
                                tab(4) + q(RestViewEditorState.ID_NAME) + colon() + q(comp2Name) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.ID_DESCRIPTION) + colon() + q(comp2Desc) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.LEFT_SOURCE_PATH_LABEL) + colon() + q(comp2LeftSource) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL) + colon() + q(comp2RightSource) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL) + colon() + q(comp2LeftColumn) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL) + colon() + q(comp2RightColumn) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.TYPE_LABEL) + colon() + q(comp2Type) + pnl(COMMA) +
                                tab(4) + q(RestViewEditorState.OPERATOR_LABEL) + colon() + q(comp2Operator) + NEW_LINE +
                            tab(3) + CLOSE_BRACE + NEW_LINE +
                        tab(2) + pnl(CLOSE_SQUARE_BRACKET + COMMA) +   

                        // projected columns array
                        tab(2) + q(RestViewEditorState.PROJECTED_COLUMNS_LABEL) + colon() + pnl(OPEN_SQUARE_BRACKET) + 
                            tab(3) + OPEN_BRACE + NEW_LINE +
                                tab(4) + q(RestSqlProjectedColumn.NAME_LABEL) + colon() + q(column1Name) + pnl(COMMA) +
                                tab(4) + q(RestSqlProjectedColumn.TYPE_LABEL) + colon() + q(column1Type) + pnl(COMMA) +
                                tab(4) + q(RestSqlProjectedColumn.SELECTED_LABEL) + colon() + column1Selected +
                            tab(3) + CLOSE_BRACE + pnl(COMMA) +
                            tab(3) + OPEN_BRACE + NEW_LINE +
                                tab(4) + q(RestSqlProjectedColumn.NAME_LABEL) + colon() + q(column2Name) + pnl(COMMA) +
                                tab(4) + q(RestSqlProjectedColumn.TYPE_LABEL) + colon() + q(column2Type) + pnl(COMMA) +
                                tab(4) + q(RestSqlProjectedColumn.SELECTED_LABEL) + colon() + column2Selected + NEW_LINE +
                            tab(3) + CLOSE_BRACE + NEW_LINE +
                        tab(2) + pnl(CLOSE_SQUARE_BRACKET) +   
            	TAB + CLOSE_BRACE +
            CLOSE_BRACE;

        return state;
    }

    @Test
    public void shouldImportJson() {
        String state = createViewEditorState();

        RestViewEditorState viewEditorState = KomodoJsonMarshaller.unmarshall(state, RestViewEditorState.class);
        assertEquals(viewName, viewEditorState.getId());
        
        RestViewDefinition viewDef = viewEditorState.getViewDefinition();
        assertNotNull(viewDef);
        assertEquals(viewDefinitionName, viewDef.getViewName());
        
        String[] paths = viewDef.getSourcePaths();
        assertNotNull(paths);
        assertEquals(4, paths.length);
        
        RestSqlComposition[] comps = viewDef.getSqlCompositions(); 
        assertNotNull(comps);
        assertEquals(2, comps.length);
    }

    @Test
    public void shouldExportJson() throws KException {
        String newName = viewName;

        Map<String, String> undoArgs = new LinkedHashMap<>();
        undoArgs.put(oldNameKey, newName);
        undoArgs.put(newNameKey, untitledName);

        Map<String, String> redoArgs = new LinkedHashMap<>();
        redoArgs.put(oldNameKey, untitledName);
        redoArgs.put(newNameKey, newName);

        String[] sourceTablePaths = { sourceTablePath1, sourceTablePath2, sourceTablePath3, sourceTablePath4 };
        ViewDefinition viewDef = mock(ViewDefinition.class);
        when(viewDef.getName()).thenReturn(viewName);
        when(viewDef.getViewName()).thenReturn(viewDefinitionName);
        when(viewDef.getDescription()).thenReturn(description);
        when(viewDef.isComplete()).thenReturn(isComplete);
        when(viewDef.isUserDefined()).thenReturn(isUserDefined);
        when(viewDef.getSourcePaths()).thenReturn(Arrays.asList(sourceTablePaths));
        
        // Mocks for Compositions
        SqlComposition sqlComp1 = mock(SqlComposition.class);
        when(sqlComp1.getName()).thenReturn(comp1Name);
        when(sqlComp1.getDescription()).thenReturn(comp1Desc);
        when(sqlComp1.getLeftSourcePath()).thenReturn(comp1LeftSource);
        when(sqlComp1.getRightSourcePath()).thenReturn(comp1RightSource);
        when(sqlComp1.getLeftCriteriaColumn()).thenReturn(comp1LeftColumn);
        when(sqlComp1.getRightCriteriaColumn()).thenReturn(comp1RightColumn);
        when(sqlComp1.getType()).thenReturn(comp1Type);
        when(sqlComp1.getOperator()).thenReturn(comp1Operator);
        
        SqlComposition sqlComp2 = mock(SqlComposition.class);
        when(sqlComp2.getName()).thenReturn(comp2Name);
        when(sqlComp2.getDescription()).thenReturn(comp2Desc);
        when(sqlComp2.getLeftSourcePath()).thenReturn(comp2LeftSource);
        when(sqlComp2.getRightSourcePath()).thenReturn(comp2RightSource);
        when(sqlComp2.getLeftCriteriaColumn()).thenReturn(comp2LeftColumn);
        when(sqlComp2.getRightCriteriaColumn()).thenReturn(comp2RightColumn);
        when(sqlComp2.getType()).thenReturn(comp2Type);
        when(sqlComp2.getOperator()).thenReturn(comp2Operator);
        
        SqlComposition[] sqlComps = { sqlComp1, sqlComp2 };
        when(viewDef.getSqlCompositions()).thenReturn(Arrays.asList(sqlComps));

        // Mocks for projected columns
        SqlProjectedColumn sqlCol1 = mock(SqlProjectedColumn.class);
        when(sqlCol1.getName()).thenReturn(column1Name);
        when(sqlCol1.getType()).thenReturn(column1Type);
        when(sqlCol1.isSelected()).thenReturn(column1Selected);

        SqlProjectedColumn sqlCol2 = mock(SqlProjectedColumn.class);
        when(sqlCol2.getName()).thenReturn(column2Name);
        when(sqlCol2.getType()).thenReturn(column2Type);
        when(sqlCol2.isSelected()).thenReturn(column2Selected);
        
        SqlProjectedColumn[] sqlCols = { sqlCol1, sqlCol2 };
        when(viewDef.getProjectedColumns()).thenReturn(Arrays.asList(sqlCols));

        RestViewEditorState restState = new RestViewEditorState(MY_BASE_URI, viewDef);

        String expectedJson = createViewEditorState()
                                                    .replaceAll(NEW_LINE,  SPACE)
                                                    .replaceAll(TAB, SPACE)
                                                    .replaceAll(SPACE, EMPTY_STRING);
        
        String resultJson = KomodoJsonMarshaller.marshall(restState)
                                                    .replaceAll(NEW_LINE,  SPACE)
                                                    .replaceAll(TAB, SPACE)
                                                    .replaceAll(SPACE, EMPTY_STRING);
        assertEquals(expectedJson, resultJson);
    }  
}
