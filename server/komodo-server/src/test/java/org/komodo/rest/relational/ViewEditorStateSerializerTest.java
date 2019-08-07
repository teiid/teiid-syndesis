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
package org.komodo.rest.relational;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.komodo.KException;
import org.komodo.datavirtualization.SqlComposition;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.rest.KomodoJsonMarshaller;

public class ViewEditorStateSerializerTest {
	
    private String viewName = "myNewView";
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
        return "{\n" + 
        		"  \"id\" : \"myNewView\",\n" +
        		"  \"name\" : \"testView\",\n" + 
        		"  \"dataVirtualizationName\" : \"dvName\",\n" + 
        		"  \"sourcePaths\" : [ \"path/to/source1\", \"path/to/source2\", \"path/to/source3\", \"path/to/source4\" ],\n" + 
        		"  \"compositions\" : [ {\n" + 
        		"    \"name\" : \"comp1\",\n" + 
        		"    \"description\" : \"description for comp1\",\n" + 
        		"    \"leftSourcePath\" : \"path/to/source1\",\n" + 
        		"    \"leftCriteriaColumn\" : \"column1\",\n" + 
        		"    \"rightSourcePath\" : \"path/to/source2\",\n" + 
        		"    \"rightCriteriaColumn\" : \"column2\",\n" + 
        		"    \"operator\" : \"EQ\",\n" + 
        		"    \"type\" : \"INNER_JOIN\"\n" + 
        		"  }, {\n" + 
        		"    \"name\" : \"comp2\",\n" + 
        		"    \"description\" : \"description for comp2\",\n" + 
        		"    \"leftSourcePath\" : \"path/to/source3\",\n" + 
        		"    \"leftCriteriaColumn\" : \"column3\",\n" + 
        		"    \"rightSourcePath\" : \"path/to/source4\",\n" + 
        		"    \"rightCriteriaColumn\" : \"column4\",\n" + 
        		"    \"operator\" : \"EQ\",\n" + 
        		"    \"type\" : \"LEFT_OUTER_JOIN\"\n" + 
        		"  } ],\n" + 
        		"  \"projectedColumns\" : [ {\n" + 
        		"    \"name\" : \"col1\",\n" + 
        		"    \"type\" : \"string\",\n" + 
        		"    \"selected\" : false\n" + 
        		"  }, {\n" + 
        		"    \"name\" : \"col2\",\n" + 
        		"    \"type\" : \"integer\",\n" + 
        		"    \"selected\" : true\n" + 
        		"  } ],\n" + 
        		"  \"keng__description\" : \"test view description text\",\n" + 
        		"  \"isComplete\" : true,\n" + 
        		"  \"isUserDefined\" : false\n" + 
        		"}";
    }

    @Test
    public void shouldImportJson() {
        String state = createViewEditorState();

        ViewDefinition viewEditorState = KomodoJsonMarshaller.unmarshall(state, org.komodo.datavirtualization.ViewDefinition.class);
        assertEquals(viewName, viewEditorState.getId());
        
        assertNotNull(viewEditorState);
        assertEquals(viewDefinitionName, viewEditorState.getName());
        
        List<String> paths = viewEditorState.getSourcePaths();
        assertNotNull(paths);
        assertEquals(4, paths.size());
        
        List<SqlComposition> comps = viewEditorState.getCompositions(); 
        assertNotNull(comps);
        assertEquals(2, comps.size());
    }

    @Test
    public void shouldExportJson() throws KException {
        String[] sourceTablePaths = { sourceTablePath1, sourceTablePath2, sourceTablePath3, sourceTablePath4 };
        org.komodo.datavirtualization.ViewDefinition viewDef = new org.komodo.datavirtualization.ViewDefinition("dvName", viewDefinitionName);
        viewDef.setId(viewName);
        viewDef.setDescription(description);
        viewDef.setComplete(isComplete);
        viewDef.setUserDefined(isUserDefined);
        viewDef.setSourcePaths(Arrays.asList(sourceTablePaths));
        
        org.komodo.datavirtualization.SqlComposition sqlComp1 = new org.komodo.datavirtualization.SqlComposition(comp1Name);
        sqlComp1.setDescription(comp1Desc);
        sqlComp1.setLeftSourcePath(comp1LeftSource);
        sqlComp1.setRightSourcePath(comp1RightSource);
        sqlComp1.setLeftCriteriaColumn(comp1LeftColumn);
        sqlComp1.setRightCriteriaColumn(comp1RightColumn);
        sqlComp1.setType(comp1Type);
        sqlComp1.setOperator(comp1Operator);
        
        org.komodo.datavirtualization.SqlComposition sqlComp2 = new org.komodo.datavirtualization.SqlComposition(comp2Name);
        sqlComp2.setDescription(comp2Desc);
        sqlComp2.setLeftSourcePath(comp2LeftSource);
        sqlComp2.setRightSourcePath(comp2RightSource);
        sqlComp2.setLeftCriteriaColumn(comp2LeftColumn);
        sqlComp2.setRightCriteriaColumn(comp2RightColumn);
        sqlComp2.setType(comp2Type);
        sqlComp2.setOperator(comp2Operator);

        viewDef.setCompositions(Arrays.asList(sqlComp1, sqlComp2));

        org.komodo.datavirtualization.SqlProjectedColumn sqlCol1 = new org.komodo.datavirtualization.SqlProjectedColumn(column1Name);
        sqlCol1.setName(column1Name);
        sqlCol1.setType(column1Type);
        sqlCol1.setSelected(column1Selected);

        org.komodo.datavirtualization.SqlProjectedColumn sqlCol2 = new org.komodo.datavirtualization.SqlProjectedColumn(column2Name);
        sqlCol2.setName(column2Name);
        sqlCol2.setType(column2Type);
        sqlCol2.setSelected(column2Selected);

        viewDef.setProjectedColumns(Arrays.asList(sqlCol1, sqlCol2));

        String expectedJson = createViewEditorState();
        
        String resultJson = KomodoJsonMarshaller.marshall(viewDef);
        
        assertEquals(expectedJson, resultJson);
    }  
}
