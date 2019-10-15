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
package org.komodo.rest.datavirtualization;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.rest.KomodoJsonMarshaller;

@SuppressWarnings("nls")
public class ViewDefinitionSerializerTest {

    private String viewName = "myNewView";
    private String viewDefinitionName = "testView";
    private String description = "test view description text";
    private boolean isComplete = true;
    private boolean isUserDefined = false;
    private String sourceTablePath1 = "path/to/source1";
    private String sourceTablePath2 = "path/to/source2";
    private String sourceTablePath3 = "path/to/source3";
    private String sourceTablePath4 = "path/to/source4";

    private String createViewEditorState() {
        return "{\n" +
                "  \"complete\" : true,\n" +
                "  \"dataVirtualizationName\" : \"dvName\",\n" +
                "  \"description\" : \"test view description text\",\n" +
                "  \"id\" : \"myNewView\",\n" +
                "  \"name\" : \"testView\",\n" +
                "  \"sourcePaths\" : [ \"path/to/source1\", \"path/to/source2\", \"path/to/source3\", \"path/to/source4\" ],\n" +
                "  \"userDefined\" : false\n" +
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
    }

    @Test
    public void shouldExportJson() {
        String[] sourceTablePaths = { sourceTablePath1, sourceTablePath2, sourceTablePath3, sourceTablePath4 };
        org.komodo.datavirtualization.ViewDefinition viewDef = new org.komodo.datavirtualization.ViewDefinition("dvName", viewDefinitionName);
        viewDef.setId(viewName);
        viewDef.setDescription(description);
        viewDef.setComplete(isComplete);
        viewDef.setUserDefined(isUserDefined);
        viewDef.setSourcePaths(Arrays.asList(sourceTablePaths));

        String expectedJson = createViewEditorState();

        String resultJson = KomodoJsonMarshaller.marshall(viewDef);

        assertEquals(expectedJson, resultJson);
    }
}
