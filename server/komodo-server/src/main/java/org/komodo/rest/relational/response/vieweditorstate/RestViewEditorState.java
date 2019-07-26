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
package org.komodo.rest.relational.response.vieweditorstate;

import java.net.URI;

import org.komodo.relational.dataservice.ViewDefinition;
import org.komodo.rest.AbstractKEntity;
import org.komodo.spi.KException;

public class RestViewEditorState extends AbstractKEntity {

    /**
     * Label used for view editor state id
     */
    public static final String ID_LABEL = "id";

    /**
     * Label used for view definition start content
     */
    public static final String VIEW_DEFINITION_LABEL = "viewDefinition";
    
    /**
     * Label used for view definition name
     */
    public static final String ID_NAME = "name";
    
    /**
     * Label used for view definition name
     */
    public static final String ID_VIEW_NAME = "viewName";
    
    /**
     * Label used for view definition description
     */
    public static final String ID_DESCRIPTION = "description";
    
    /**
     * isComplete property
     */
    public static final String IS_COMPLETE = "isComplete"; //$NON-NLS-1$

    /**
     * isUserDefined property
     */
    public static final String IS_USER_DEFINED = "isUserDefined"; //$NON-NLS-1$
    
    /**
     * Label used for view definition source table paths array
     */
    public static final String SOURCE_PATHS = "sourcePaths";
    
    /**
     * Label used for view definition compositions array
     */
    public static final String COMPOSITIONS_LABEL = "compositions";
    /**
     * label used for sql composition left source path value
     */
    public static final String LEFT_SOURCE_PATH_LABEL = "leftSourcePath";
    /**
     * label used for sql composition right source path value
     */
    public static final String RIGHT_SOURCE_PATH_LABEL = "rightSourcePath";
    /**
     * label used for sql composition left criteria column value
     */
    public static final String LEFT_CRITERIA_COLUMN_LABEL = "leftCriteriaColumn";
    /**
     * label used for sql composition right criteria column value
     */
    public static final String RIGHT_CRITERIA_COLUMN_LABEL = "rightCriteriaColumn";
    /**
     * label used for sql composition type value
     */
    public static final String TYPE_LABEL = "type";
    /**
     * label used for sql composition operator value
     */
    public static final String OPERATOR_LABEL = "operator";
    
    /**
     * Label used for view definition projected columns array
     */
    public static final String PROJECTED_COLUMNS_LABEL = "projectedColumns";

    /*
     * the view definition
     */
    private RestViewDefinition viewDefinition;
    
    /**
     * Constructor for use when deserializing
     */
    public RestViewEditorState() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param viewEditorState the view editor state
     * @throws KException if error occurs
     */
    public RestViewEditorState(URI baseUri, ViewDefinition viewEditorState) throws KException {
        super(baseUri);

        setId(viewEditorState.getName());
        
        this.viewDefinition = new RestViewDefinition(baseUri, viewEditorState);
    }

    /**
     * @return the id
     */
    public String getId() {
        Object id = tuples.get(ID_LABEL);
        return id != null ? id.toString() : null;
    }

    public void setId(String name) {
        tuples.put(ID_LABEL, name);
    }
    
    /**
     * @return the viewDefinition
     */
    public RestViewDefinition getViewDefinition() {
        return viewDefinition;
    }

    public void setViewDefinition(RestViewDefinition viewDefinition) {
        this.viewDefinition = viewDefinition;
    }

}