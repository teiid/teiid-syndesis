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
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.rest.AbstractKEntity;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;

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

    /**
     * Label used for view editor start content
     */
    public static final String CONTENT_LABEL = "undoables";

    /*
     * the view definition
     */
    private RestViewDefinition viewDefinition;
    
    /*
     * The contents of the view editor state
     */
    private RestStateCommandAggregate[] commands = new RestStateCommandAggregate[0];

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
     *
     * @throws KException if error occurs
     */
    public RestViewEditorState(URI baseUri, ViewEditorState viewEditorState, UnitOfWork transaction) throws KException {
        super(baseUri);

        setId(viewEditorState.getName(transaction));
        
        ViewDefinition viewDef = viewEditorState.getViewDefinition(transaction);
        if( viewDef != null ) {
        	this.viewDefinition = new RestViewDefinition(baseUri, viewDef, transaction);
        }

        List<RestStateCommandAggregate> cmdList = new ArrayList<>();
        for (StateCommandAggregate cmd : viewEditorState.getCommands(transaction)) {
            RestStateCommandAggregate restCmd = new RestStateCommandAggregate(baseUri, cmd, transaction);
            cmdList.add(restCmd);
        }

        this.commands = cmdList.toArray(new RestStateCommandAggregate[0]);
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

    /**
     * @return the content
     */
    public RestStateCommandAggregate[] getCommands() {
        return commands;
    }

    public void setCommands(RestStateCommandAggregate[] commands) {
        this.commands = commands;
    }
}