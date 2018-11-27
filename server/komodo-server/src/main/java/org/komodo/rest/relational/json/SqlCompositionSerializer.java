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

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;

import java.io.IOException;

import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;



/**
 * Serializer for Sql Composition
 */
public class SqlCompositionSerializer extends TypeAdapter<RestSqlComposition> {

	@Override
	public void write(JsonWriter out, RestSqlComposition restSqlComposition) throws IOException {
		out.beginObject();
		
        out.name(RestViewEditorState.ID_NAME);
        out.value((String)restSqlComposition.getTuples().get(RestViewEditorState.ID_NAME));

        out.name(RestViewEditorState.ID_DESCRIPTION);
        out.value((String)restSqlComposition.getTuples().get(RestViewEditorState.DESCRIPTION));

        out.name(RestViewEditorState.LEFT_SOURCE_PATH_LABEL);
        out.value(restSqlComposition.getLeftSourcePath());
        
        out.name(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL);
        out.value(restSqlComposition.getRightSourcePath());
        
        out.name(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL);
        out.value(restSqlComposition.getLeftCriteriaColumn());
        
        out.name(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL);
        out.value(restSqlComposition.getRightCriteriaColumn());
        
        out.name(RestViewEditorState.TYPE_LABEL);
        out.value(restSqlComposition.getType());
        
        out.name(RestViewEditorState.OPERATOR_LABEL);
        out.value(restSqlComposition.getOperator());
		
        out.endObject();
	}

	@Override
	public RestSqlComposition read(JsonReader in) throws IOException {
		RestSqlComposition sqlComp = new RestSqlComposition();
		
		in.beginObject();
		

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestViewEditorState.ID_NAME:
                	sqlComp.setName(in.nextString());
                    break;
                case RestViewEditorState.ID_DESCRIPTION:
                	sqlComp.setDescription(in.nextString());
                    break;
                case RestViewEditorState.LEFT_SOURCE_PATH_LABEL:
                	sqlComp.setLeftSourcePath(in.nextString());
                    break;
                case RestViewEditorState.RIGHT_SOURCE_PATH_LABEL:
                	sqlComp.setRightSourcePath(in.nextString());
                    break;
                case RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL:
                	sqlComp.setLeftCriteriaColumn(in.nextString());
                    break;
                case RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL:
                	sqlComp.setRightCriteriaColumn(in.nextString());
                    break;
                case RestViewEditorState.TYPE_LABEL:
                	sqlComp.setType(in.nextString());
                    break;
                case RestViewEditorState.OPERATOR_LABEL:
                	sqlComp.setOperator(in.nextString());
                    break;
                default: {
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ));
                }
            }
        }

        in.endObject();
		
		return sqlComp;
	}


}
