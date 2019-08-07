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
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;

import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlProjectedColumn;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;



public class ViewDefinitionSerializer extends TypeAdapter<RestViewDefinition> {

	@Override
	public void write(JsonWriter out, RestViewDefinition restViewDef) throws IOException {
		out.beginObject();
		
		out.name(RestViewEditorState.ID_VIEW_NAME);
		out.value(restViewDef.getViewName());
		if( restViewDef.getDescription() != null ) {
			out.name(RestViewEditorState.DESCRIPTION);
			out.value(restViewDef.getDescription());
		}
		if( restViewDef.getDdl() != null ) {
			out.name(RestViewEditorState.DDL);
			out.value(restViewDef.getDdl());
		}
		
		out.name(RestViewEditorState.IS_COMPLETE);
		out.value(restViewDef.isComplete());

		out.name(RestViewEditorState.IS_USER_DEFINED);
		out.value(restViewDef.isUserDefined());
		
		if( restViewDef.getSourcePaths() != null ) {
			out.name(RestViewEditorState.SOURCE_PATHS);
			out.beginArray();
			String[] paths = restViewDef.getSourcePaths();
			for( String path : paths ) {
				out.value(path);
			}
			out.endArray();
		}
		
        RestSqlComposition[] comps = restViewDef.getSqlCompositions();
        if( comps.length != 0 ) {
            out.name(RestViewEditorState.COMPOSITIONS_LABEL);
            out.beginArray();
            for( RestSqlComposition comp : comps) {
                BUILDER.getAdapter(RestSqlComposition.class).write(out, comp);
            }
            out.endArray();
        }

        RestSqlProjectedColumn[] cols = restViewDef.getProjectedColumns();
        if( cols.length != 0 ) {
            out.name(RestViewEditorState.PROJECTED_COLUMNS_LABEL);
            out.beginArray();
            for( RestSqlProjectedColumn col : cols) {
                BUILDER.getAdapter(RestSqlProjectedColumn.class).write(out, col);
            }
            out.endArray();
        }
		out.endObject();
	}

	@Override
	public RestViewDefinition read(JsonReader in) throws IOException {
		RestViewDefinition viewDef = new RestViewDefinition();
		
		in.beginObject();
		

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestViewEditorState.ID_VIEW_NAME:
                    viewDef.setViewName(in.nextString());
                    break;
                case RestViewEditorState.DESCRIPTION:
                	viewDef.setDescription(in.nextString());
                    break;
                case RestViewEditorState.DDL:
                	viewDef.setDdl(in.nextString());
                    break;
                case RestViewEditorState.SOURCE_PATHS:
                    String[] sourcePaths = BUILDER.fromJson(in, String[].class);
                    viewDef.setSourcePaths(sourcePaths);
                    break;
                case RestViewEditorState.IS_COMPLETE:
                    viewDef.setComplete(in.nextBoolean());
                    break;
                case RestViewEditorState.IS_USER_DEFINED:
                    viewDef.setUserDefined(in.nextBoolean());
                    break;
                case RestViewEditorState.COMPOSITIONS_LABEL:
                    RestSqlComposition[] comps = BUILDER.fromJson(in, RestSqlComposition[].class);
                    viewDef.setSqlCompositions(comps);
                    break;
                case RestViewEditorState.PROJECTED_COLUMNS_LABEL:
                    RestSqlProjectedColumn[] cols = BUILDER.fromJson(in, RestSqlProjectedColumn[].class);
                    viewDef.setProjectedColumns(cols);
                    break;
                default: {
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ));
                }
            }
        }

        in.endObject();
		
		return viewDef;
	}


}
