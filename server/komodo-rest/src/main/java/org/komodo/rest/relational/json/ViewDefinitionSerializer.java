/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;

import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;



public class ViewDefinitionSerializer extends TypeAdapter<RestViewDefinition> {

	@Override
	public void write(JsonWriter out, RestViewDefinition restViewDef) throws IOException {
		out.beginObject();
		
		out.name(RestViewEditorState.BASE_URI);
		out.value(restViewDef.getBaseUri().toString());
		
		out.name(RestViewEditorState.ID_VIEW_NAME);
		out.value(restViewDef.getViewName());
		if( restViewDef.getDescription() != null ) {
			out.name(RestViewEditorState.DESCRIPTION);
			out.value(restViewDef.getDescription());
		}
		
		out.name(RestViewEditorState.IS_COMPLETE);
		out.value(Boolean.toString(restViewDef.isComplete()));
		
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
//        		BUILDER.getAdapter(RestSqlComposition.class).write(out, comp);
                out.beginObject();

                out.name(RestViewEditorState.ID_NAME);
                out.value((String)comp.getTuples().get(RestViewEditorState.ID_NAME));
                out.name(RestViewEditorState.ID_DESCRIPTION);
                out.value((String)comp.getTuples().get(RestViewEditorState.DESCRIPTION));
                out.name(RestViewEditorState.LEFT_SOURCE_PATH_LABEL);
                out.value(comp.getLeftSourcePath());
                out.name(RestViewEditorState.RIGHT_SOURCE_PATH_LABEL);
                out.value(comp.getRightSourcePath());
                out.name(RestViewEditorState.LEFT_CRITERIA_COLUMN_LABEL);
                out.value(comp.getLeftCriteriaColumn());
                out.name(RestViewEditorState.RIGHT_CRITERIA_COLUMN_LABEL);
                out.value(comp.getRightCriteriaColumn());
                out.name(RestViewEditorState.TYPE_LABEL);
                out.value(comp.getType());
                out.name(RestViewEditorState.OPERATOR_LABEL);
                out.value(comp.getOperator());

                out.endObject();
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
                case RestViewEditorState.SOURCE_PATHS:
                    String[] sourcePaths = BUILDER.fromJson(in, String[].class);
                    viewDef.setSourcePaths(sourcePaths);
                    break;
                case RestViewEditorState.IS_COMPLETE:
                    String strValue = in.nextString();
                    viewDef.setComplete(Boolean.parseBoolean(strValue));
                    break;
                case RestViewEditorState.COMPOSITIONS_LABEL:
                    RestSqlComposition[] comps = BUILDER.fromJson(in, RestSqlComposition[].class);
                    viewDef.setSqlCompositions(comps);
                    break;
                case RestViewEditorState.BASE_URI:
                	in.nextString();
                    //viewDef.getTuples().put(RestViewEditorState.BASE_URI, in.nextString());
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
