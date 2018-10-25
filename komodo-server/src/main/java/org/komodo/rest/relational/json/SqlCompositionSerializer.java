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
