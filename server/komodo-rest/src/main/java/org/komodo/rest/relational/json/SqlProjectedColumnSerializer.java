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
import org.komodo.rest.relational.response.vieweditorstate.RestSqlProjectedColumn;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * Serializer for projected columns
 */
public class SqlProjectedColumnSerializer extends TypeAdapter<RestSqlProjectedColumn> {

	@Override
	public void write(JsonWriter out, RestSqlProjectedColumn restSqlProjectedColumn) throws IOException {
		out.beginObject();
		
        out.name(RestSqlProjectedColumn.NAME_LABEL);
        out.value(restSqlProjectedColumn.getName());

        out.name(RestSqlProjectedColumn.TYPE_LABEL);
        out.value(restSqlProjectedColumn.getType());
        
        out.name(RestSqlProjectedColumn.SELECTED_LABEL);
        out.value(restSqlProjectedColumn.isSelected());
		
        out.endObject();
	}

	@Override
	public RestSqlProjectedColumn read(JsonReader in) throws IOException {
	    RestSqlProjectedColumn sqlCol = new RestSqlProjectedColumn();
		
		in.beginObject();
		

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case RestSqlProjectedColumn.NAME_LABEL:
                	sqlCol.setName(in.nextString());
                    break;
                case RestSqlProjectedColumn.TYPE_LABEL:
                	sqlCol.setType(in.nextString());
                    break;
                case RestSqlProjectedColumn.SELECTED_LABEL:
                	sqlCol.setSelected(in.nextBoolean());
                    break;
                default: {
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ));
                }
            }
        }

        in.endObject();
		
		return sqlCol;
	}


}
