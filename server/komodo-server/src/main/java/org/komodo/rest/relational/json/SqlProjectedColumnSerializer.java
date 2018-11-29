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
