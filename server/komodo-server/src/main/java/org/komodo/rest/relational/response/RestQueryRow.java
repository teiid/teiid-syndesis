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
package org.komodo.rest.relational.response;

import java.sql.Clob;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.ws.rs.core.MediaType;

import org.komodo.metadata.query.QSRow;
import org.komodo.rest.KRestEntity;

public class RestQueryRow implements KRestEntity {

    /**
     * Label for row
     */
    public static final String ROW_LABEL = "row";

    private List<String> values;

    /**
     * Constructor for use when deserializing
     */
    public RestQueryRow() {
        super();
    }

    public RestQueryRow(QSRow qsRow) {
        if (qsRow == null)
            this.values = Collections.emptyList();
        else {
            this.values = new ArrayList<>();
            for (Object value : qsRow.getValues()) {
                String valueStr = EMPTY_STRING;
                if(value!=null) {
                    // Handle Clob values
                    if(value instanceof Clob) {
                        Clob valueClob = (Clob)value;
                        try {
                            long clobLength = valueClob.length();
                            if(clobLength>0) {
                                valueStr = valueClob.getSubString(1, (int)clobLength);
                            } else {
                                valueStr = EMPTY_STRING;
                            }
                        } catch (SQLException ex) {
                            valueStr = EMPTY_STRING;
                        }
                    // All other values
                    } else {
                        valueStr = value.toString();
                    }
                }
                this.values.add(valueStr);
            }
        }
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    public String[] getValues() {
        return values.toArray(new String[0]);
    }

    public void setValues(Object[] values) {
        if (values == null || values.length == 0)
            this.values = Collections.emptyList();

        this.values = new ArrayList<String>();
        for (Object value: values) {
            this.values.add(value.toString());
        }
    }
}
