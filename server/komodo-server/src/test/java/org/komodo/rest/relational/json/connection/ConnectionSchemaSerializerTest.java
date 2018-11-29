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
package org.komodo.rest.relational.json.connection;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.rest.relational.connection.ConnectionSchema;
import org.komodo.rest.relational.json.AbstractSerializerTest;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;

public class ConnectionSchemaSerializerTest extends AbstractSerializerTest {

    private static final String CONNECTION_SCHEMA = EMPTY_STRING +
        OPEN_BRACE + NEW_LINE +
            tab(1) + q("keng__id") + colon() +  q("connection") + COMMA + NEW_LINE +
            tab(1) + q("keng__kType") + colon() +  q("Connection") + COMMA + NEW_LINE +
            tab(1) + q("keng__description") + colon() +  q("Describes the configuration for a connection") + COMMA + NEW_LINE +
            tab(1) + q("keng__properties") + colon() +  OPEN_BRACE + NEW_LINE +
                tab(2) + q("dv__jndiName") + colon() +  OPEN_BRACE + NEW_LINE +
                    tab(3) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                    tab(3) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                    tab(3) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                tab(2) + CLOSE_BRACE + COMMA + NEW_LINE +
                tab(2) + q("dv__driverName") + colon() +  OPEN_BRACE + NEW_LINE +
                    tab(3) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                    tab(3) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                    tab(3) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                tab(2) + CLOSE_BRACE + COMMA + NEW_LINE +
                tab(2) + q("property") + colon() +  OPEN_BRACE + NEW_LINE +
                    tab(3) + q("keng__properties") + colon() +  OPEN_BRACE + NEW_LINE +
                        tab(4) + q("name") + colon() +  OPEN_BRACE + NEW_LINE +
                            tab(5) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                            tab(5) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                            tab(5) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                        tab(4) + CLOSE_BRACE + COMMA + NEW_LINE +
                        tab(4) + q("value") + colon() +  OPEN_BRACE + NEW_LINE +
                            tab(5) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                            tab(5) + q("keng__required") + colon() +  true + COMMA + NEW_LINE +
                            tab(5) + q("keng__repeatable") + colon() +  false + NEW_LINE +
                        tab(4) + CLOSE_BRACE + NEW_LINE +
                    tab(3) + CLOSE_BRACE + COMMA + NEW_LINE +
                    tab(3) + q("keng__type") + colon() +  q("string") + COMMA + NEW_LINE +
                    tab(3) + q("keng__required") + colon() +  false + COMMA + NEW_LINE +
                    tab(3) + q("keng__repeatable") + colon() +  true + COMMA + NEW_LINE +
                    tab(3) + q("keng__limit") + colon() + "-1" + NEW_LINE +
                tab(2) + CLOSE_BRACE + NEW_LINE +
            tab(1) + CLOSE_BRACE + NEW_LINE +
        CLOSE_BRACE;

    @Test
    public void testConnectionSchema() {
        ConnectionSchema schema = new ConnectionSchema();

        String expected = CONNECTION_SCHEMA
                                            .replaceAll(NEW_LINE,  SPACE)
                                            .replaceAll(TAB, SPACE)
                                            .replaceAll(SPACE, EMPTY_STRING);

        String output = KomodoJsonMarshaller.marshall(schema, true)
                                            .replaceAll(NEW_LINE,  SPACE)
                                            .replaceAll(TAB, SPACE)
                                            .replaceAll(SPACE, EMPTY_STRING);

        assertEquals(expected, output);
    }
}
