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

package org.komodo.rest.relational.connection;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.komodo.rest.KomodoJsonMarshaller;
import org.komodo.rest.datavirtualization.connection.RestSchemaNode;

public class RestSchemaNodeTest {

    @Test public void testJsonRoundtrip() {
        RestSchemaNode rsn = new RestSchemaNode("x", "y", "z");
        rsn.addChild(new RestSchemaNode("a", "b", "c"));

        String value = KomodoJsonMarshaller.marshall(rsn);
        String expected = "{\n" +
                "  \"children\" : [ {\n" +
                "    \"children\" : [ ],\n" +
                "    \"name\" : \"b\",\n" +
                "    \"connectionName\" : \"a\",\n" +
                "    \"type\" : \"c\",\n" +
                "    \"queryable\" : false\n" +
                "  } ],\n" +
                "  \"name\" : \"y\",\n" +
                "  \"connectionName\" : \"x\",\n" +
                "  \"type\" : \"z\",\n" +
                "  \"queryable\" : false\n" +
                "}";
        assertEquals(expected, value);

        RestSchemaNode other = KomodoJsonMarshaller.unmarshall(value, RestSchemaNode.class);

        assertEquals(expected, KomodoJsonMarshaller.marshall(other));

        value = KomodoJsonMarshaller.marshall(new Object[] {new RestSchemaNode(), new RestSchemaNode()}, true);

        assertEquals("[ {\n" +
                "  \"children\" : [ ],\n" +
                "  \"queryable\" : false\n" +
                "}, {\n" +
                "  \"children\" : [ ],\n" +
                "  \"queryable\" : false\n" +
                "} ]", value);
    }

}
