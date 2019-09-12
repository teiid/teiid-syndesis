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

package org.komodo.rest.datavirtualization;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;
import org.komodo.rest.KomodoJsonMarshaller;
import org.komodo.rest.datavirtualization.RestSyndesisSourceStatus.EntityState;

@SuppressWarnings("nls")
public class RestSyndesisSourceStatusTest {

    @Test public void testSerialization() {
        RestSyndesisSourceStatus rsss = new RestSyndesisSourceStatus("x");
        rsss.setHasTeiidSource(true);
        rsss.setId("id");
        rsss.setErrors(Arrays.asList("some error"));
        rsss.setSchemaState(EntityState.ACTIVE);
        rsss.setLoading(true);

        String value = KomodoJsonMarshaller.marshall(rsss);
        assertEquals("{\n" +
                "  \"sourceName\" : \"x\",\n" +
                "  \"hasTeiidSource\" : true,\n" +
                "  \"errors\" : [ \"some error\" ],\n" +
                "  \"schemaState\" : \"ACTIVE\",\n" +
                "  \"id\" : \"id\",\n" +
                "  \"loading\" : true\n" +
                "}", value);
    }

}
