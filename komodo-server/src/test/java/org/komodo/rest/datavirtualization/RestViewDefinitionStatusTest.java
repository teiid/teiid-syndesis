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

import org.junit.Test;
import org.komodo.datavirtualization.ViewDefinition;
import org.komodo.rest.KomodoJsonMarshaller;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestViewDefinitionStatusTest {

    @Test public void shouldSerialize() {
        RestViewDefinitionStatus status = new RestViewDefinitionStatus();
        status.setStatus("FINE");
        status.setMessage("Maybe a warning?");
        assertEquals("{\n" +
                "  \"status\" : \"FINE\",\n" +
                "  \"message\" : \"Maybe a warning?\"\n" +
                "}", KomodoJsonMarshaller.marshall(status));

        ViewDefinition vd = new ViewDefinition("x", "y");
        status.setViewDefinition(vd);
        assertEquals("{\n" +
                "  \"status\" : \"FINE\",\n" +
                "  \"message\" : \"Maybe a warning?\",\n" +
                "  \"dataVirtualizationName\" : \"x\",\n" +
                "  \"isComplete\" : false,\n" +
                "  \"isUserDefined\" : false,\n" +
                "  \"name\" : \"y\",\n" +
                "  \"sourcePaths\" : [ ]\n" +
                "}", KomodoJsonMarshaller.marshall(status));
    }

}
