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

package org.komodo.rest.service;

import static org.junit.Assert.assertFalse;

import java.util.Map;

import org.junit.Test;
import org.komodo.rest.relational.dataservice.RestDataservice;

import com.fasterxml.jackson.databind.type.SimpleType;

import io.swagger.converter.ModelConverters;
import io.swagger.models.Model;

public class SwaggerTest {
	
	@Test public void shouldHaveProperties() {
		//TODO could scan for all marked with json serialize
		
		//if we use the JsonSerialize annotation, we're required to have the as set
		//otherwise the introspection won't find any properties
		Map<String, Model> models = ModelConverters.getInstance().read(SimpleType.constructUnsafe(RestDataservice.class));
		assertFalse(models.isEmpty());
		assertFalse(models.values().iterator().next().getProperties().isEmpty());
	}

}
