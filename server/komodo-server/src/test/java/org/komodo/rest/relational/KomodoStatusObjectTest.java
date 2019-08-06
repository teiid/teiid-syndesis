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

package org.komodo.rest.relational;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.komodo.rest.KomodoJsonMarshaller;
import org.komodo.rest.relational.KomodoStatusObject;

public class KomodoStatusObjectTest {
	
	@Test public void testJsonRoundtrip() {
		KomodoStatusObject kso = new KomodoStatusObject("x");
		kso.addAttribute("attribute", "message");
		kso.addAttribute("attribute1", "message1");
		
		String value = KomodoJsonMarshaller.marshall(kso);
		assertEquals("{\n" + 
				"  \"Title\" : \"x\",\n" + 
				"  \"Information\" : {\n" + 
				"    \"attribute\" : \"message\",\n" + 
				"    \"attribute1\" : \"message1\"\n" + 
				"  }\n" + 
				"}", value);
		
		KomodoStatusObject other = KomodoJsonMarshaller.unmarshall(value, KomodoStatusObject.class);
		
		assertEquals(kso, other);
	}
	
}
