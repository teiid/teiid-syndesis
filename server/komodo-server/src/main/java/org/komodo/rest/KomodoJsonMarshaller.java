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
package org.komodo.rest;

import java.io.IOException;

import org.komodo.utils.ArgCheck;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * A JSON serializer and deserializer.
 */
public final class KomodoJsonMarshaller {
	
	private final static ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    /**
     * Outputs a pretty printed JSON representation.
     *
     * @param entity
     *        the entity whose JSON representation is being requested (cannot be <code>null</code>)
     * @return the JSON representation (never empty)
     */
    public static String marshall( final Object entity ) {
        return marshall( entity, true );
    }

    /**
     * @param entity
     *        the entity whose JSON representation is being requested (cannot be <code>null</code>)
     * @param prettyPrint
     *        <code>true</code> if JSON output should be pretty printed
     * @return the JSON representation (never empty)
     */
    public static String marshall( final Object entity,
                                   final boolean prettyPrint ) {
        ArgCheck.isNotNull( entity, "entity" ); //$NON-NLS-1$

    	try {
    		if (prettyPrint) {
				return OBJECT_MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(entity);
    		}
    		return OBJECT_MAPPER.writeValueAsString(entity);
		} catch (JsonProcessingException e) {
			throw new RuntimeException(e);
		}
    }

    public static < T extends Object > T unmarshall( final String json,
                                                               final Class< T > entityClass ) {
    	
    	try {
			return OBJECT_MAPPER.readValue(json, entityClass);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
    }

    /**
     * Don't allow construction outside of this class.
     */
    private KomodoJsonMarshaller() {
        // nothing to do
    }

}
