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

import java.io.IOException;

import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.response.RestQueryColumn;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * A JSON serializer and deserializer for {@link RestBasicEntity Komodo REST objects}.
 */
public final class KomodoJsonMarshaller {
	
	private final static ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private static final KLog LOGGER = KLog.getLogger();

    /**
     * The shared JSON serialier/deserializer for {@link RestBasicEntity} objects.
     */
    public static final Gson BUILDER;

    public static final Gson PRETTY_BUILDER;

    static {
        final GsonBuilder temp = new GsonBuilder().registerTypeAdapter(RestSyndesisSourceStatus.class, new SyndesisSourceStatusSerializer())
                                                  .registerTypeAdapter(RestBasicEntity.class, new BasicEntitySerializer<RestBasicEntity>())
                                                  .registerTypeAdapter(RestQueryResult.class, new QueryResultSerializer())
                                                  .registerTypeAdapter(RestQueryColumn.class, new QueryColumnSerializer())
                                                  .registerTypeAdapter(RestQueryRow.class, new QueryRowSerializer());

        BUILDER = temp.create();
        PRETTY_BUILDER = temp.setPrettyPrinting().create();
    }

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

        String json = null;
        
        if (useJackson(entity.getClass())) {
        	try {
        		if (prettyPrint) {
    				return OBJECT_MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(entity);
        		}
        		return OBJECT_MAPPER.writeValueAsString(entity);
			} catch (JsonProcessingException e) {
				throw new RuntimeException(e);
			}
        }

        if ( prettyPrint ) {
            json = PRETTY_BUILDER.toJson( entity );
        } else {
            json = BUILDER.toJson( entity );
        }
        return json;
    }

	private static boolean useJackson(final Class<?> entityClass) {
		if (entityClass.getAnnotation(JsonSerialize.class) != null) {
			return true;
		}
		boolean result = !BUILDER.getAdapter(entityClass).getClass().getName().contains("komodo");
		//if there's no registered serializer, use jackson as well
		if (result) {
			LOGGER.info("should mark {0} with JsonSerialize", entityClass);
		}
		return result;
	}

    /**
     * @param entities
     *        the entities whose JSON representation is being requested (cannot be <code>null</code>)
     * @param prettyPrint
     *        <code>true</code> if JSON output should be pretty printed
     * @return the JSON representation (never empty)
     */
    public static String marshallArray( final Object[] entities,
                                   final boolean prettyPrint ) {
        ArgCheck.isNotNull( entities, "entities" ); //$NON-NLS-1$

        String json = null;
        
        if (entities.length > 0 && useJackson(entities[0].getClass())) {
        	try {
				return OBJECT_MAPPER.writeValueAsString(entities);
			} catch (JsonProcessingException e) {
				throw new RuntimeException(e);
			}
        }

        if ( prettyPrint ) {
            json = PRETTY_BUILDER.toJson( entities );
        } else {
            json = BUILDER.toJson( entities );
        }

        LOGGER.debug( "marshall: {0}", json ); //$NON-NLS-1$
        return json;
    }

    /**
     * @param <T>
     *        the {@link RestBasicEntity} type of the output
     * @param json
     *        the JSON representation being converted to a {@link RestBasicEntity} (cannot be empty)
     * @param entityClass
     *        the type of {@link RestBasicEntity} the JSON will be converted to (cannot be <code>null</code>)
     * @return the {@link RestBasicEntity} (never <code>null</code>)
     */
    public static < T extends Object > T unmarshall( final String json,
                                                               final Class< T > entityClass ) {
    	
    	if (useJackson(entityClass)) {
        	try {
				return OBJECT_MAPPER.readValue(json, entityClass);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
        }
    	
        final T entity = BUILDER.fromJson( json, entityClass );
        return entity;
    }

    /**
     * @param <T>
     *        the {@link RestBasicEntity} type of the output
     * @param json
     *        the JSON representation being converted to a {@link RestBasicEntity} (cannot be empty)
     * @param entityClass
     *        the type of {@link RestBasicEntity} the JSON will be converted to (cannot be <code>null</code>)
     * @return the {@link RestBasicEntity} (never <code>null</code>)
     */
    public static < T extends Object > T[] unmarshallArray( final String json,
                                                               final Class< T[] > entityClass ) {
    	if (useJackson(entityClass.getComponentType())) {
        	try {
				return OBJECT_MAPPER.readValue(json, entityClass);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
        }
    	
    	final T[] entity = BUILDER.fromJson( json, entityClass );
        return entity;
    }

    /**
     * Don't allow construction outside of this class.
     */
    private KomodoJsonMarshaller() {
        // nothing to do
    }

}
