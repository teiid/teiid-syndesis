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

import org.komodo.rest.KRestEntity;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestProperty;
import org.komodo.rest.json.LinkSerializer;
import org.komodo.rest.json.RestPropertySerializer;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.ViewEditorStateCommandSerializer.ViewEditorStateCmdUnitSerializer;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestQueryColumn;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlProjectedColumn;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate.RestStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;
import org.komodo.rest.relational.response.virtualization.RestRouteStatus;
import org.komodo.rest.relational.response.virtualization.RestVirtualizationStatus;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

/**
 * A JSON serializer and deserializer for {@link RestBasicEntity Komodo REST objects}.
 */
public final class KomodoJsonMarshaller {

    private static final KLog LOGGER = KLog.getLogger();

    /**
     * The shared JSON serialier/deserializer for {@link RestBasicEntity} objects.
     */
    public static final Gson BUILDER;

    public static final Gson PRETTY_BUILDER;

    static {
        final GsonBuilder temp = new GsonBuilder().registerTypeAdapter( RestLink.class, new LinkSerializer())
                                                  .registerTypeAdapter(KomodoStatusObject.class, new StatusObjectSerializer())
                                                  .registerTypeAdapter(RestProperty.class, new RestPropertySerializer())
                                                  .registerTypeAdapter(RestDataservice.class, new DataserviceSerializer())
                                                  .registerTypeAdapter(RestSyndesisSourceStatus.class, new SyndesisSourceStatusSerializer())
                                                  .registerTypeAdapter(RestBasicEntity.class, new BasicEntitySerializer<RestBasicEntity>())
                                                  .registerTypeAdapter(KomodoQueryAttribute.class, new QueryAttributeSerializer())
                                                  .registerTypeAdapter(RestQueryResult.class, new QueryResultSerializer())
                                                  .registerTypeAdapter(RestQueryColumn.class, new QueryColumnSerializer())
                                                  .registerTypeAdapter(RestQueryRow.class, new QueryRowSerializer())
                                                  .registerTypeAdapter(KomodoConnectionAttributes.class, new ConnectionAttributesSerializer())
                                                  .registerTypeAdapter(RestViewEditorState.class, new ViewEditorStateSerializer())
                                                  .registerTypeAdapter(RestViewDefinition.class, new ViewDefinitionSerializer())
                                                  .registerTypeAdapter(RestSqlComposition.class, new SqlCompositionSerializer())
                                                  .registerTypeAdapter(RestSqlProjectedColumn.class, new SqlProjectedColumnSerializer())
                                                  .registerTypeAdapter(RestStateCommandAggregate.class, new ViewEditorStateCommandSerializer())
                                                  .registerTypeAdapter(RestStateCommand.class, new ViewEditorStateCmdUnitSerializer())
                                                  .registerTypeAdapter(RestVirtualizationStatus.class, new VirtualizationStatusSerializer())
                                                  .registerTypeAdapter(RestRouteStatus.class, new RouteStatusSerializer());

        BUILDER = temp.create();
        PRETTY_BUILDER = temp.setPrettyPrinting().create();
    }

    /**
     * Outputs a non-pretty printed JSON representation.
     *
     * @param entity
     *        the entity whose JSON representation is being requested (cannot be <code>null</code>)
     * @return the JSON representation (never empty)
     */
    public static String marshall( final KRestEntity entity ) {
        return marshall( entity, true );
    }

    /**
     * @param entity
     *        the entity whose JSON representation is being requested (cannot be <code>null</code>)
     * @param prettyPrint
     *        <code>true</code> if JSON output should be pretty printed
     * @return the JSON representation (never empty)
     */
    public static String marshall( final KRestEntity entity,
                                   final boolean prettyPrint ) {
        ArgCheck.isNotNull( entity, "entity" ); //$NON-NLS-1$

        String json = null;

        if ( prettyPrint ) {
            json = PRETTY_BUILDER.toJson( entity );
        } else {
            json = BUILDER.toJson( entity );
        }
        return json;
    }

    /**
     * @param entities
     *        the entities whose JSON representation is being requested (cannot be <code>null</code>)
     * @param prettyPrint
     *        <code>true</code> if JSON output should be pretty printed
     * @return the JSON representation (never empty)
     */
    public static String marshallArray( final KRestEntity[] entities,
                                   final boolean prettyPrint ) {
        ArgCheck.isNotNull( entities, "entities" ); //$NON-NLS-1$

        String json = null;

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
    public static < T extends KRestEntity > T unmarshall( final String json,
                                                               final Class< T > entityClass ) {
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
    public static < T extends KRestEntity > T[] unmarshallArray( final String json,
                                                               final Class< T[] > entityClass ) {
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
