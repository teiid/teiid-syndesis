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

import java.util.HashMap;
import java.util.Map;

import org.komodo.rest.KRestEntity;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestProperty;
import org.komodo.rest.json.LinkSerializer;
import org.komodo.rest.json.RestPropertySerializer;
import org.komodo.rest.relational.connection.ConnectionSchema;
import org.komodo.rest.relational.connection.ConnectionSchemaPairProperty;
import org.komodo.rest.relational.connection.ConnectionSchemaProperty;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.dataservice.DataServiceSchema;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.ViewEditorStateCommandSerializer.ViewEditorStateCmdUnitSerializer;
import org.komodo.rest.relational.json.connection.ConnectionSchemaPropertyListSerializer;
import org.komodo.rest.relational.json.connection.ConnectionSchemaPropertyPairPropertySerializer;
import org.komodo.rest.relational.json.connection.ConnectionSchemaPropertySerializer;
import org.komodo.rest.relational.json.connection.ConnectionSchemaSerializer;
import org.komodo.rest.relational.json.connection.ConnectionSerializer;
import org.komodo.rest.relational.json.connection.MetadataConnectionSerializer;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;
import org.komodo.rest.relational.request.KomodoFileAttributes;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.request.KomodoSearcherAttributes;
import org.komodo.rest.relational.request.KomodoTeiidAttributes;
import org.komodo.rest.relational.request.KomodoViewsInfo;
import org.komodo.rest.relational.response.ImportExportStatus;
import org.komodo.rest.relational.response.KomodoSavedSearcher;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.rest.relational.response.RestConnectionSummary;
import org.komodo.rest.relational.response.RestQueryColumn;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.rest.relational.response.RestStorageType;
import org.komodo.rest.relational.response.RestStorageTypeDescriptor;
import org.komodo.rest.relational.response.RestSyndesisDataSource;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbCondition;
import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.rest.relational.response.RestVdbMask;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.relational.response.RestVdbModelSource;
import org.komodo.rest.relational.response.RestVdbModelTable;
import org.komodo.rest.relational.response.RestVdbModelTableColumn;
import org.komodo.rest.relational.response.RestVdbModelView;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.rest.relational.response.metadata.RestMetadataConnection;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplate;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplateEntry;
import org.komodo.rest.relational.response.metadata.RestMetadataVdb;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatusVdb;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbTranslator;
import org.komodo.rest.relational.response.metadata.RestSyndesisSourceStatus;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlComposition;
import org.komodo.rest.relational.response.vieweditorstate.RestSqlProjectedColumn;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate;
import org.komodo.rest.relational.response.vieweditorstate.RestStateCommandAggregate.RestStateCommand;
import org.komodo.rest.relational.response.vieweditorstate.RestViewDefinition;
import org.komodo.rest.relational.response.vieweditorstate.RestViewEditorState;
import org.komodo.rest.relational.response.virtualization.RestRouteStatus;
import org.komodo.rest.relational.response.virtualization.RestVirtualizationStatus;
import org.komodo.rest.schema.json.TeiidXsdReader;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

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
                                                  .registerTypeAdapter(KomodoSavedSearcher.class, new SavedSearcherSerializer())
                                                  .registerTypeAdapter(KomodoPathAttribute.class, new PathAttributeSerializer<KomodoPathAttribute>())
                                                  .registerTypeAdapter(KomodoSearcherAttributes.class, new SearcherAttributesSerializer())
                                                  .registerTypeAdapter(KomodoTeiidAttributes.class, new TeiidAttributesSerializer())
                                                  .registerTypeAdapter(KomodoViewsInfo.class, new ViewsInfoSerializer())
                                                  .registerTypeAdapter(RestProperty.class, new RestPropertySerializer())
                                                  .registerTypeAdapter(RestVdb.class, new VdbSerializer())
                                                  .registerTypeAdapter(RestVdbModel.class, new VdbModelSerializer())
                                                  .registerTypeAdapter(RestVdbModelSource.class, new VdbModelSourceSerializer())
                                                  .registerTypeAdapter(RestVdbModelTable.class, new VdbModelTableSerializer())
                                                  .registerTypeAdapter(RestVdbModelTableColumn.class, new VdbModelTableColumnSerializer())
                                                  .registerTypeAdapter(RestVdbModelView.class, new VdbModelViewSerializer())
                                                  .registerTypeAdapter(RestVdbDataRole.class, new VdbDataRoleSerializer())
                                                  .registerTypeAdapter(RestVdbImport.class, new VdbImportSerializer())
                                                  .registerTypeAdapter(RestVdbPermission.class, new VdbPermissionSerializer())
                                                  .registerTypeAdapter(RestVdbCondition.class, new VdbConditionSerializer())
                                                  .registerTypeAdapter(RestVdbMask.class, new VdbMaskSerializer())
                                                  .registerTypeAdapter(RestVdbTranslator.class, new VdbTranslatorSerializer())
                                                  .registerTypeAdapter(RestDataservice.class, new DataserviceSerializer())
                                                  .registerTypeAdapter(RestConnection.class, new ConnectionSerializer())
                                                  .registerTypeAdapter(RestConnectionSummary.class, new ConnectionSummarySerializer())
                                                  .registerTypeAdapter(RestSyndesisSourceStatus.class, new SyndesisSourceStatusSerializer())
                                                  .registerTypeAdapter(RestMetadataConnectionStatus.class, new MetadataConnectionStatusSerializer())
                                                  .registerTypeAdapter(RestMetadataTemplate.class, new MetadataTemplateSerializer())
                                                  .registerTypeAdapter(RestMetadataTemplateEntry.class, new MetadataTemplateEntrySerializer())
                                                  .registerTypeAdapter(RestBasicEntity.class, new BasicEntitySerializer<RestBasicEntity>())
                                                  .registerTypeAdapter(RestMetadataStatus.class, new MetadataStatusSerializer())
                                                  .registerTypeAdapter(RestMetadataVdb.class, new MetadataVdbSerializer())
                                                  .registerTypeAdapter(RestMetadataVdbStatus.class, new MetadataVdbStatusSerializer())
                                                  .registerTypeAdapter(RestMetadataVdbStatusVdb.class, new MetadataVdbStatusVdbSerializer())
                                                  .registerTypeAdapter(RestMetadataConnection.class, new MetadataConnectionSerializer())
                                                  .registerTypeAdapter(RestMetadataVdbTranslator.class, new MetadataVdbTranslatorSerializer())
                                                  .registerTypeAdapter(RestSyndesisDataSource.class, new ServiceCatalogDataSourceSerializer())
                                                  .registerTypeAdapter(ConnectionSchema.class, new ConnectionSchemaSerializer())
                                                  .registerTypeAdapter(ConnectionSchemaPropertyListSerializer.class, new ConnectionSchemaPropertyListSerializer())
                                                  .registerTypeAdapter(ConnectionSchemaPairProperty.class, new ConnectionSchemaPropertyPairPropertySerializer())
                                                  .registerTypeAdapter(ConnectionSchemaProperty.class, new ConnectionSchemaPropertySerializer())
                                                  .registerTypeAdapter(KomodoStorageAttributes.class, new StorageAttributesSerializer())
                                                  .registerTypeAdapter(KomodoFileAttributes.class, new FileAttributesSerializer())
                                                  .registerTypeAdapter(ImportExportStatus.class, new ImportExportStatusSerializer())
                                                  .registerTypeAdapter(RestStorageType.class, new StorageTypeSerializer())
                                                  .registerTypeAdapter(RestStorageTypeDescriptor.class, new StorageTypeDescriptorSerializer())
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
     * Cached teiid element schema. Unlikely to ever change
     * and this reduces the work required.
     */
    private static Map<Object, String> teiidElementSchemaCache = new HashMap<>();

    public static String teiidElementSchema(KomodoType kType) throws Exception {
        String schema;

        //
        // Check the cache first
        //
        Object key = kType;
        if (kType == null)
            key = Void.class;

        schema = teiidElementSchemaCache.get(key);
        if (schema != null)
            return schema;

        //
        // Not in cache so create
        //
        TeiidXsdReader reader = new TeiidXsdReader();

        if (kType == null) {
            schema = reader.read();

            JsonParser parser = new JsonParser();
            JsonElement shellElement = parser.parse(schema);
            JsonObject shell = shellElement.getAsJsonObject();
            JsonElement schema1Element = shell.get("schema-1");
            JsonObject schema1 = schema1Element.getAsJsonObject();

            ConnectionSchema dsSchema = new ConnectionSchema();
            schema1.add(ConnectionSchema.NAME_LABEL, BUILDER.toJsonTree(dsSchema));

            schema = PRETTY_BUILDER.toJson(shell);

        } else if (KomodoType.CONNECTION.equals(kType)) {
            // Data sources do not have a mandated schema set out in the vdb-deployer.xsd
            // so need to construct our own. At this point should be pretty simple

            ConnectionSchema dsSchema = new ConnectionSchema();
            schema = marshall(dsSchema);

        } else if (KomodoType.DATASERVICE.equals(kType)) {
            DataServiceSchema dataserviceSchema = new DataServiceSchema();
            schema = marshall(dataserviceSchema);

        } else {
            schema = reader.schemaByKType(kType);
        }

        teiidElementSchemaCache.put(key, schema);
        return schema;
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
