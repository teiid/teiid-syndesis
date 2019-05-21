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

import java.io.InputStream;
import java.util.Properties;

import javax.ws.rs.core.Application;

import org.komodo.core.KEngine;
import org.komodo.rest.json.JsonConstants;
import org.komodo.spi.KException;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.Repository;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * The JAX-RS {@link Application} that provides the Komodo REST API.
 */
public class KomodoRestV1Application implements SystemConstants {

    @Autowired
    KEngine kengine;

    /**
     * Constants associated with version 1 of the Komodo REST application.
     */
    public static interface V1Constants extends JsonConstants {

        class App {

            private static final Properties properties = new Properties();

            private static void init() {
                InputStream fileStream = KomodoRestV1Application.class.getClassLoader().getResourceAsStream("app.properties");

                try {
                    properties.load(fileStream);
                } catch (Exception ex) {
                    throw new RuntimeException(ex);
                }
            }

            /**
             * Application name and context
             */
            public static String name() {
                init();

                return properties.getProperty("app.name");
            }

            /**
             * Application display title
             */
            public static String title() {
                init();

                return properties.getProperty("app.title");
            }

            /**
             * Application description
             */
            public static String description() {
                init();

                return properties.getProperty("app.description");
            }

            /**
             * Version of the application
             */
            public static String version() {
                init();

                return properties.getProperty("app.version");
            }
        }

        /**
         * The URI path segment for the Komodo REST application. It is included in the base URI. <strong>DO NOT INCLUDE THIS IN
         * OTHER URI SEGMENTS</strong>
         */
        String APP_PATH = FORWARD_SLASH + "v1"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the Komodo workspace.
         */
        String WORKSPACE_SEGMENT = "workspace"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the utility service.
         */
        String SERVICE_SEGMENT = "service"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the metadata service.
         */
        String METADATA_SEGMENT = "metadata"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the Komodo schema.
         */
        String SCHEMA_SEGMENT = "schema"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the VDB manifest XML resource.
         */
        String VDB_MANIFEST_SEGMENT = "manifest"; //$NON-NLS-1$

        /**
         * The about segment
         */
        String ABOUT = "about"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for a Vdb in the Komodo workspace.
         */
        String VDB_SEGMENT = "vdb"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of VDBs in the Komodo workspace.
         */
        String VDBS_SEGMENT = "vdbs"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific vdb id
         */
        String VDB_PLACEHOLDER = "{vdbName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for clone.
         */
        String CLONE_SEGMENT = "clone"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for refresh of the preview vdb.
         */
        String REFRESH_PREVIEW_VDB_SEGMENT = "refreshPreviewVdb"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for creating workspace VDBs from teiid
         */
        String VDBS_FROM_TEIID = "VdbsFromTeiid"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for creating workspace connections from teiid
         */
        String CONNECTIONS_FROM_TEIID = "connectionsFromTeiid"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for undeploy.
         */
        String UNDEPLOY = "undeploy"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of DataServices in the Komodo workspace.
         */
        String DATA_SERVICES_SEGMENT = "dataservices"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for a DataService in the Komodo workspace.
         */
        String DATA_SERVICE_SEGMENT = "dataservice"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific data service id
         */
        String DATA_SERVICE_PLACEHOLDER = "{dataserviceName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for DataService deployable status
         */
        String DEPLOYABLE_STATUS_SEGMENT = "deployableStatus"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for validating a data service or connection name.
         */
        String NAME_VALIDATION_SEGMENT = "nameValidation"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for finding source vdb matches for a DataService
         */
        String SOURCE_VDB_MATCHES = "sourceVdbMatches"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for finding service view info for a DataService
         */
        String SERVICE_VIEW_INFO = "serviceViewInfo"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for connections.
         */
        String CONNECTIONS_SEGMENT = "connections"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for a connection.
         */
        String CONNECTION_SEGMENT = "connection"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific connection id
         */
        String CONNECTION_PLACEHOLDER = "{connectionName}"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific syndesis source name
         */
        String SYNDESIS_SOURCE_PLACEHOLDER = "{syndesisSourceName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for schema refresh
         */
        String REFRESH_SCHEMA_SEGMENT = "refresh-schema"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for service catalog.
         */
        String SERVICE_CATALOG_SEGMENT = "serviceCatalog"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for templates.
         */
        String TEMPLATES_SEGMENT = "templates"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for a template.
         */
        String TEMPLATE_SEGMENT = "template"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific template id
         */
        String TEMPLATE_PLACEHOLDER = "{templateName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for template entries.
         */
        String TEMPLATE_ENTRIES_SEGMENT = "entries"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific template entry id
         */
        String TEMPLATE_ENTRY_PLACEHOLDER = "{templateEntryName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for a setting a dataservice's service vdb for single source table views
         */
        String SERVICE_VDB_FOR_SINGLE_SOURCE_TABLES = "ServiceVdbForSingleSourceTables"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for a setting a dataservice's service vdb for join view
         */
        String SERVICE_VDB_FOR_JOIN_TABLES = "ServiceVdbForJoinTables"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for getting the DDL for single table view
         */
        String SERVICE_VIEW_DDL_FOR_SINGLE_TABLE = "ServiceViewDdlForSingleTable"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for getting the DDL for join view
         */
        String SERVICE_VIEW_DDL_FOR_JOIN_TABLES = "ServiceViewDdlForJoinTables"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for getting the join criteria given two tables
         */
        String CRITERIA_FOR_JOIN_TABLES = "CriteriaForJoinTables"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of Drivers in the Komodo workspace.
         */
        String DRIVERS_SEGMENT = "drivers"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of models of a vdb
         */
        String MODELS_SEGMENT = "Models"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific model id
         */
        String MODEL_PLACEHOLDER = "{modelName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of sources of a model
         */
        String SOURCES_SEGMENT = "VdbModelSources"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific source id
         */
        String SOURCE_PLACEHOLDER = "{sourceName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of views of a vdb model
         */
        String VIEWS_SEGMENT = "Views"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific view id
         */
        String VIEW_PLACEHOLDER = "{viewName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of catalogs
         */
        String JDBC_CATALOG_SCHEMA_SEGMENT = "JdbcCatalogSchema"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the jdbc info
         */
        String JDBC_INFO_SEGMENT = "JdbcInfo"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of tables of a model
         */
        String TABLES_SEGMENT = "Tables"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific table id
         */
        String TABLE_PLACEHOLDER = "{tableName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of columns of a table
         */
        String COLUMNS_SEGMENT = "Columns"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for default translator
         */
        String TRANSLATOR_DEFAULT_SEGMENT = "TranslatorDefault"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of translators of a vdb
         */
        String TRANSLATORS_SEGMENT = "VdbTranslators"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific translator id
         */
        String TRANSLATOR_PLACEHOLDER = "{translatorName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of imports of a vdb
         */
        String IMPORTS_SEGMENT = "VdbImports"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific import id
         */
        String IMPORT_PLACEHOLDER = "{importName}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of data roles of a vdb
         */
        String DATA_ROLES_SEGMENT = "VdbDataRoles"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific data role id
         */
        String DATA_ROLE_PLACEHOLDER = "{dataRoleId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of data role permissions
         */
        String PERMISSIONS_SEGMENT = "VdbPermissions"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific permission id
         */
        String PERMISSION_PLACEHOLDER = "{permissionId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of permission's conditions
         */
        String CONDITIONS_SEGMENT = "VdbConditions"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific condition id
         */
        String CONDITION_PLACEHOLDER = "{conditionId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for the collection of permission's masks
         */
        String MASKS_SEGMENT = "VdbMasks"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific mask id
         */
        String MASK_PLACEHOLDER = "{maskId}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for loading of the sample vdb data
         */
        String SAMPLE_DATA = "samples"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for validating a value
         */
        String VALIDATE_SEGMENT = "validate"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI for validation of the value
         */
        String VALIDATE_PLACEHOLDER = "{validateValue}"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for searching the workspace
         */
        String SEARCH_SEGMENT = "search"; //$NON-NLS-1$

        /**
         * The name of the URI search saved search parameter
         */
        String SEARCH_SAVED_NAME_PARAMETER = "searchName"; //$NON-NLS-1$

        /**
         * The name of the URI search contains parameter
         */
        String SEARCH_CONTAINS_PARAMETER = "contains"; //$NON-NLS-1$

        /**
         * The name of the URI search name parameter
         */
        String SEARCH_OBJECT_NAME_PARAMETER = "objectName"; //$NON-NLS-1$

        /**
         * The name of the URI search path parameter
         */
        String SEARCH_PATH_PARAMETER = "path"; //$NON-NLS-1$

        /**
         * The name of the URI search path parameter
         */
        String SEARCH_TYPE_PARAMETER = "type"; //$NON-NLS-1$

        /**
         * The name of the URI search parent parameter
         */
        String SEARCH_PARENT_PARAMETER = "parent"; //$NON-NLS-1$

        /**
         * The name of the URI search ancestor parameter
         */
        String SEARCH_ANCESTOR_PARAMETER = "ancestor"; //$NON-NLS-1$

        /**
         * The URI path for the collection of saved searches
         */
        String SAVED_SEARCHES_SEGMENT = "savedSearches"; //$NON-NLS-1$

        /**
         * The name of the URI vdb name parameter
         */
        String VDB_NAME_PARAMETER = "name"; //$NON-NLS-1$

        /**
         * The vdb export xml property
         */
        String VDB_EXPORT_XML_PROPERTY = "vdb-export-xml"; //$NON-NLS-1$

        /**
         * The name of the URI path segment for creating a workspace vdb model using teiid ddl
         */
        String MODEL_FROM_TEIID_DDL = "ModelFromTeiidDdl"; //$NON-NLS-1$

        /**
         * The teiid credentials property for modifying the usernames and passwords
         */
        String METADATA__CREDENTIALS = "credentials"; //$NON-NLS-1$

        /**
         * The driver property for adding a driver to the teiid server
         */
        String METADATA_DRIVER = "driver"; //$NON-NLS-1$

        /**
         * Placeholder added to an URI to allow a specific teiid driver id
         */
        String METADATA_DRIVER_PLACEHOLDER = "{driverName}"; //$NON-NLS-1$

        /**
         * The teiid status path segment
         */
        String STATUS_SEGMENT = "status"; //$NON-NLS-1$

        /**
         * The name of the resource used for importing and exporting artifacts
         */
        String IMPORT_EXPORT_SEGMENT = "importexport"; //$NON-NLS-1$

        /**
         * The export operation of the import export service
         */
        String EXPORT = "export"; //$NON-NLS-1$

        /**
         * The import operation of the import export service
         */
        String IMPORT = "import"; //$NON-NLS-1$

        /**
         * The export-to-git operation of the import export service
         */
        String EXPORT_TO_GIT = "exportToGit"; //$NON-NLS-1$

        /**
         * The available storage types of the import export service
         */
        String STORAGE_TYPES = "availableStorageTypes"; //$NON-NLS-1$

        /**
         * The teiid segment for running a query against the teiid server
         */
        String QUERY_SEGMENT = "query"; //$NON-NLS-1$

        /**
         * The teiid segment for running a ping against the teiid server
         */
        String PING_SEGMENT = "ping"; //$NON-NLS-1$

        /**
         * The name of the URI ping type parameter
         */
        String PING_TYPE_PARAMETER = "pingType"; //$NON-NLS-1$

        /**
         * syndesis source segment
         */
        String SYNDESIS_SOURCE = "syndesisSource"; //$NON-NLS-1$

        /**
         * syndesis sources segment
         */
        String SYNDESIS_SOURCES = "syndesisSources"; //$NON-NLS-1$

        /**
         * syndesis source summaries segment
         */
        String SYNDESIS_SOURCE_STATUSES = "syndesisSourceStatuses"; //$NON-NLS-1$
        
        /**
         * Bind to available source in OpenShift Service catalog
         */
        String BIND_TO_SERVICE_CATALOG_SOURCE = "bindToServiceCatalogSource"; //$NON-NLS-1$

        /**
         * User profile resource method constant
         */
        String USER_PROFILE = "userProfile"; //$NON-NLS-1$

        /**
         * The git repository configuration to the user profile
         */
        String GIT_REPOSITORY = "gitRepository"; //$NON-NLS-1$

        /**
         * Git repository configuration placeholder
         */
        String GIT_REPO_PLACEHOLDER = "{gitRepositoryName}"; //$NON-NLS-1$

        /**
         * The view editor state of the user profile
         */
        String VIEW_EDITOR_STATE = "viewEditorState"; //$NON-NLS-1$

        /**
         * The view editor state of the user profile
         */
        String VIEW_EDITOR_STATES = "viewEditorStates"; //$NON-NLS-1$

        /**
         * View editor state placeholder
         */
        String VIEW_EDITOR_STATE_PLACEHOLDER = "{viewEditorStateId}"; //$NON-NLS-1$

        /**
         * Publish VDB
         */
        String PUBLISH = "publish"; //$NON-NLS-1$

        /**
         * Publish VDB Logs
         */
        String PUBLISH_LOGS = "publishLogs"; //$NON-NLS-1$

        /**
         * Refresh views referenced in a view editor state object
         */
        String REFRESH_DATASERVICE_VIEWS = "refreshViews";
        
        /**
         * Validate a ViewDefintion
         */
        String VALIDATE_VIEW_DEFINITION = "validateViewDefinition";
    }


    public KEngine getEngine() throws KException {
        return kengine;
    }


    public Repository getDefaultRepository() throws KException {
        return kengine.getDefaultRepository();
    }
}
