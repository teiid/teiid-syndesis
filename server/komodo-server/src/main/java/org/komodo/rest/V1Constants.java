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

import org.komodo.StringConstants;

/**
 * Constants associated with version 1 of the Komodo REST application.
 */
public interface V1Constants extends StringConstants {

    class App {

        private static final Properties properties = new Properties();

        private static void init() {
            InputStream fileStream = V1Constants.class.getClassLoader().getResourceAsStream("app.properties");

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
     * The about segment
     */
    String ABOUT = "about"; //$NON-NLS-1$

    /**
     * The name of the URI path segment for the collection of VDBs in the Komodo workspace.
     */
    String VDBS_SEGMENT = "vdbs"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific vdb id
     */
    String VDB_PLACEHOLDER = "{vdbName}"; //$NON-NLS-1$

    /**
     * The name of the URI path segment for refresh of the preview vdb.
     */
    String REFRESH_PREVIEW_VDB_SEGMENT = "refreshPreviewVdb"; //$NON-NLS-1$

    /**
     * The name of the URI path segment for the collection of DataServices in the Komodo workspace.
     */
    String DATA_SERVICES_SEGMENT = "dataservices"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific data service id
     */
    String DATA_SERVICE_PLACEHOLDER = "{dataserviceName}"; //$NON-NLS-1$

    /**
     * The name of the URI path segment for validating a data service or connection name.
     */
    String NAME_VALIDATION_SEGMENT = "nameValidation"; //$NON-NLS-1$

    /**
     * The name of the URI path segment for finding service view info for a DataService
     */
    String SERVICE_VIEW_INFO = "serviceViewInfo"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific connection id
     */
    String CONNECTION_PLACEHOLDER = "{connectionName}"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific komodo source name
     */
    String KOMODO_SOURCE_PLACEHOLDER = "{komodoSourceName}"; //$NON-NLS-1$

    /**
     * The name of the URI path segment for schema refresh
     */
    String REFRESH_SCHEMA_SEGMENT = "refresh-schema"; //$NON-NLS-1$

    /**
     * The name of the URI path segment for the collection of models of a vdb
     */
    String MODELS_SEGMENT = "Models"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific model id
     */
    String MODEL_PLACEHOLDER = "{modelName}"; //$NON-NLS-1$

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
     * Placeholder added to an URI to allow a specific table id
     */
    String TABLE_PLACEHOLDER = "{tableName}"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific data role id
     */
    String DATA_ROLE_PLACEHOLDER = "{dataRoleId}"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific permission id
     */
    String PERMISSION_PLACEHOLDER = "{permissionId}"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific condition id
     */
    String CONDITION_PLACEHOLDER = "{conditionId}"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI to allow a specific mask id
     */
    String MASK_PLACEHOLDER = "{maskId}"; //$NON-NLS-1$

    /**
     * Placeholder added to an URI for validation of the value
     */
    String VALIDATE_PLACEHOLDER = "{validateValue}"; //$NON-NLS-1$

    /**
     * The name of the URI vdb name parameter
     */
    String VDB_NAME_PARAMETER = "name"; //$NON-NLS-1$

    /**
     * The teiid segment for running a query against the teiid server
     */
    String QUERY_SEGMENT = "query"; //$NON-NLS-1$

    /**
     * syndesis source summaries segment
     */
    String SYNDESIS_SOURCE_STATUSES = "syndesisSourceStatuses"; //$NON-NLS-1$

    /**
     * User profile resource method constant
     */
    String USER_PROFILE = "userProfile"; //$NON-NLS-1$

    /**
     * The view editor state of the user profile
     */
    String VIEW_EDITOR_STATE = "viewEditorState"; //$NON-NLS-1$

    /**
     * The view editor state of the user profile
     */
    String VIEW_LISTINGS = "viewListings"; //$NON-NLS-1$

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

    String IMPORT = "import";

    /**
     * Validate a ViewDefintion
     */
    String VALIDATE_VIEW_DEFINITION = "validateViewDefinition";

    /**
     * Get source schema, table, column information for view definition
     */
    String RUNTIME_METADATA = "runtimeMetadata";
}