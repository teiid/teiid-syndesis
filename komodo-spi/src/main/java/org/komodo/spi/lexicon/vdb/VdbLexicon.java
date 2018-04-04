/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.spi.lexicon.vdb;

/**
 * Constants associated with the VDB namespace used in reading VDB manifests and writing JCR nodes.
 */
public interface VdbLexicon {

    /**
     * The URI and prefix constants of the VDB namespace.
     */
    public interface Namespace {
        String PREFIX = "vdb";
        String URI = "http://www.metamatrix.com/metamodels/VirtualDatabase";
    }

    /**
     * JCR identifiers relating to VDB manifest data roles.
     */
    public interface DataRole {
        String ALLOW_CREATE_TEMP_TABLES = Namespace.PREFIX + ":allowCreateTemporaryTables";
        String ANY_AUTHENTICATED = Namespace.PREFIX + ":anyAuthenticated";
        String DATA_ROLE = Namespace.PREFIX + ":dataRole";
        String DESCRIPTION = Namespace.PREFIX + ":description";
        String MAPPED_ROLE_NAMES = Namespace.PREFIX + ":mappedRoleNames";
        String PERMISSIONS = Namespace.PREFIX + ":permissions";
        String GRANT_ALL = Namespace.PREFIX + ":grantAll";

        /**
         * JCR identifiers relating to VDB manifest data role permissions.
         */
        public interface Permission {
            String ALLOW_ALTER = Namespace.PREFIX + ":allowAlter";
            String ALLOW_CREATE = Namespace.PREFIX + ":allowCreate";
            String ALLOW_DELETE = Namespace.PREFIX + ":allowDelete";
            String ALLOW_EXECUTE = Namespace.PREFIX + ":allowExecute";
            String ALLOW_LANGUAGE = Namespace.PREFIX + ":allowLanguage";
            String ALLOW_READ = Namespace.PREFIX + ":allowRead";
            String ALLOW_UPDATE = Namespace.PREFIX + ":allowUpdate";
            String CONDITIONS = Namespace.PREFIX + ":conditions";
            String MASKS = Namespace.PREFIX + ":masks";
            String PERMISSION = Namespace.PREFIX + ":permission";

            /**
             * JCR identifiers relating to VDB manifest data role permission conditions.
             */
            public interface Condition {
                String CONDITION = Namespace.PREFIX + ":condition";
                String CONSTRAINT = Namespace.PREFIX + ":constraint";
            }

            /**
             * JCR identifiers relating to VDB manifest data role permission masks.
             */
            public interface Mask {
                String MASK = Namespace.PREFIX + ":mask";
                String ORDER = Namespace.PREFIX + ":order";
            }
        }
    }

    /**
     * JCR identifiers relating to VDB manifest sources.
     */
    public interface Source {
        String NAME = Namespace.PREFIX + ":sourceName";
        String JNDI_NAME = Namespace.PREFIX + ":sourceJndiName";
        String SOURCE = Namespace.PREFIX + ":source";
        String TRANSLATOR = Namespace.PREFIX + ":sourceTranslator";

        /**
         * The name of the origin connection property. Value is {@value} .
         */
        String ORIGIN_CONNECTION = Namespace.PREFIX + ":originConnection";
    }

    /**
     * JCR identifiers relating to VDB manifest entries.
     */
    public interface Entry {
        String DESCRIPTION = Namespace.PREFIX + ":description";
        String ENTRY = Namespace.PREFIX + ":entry";
        String PATH = Namespace.PREFIX + ":path";
    }

    /**
     * JCR identifiers relating to VDB manifest imported VDBs.
     */
    public interface ImportVdb {
        String IMPORT_DATA_POLICIES = Namespace.PREFIX + ":importDataPolicies";
        String IMPORT_VDB = Namespace.PREFIX + ":importVdb";
        String VERSION = Namespace.PREFIX + ":version";
    }

    /**
     * Constants associated with the VDB namespace that identify VDB manifest identifiers.
     */
    public interface ManifestIds {
        String ALLOW_ALTER = "allow-alter";
        String ALLOW_CREATE = "allow-create";
        String ALLOW_CREATE_TEMP_TABLES = "allow-create-temporary-tables";
        String ALLOW_DELETE = "allow-delete";
        String ALLOW_EXECUTE = "allow-execute";
        String ALLOW_LANGUAGE = "allow-language";
        String ALLOW_READ = "allow-read";
        String ALLOW_UPDATE = "allow-update";
        String ANY_AUTHENTICATED = "any-authenticated";
        String BUILT_IN = "builtIn";
        String CHECKSUM = "checksum";
        String CONDITION = "condition";
        String CONNECTION_TYPE = "connection-type";
        String CONSTRAINT = "constraint";
        String DATA_ROLE = "data-role";
        String DESCRIPTION = "description";
        String ENTRY = "entry";
        String GRANT_ALL = "grant-all";
        String IMPORTS = "imports";
        String IMPORT_DATA_POLICIES = "import-data-policies";
        String IMPORT_VDB = "import-vdb";
        String INDEX_NAME = "indexName";
        String JNDI_NAME = "connection-jndi-name";
        String MAPPED_ROLE_NAME = "mapped-role-name";
        String MASK = "mask";
        String METADATA = "metadata";
        String MODEL = "model";
        String NAME = "name";
        String ORDER = "order";
        String ORIGIN_SRC_CONNECTION = "origin-conn-src";
        String PATH = "path";
        String PERMISSION = "permission";
        String PREVIEW = "preview";
        String PROPERTY = "property";
        String RESOURCE_NAME = "resource-name";
        String SEVERITY = "severity";
        String SOURCE = "source";
        String TRANSLATOR = "translator";
        String TRANSLATOR_NAME = "translator-name";
        String TYPE = "type";
        String VALIDATION_ERROR = "validation-error";
        String VALUE = "value";
        String VDB = "vdb";
        String VERSION = "version";
        String VISIBLE = "visible";
    }

    /**
     * JCR identifiers relating to VDB manifest models.
     */
    public interface Model {
        String BUILT_IN = Namespace.PREFIX + ":builtIn";
        String CHECKSUM = Namespace.PREFIX + ":checksum";
        String DDL_FILE_ENTRY_PATH = Namespace.PREFIX + ":ddlFileEntryPath";
        String DESCRIPTION = Namespace.PREFIX + ":description";
        String INDEX_NAME = Namespace.PREFIX + ":indexName";
        String MARKERS = Namespace.PREFIX + ":markers";
        String METADATA_TYPE = Namespace.PREFIX + ":metadataType";
        String MODEL = Namespace.PREFIX + ":model";
        String MODEL_DEFINITION = Namespace.PREFIX + ":modelDefinition";
        String PATH_IN_VDB = Namespace.PREFIX + ":pathInVdb";
        String SOURCE_JNDI_NAME = Namespace.PREFIX + ":sourceJndiName";
        String SOURCE_NAME = Namespace.PREFIX + ":sourceName";
        String SOURCE_TRANSLATOR = Namespace.PREFIX + ":sourceTranslator";
        String VISIBLE = Namespace.PREFIX + ":visible";

        /**
         * JCR identifiers relating to VDB manifest model validation error markers.
         */
        public interface Marker {
            String MARKER = Namespace.PREFIX + ":marker";
            String MESSAGE = Namespace.PREFIX + ":message";
            String PATH = Namespace.PREFIX + ":path";
            String SEVERITY = Namespace.PREFIX + ":severity";
        }
    }

    /**
     * JCR identifiers relating to VDB manifest translators.
     */
    public interface Translator {
        String DESCRIPTION = Namespace.PREFIX + ":description";
        String TRANSLATOR = Namespace.PREFIX + ":translator";
        String TYPE = Namespace.PREFIX + ":type";
    }

    /**
     * JCR identifiers relating to the VDB manifest.
     */
    public interface Vdb {
        String CONNECTION_TYPE = Namespace.PREFIX + ":connectionType";
        String DATA_ROLES = Namespace.PREFIX + ":dataRoles";
        String DECLARATIVE_MODEL = Namespace.PREFIX + ":declarativeModel";
        String DESCRIPTION = Namespace.PREFIX + ":description";
        String ENTRIES = Namespace.PREFIX + ":entries";
        String IMPORT_VDBS = Namespace.PREFIX + ":importVdbs";
        String MODEL = Namespace.PREFIX + ":model";
        String NAME = Namespace.PREFIX + ":name";
        String ORIGINAL_FILE = Namespace.PREFIX + ":originalFile";
        String PREVIEW = Namespace.PREFIX + ":preview";
        String RESOURCES = Namespace.PREFIX + ":resources";
        String SOURCES = Namespace.PREFIX + ":sources";
        String TRANSLATORS = Namespace.PREFIX + ":translators";
        String VERSION = Namespace.PREFIX + ":version";
        String VIRTUAL_DATABASE = Namespace.PREFIX + ":virtualDatabase";
    }
}
