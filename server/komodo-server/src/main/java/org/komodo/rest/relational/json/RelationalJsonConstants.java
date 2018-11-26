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

import org.komodo.rest.json.JsonConstants;

/**
 * Identifiers used in Komodo REST relational object JSON representations.
 */
public interface RelationalJsonConstants extends JsonConstants {

    /**
     * Relational prefix
     */
    String RELATIONAL_PREFIX = "vdb" + PREFIX_SEPARATOR; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_CREATE_TEMP_TABLES = RELATIONAL_PREFIX + "allowCreateTempTables"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_ALTER = RELATIONAL_PREFIX + "allowAlter"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_CREATE = RELATIONAL_PREFIX + "allowCreate"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_DELETE = RELATIONAL_PREFIX + "allowDelete"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_EXECUTE = RELATIONAL_PREFIX + "allowExecute"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_LANGUAGE = RELATIONAL_PREFIX + "allowLanguage"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_READ = RELATIONAL_PREFIX + "allowRead"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_UPDATE = RELATIONAL_PREFIX + "allowUpdate"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ANY_AUTHENTICATED = RELATIONAL_PREFIX + "anyAuthenticated"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String CONDITIONS = RELATIONAL_PREFIX + "conditions"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String NAME = RELATIONAL_PREFIX + "name"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String DESCRIPTION = RELATIONAL_PREFIX + "description"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String GRANT_ALL = RELATIONAL_PREFIX + "grantAll"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String IMPORT_DATA_POLICIES = RELATIONAL_PREFIX + "importDataPolicies"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String MAPPED_ROLES = RELATIONAL_PREFIX + "mappedRoles"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String MASKS = RELATIONAL_PREFIX + "masks"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ORIGINAL_FILE = RELATIONAL_PREFIX + "originalFile"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String PATH = RELATIONAL_PREFIX + "path"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String PERMISSIONS = RELATIONAL_PREFIX + "permissions"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String TYPE = RELATIONAL_PREFIX + "type"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String VDBS = RELATIONAL_PREFIX + "vdbs"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String VERSION = RELATIONAL_PREFIX + "version"; //$NON-NLS-1$

}
