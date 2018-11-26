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
package org.komodo.spi.storage;

import org.komodo.spi.constants.StringConstants;

/**
 * Constants for use with the {@link StorageConnector}
 */
public interface StorageConnectorConstants extends StringConstants {

    /**
     * The types of storage connector currently supported.
     */
    enum Types {
        GIT, FILE;

        public String id() {
            return this.name().toLowerCase();
        }
    }

    /**
     * The path to the home directory of the location of files. Used by some connectors.
     */
    String FILES_HOME_PATH_PROPERTY = "files-home-path-property";
    /**
     * The path where the file should be located
     */
    String FILE_PATH_PROPERTY = "file-path-property";
    /**
     * Should a file be 'downloadable' once stored then this property is populated
     */
    String DOWNLOADABLE_PATH_PROPERTY = "downloadable-path-property";
    /**
     * Parameter to specify overwrite option for imports
     */
    String IMPORT_OVERWRITE_PROPERTY = "import-overwrite-property";

}
