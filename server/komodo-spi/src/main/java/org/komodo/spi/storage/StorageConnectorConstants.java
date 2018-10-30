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
