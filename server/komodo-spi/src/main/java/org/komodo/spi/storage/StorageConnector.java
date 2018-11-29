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

import java.io.InputStream;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A storage connector is a client to a certain type of storage.
 * Implementations will provide access to instances of their storage
 * repositories, allowing writes to the storage and retrieval of
 * individual artifacts
 */
public interface StorageConnector extends StorageConnectorConstants {

    /**
     * Describes a parameter/property applicable to this storage connector
     */
    static class Descriptor {

        private String name;

        private boolean required;

        private String description;

        private boolean encoded = false;

        public Descriptor(String name, boolean required, boolean encoded, String description) {
            this.name = name;
            this.required = required;
            this.description = description;
            this.encoded = encoded;
        }

        public Descriptor(String name, boolean required, String description) {
            this(name, required, false, description);
        }

        public String getName() {
            return name;
        }

        public boolean isRequired() {
            return required;
        }

        public String getDescription() {
            return description;
        }

        public boolean isEncoded() {
            return encoded;
        }
    }

    /**
     * @return the id of the connector
     */
    StorageConnectorId getId();

    /**
     * @return the applicable parameters for this storage connector
     */
    Set<Descriptor> getDescriptors();

    /**
     * Write the {@link Exportable} to the storage according to the
     * parameters
     *
     * @param artifact
     * @param parameters
     *
     * @throws Exception if error occurs
     */
    void write(Exportable artifact, UnitOfWork transaction, Properties parameters) throws Exception;

    /**
     * Refreshes the connection and any cached files from the storage location
     * @return true if refresh was successful, false otherwise
     * @throws Exception 
     */
    boolean refresh() throws Exception;

    /**
     * @param parameters the parameters used to find and read the document.
     *                  In most cases this should at least contain {@link #FILE_PATH_PROPERTY} which
     *                  points to  relative reference to the file
     * @return input stream to the file located at the given location
     * @throws Exception
     */
    InputStream read(Properties parameters) throws Exception;

    /**
     * @return the walked tree-structure of the storage location.
     * Note. if the storage is not browseable then this will return <code>null</code>.
     * @throws Exception 
     */
    StorageTree<String> browse() throws Exception;

    /**
     * Dispose of this connector
     */
    void dispose();
}
