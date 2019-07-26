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
package org.komodo.spi.repository;

import org.komodo.spi.SystemConstants;

public enum PersistenceType {

    /**
     * H2 database for testing purposes
     */
    H2 ("jdbc:h2:file:${" + SystemConstants.ENGINE_DATA_DIR + "}/komododb;AUTO_SERVER=TRUE",
            "jdbc:h2:file:${" + SystemConstants.ENGINE_DATA_DIR + "}/storage/content/binaries;DB_CLOSE_DELAY=-1",
            "org.h2.Driver", false),

    /**
     * PostgreSQL database
     */
    PGSQL ("jdbc:postgresql://${" + SystemConstants.REPOSITORY_PERSISTENCE_HOST + "}/komodo",
                   "org.postgresql.Driver", true);

    private String connUrl;

    private String binaryUrl;

    private String driver;

    private boolean external;

    PersistenceType(String connUrl, String driver, boolean external) {
        this.connUrl = connUrl;
        this.binaryUrl = this.connUrl; // Assumes urls point to the same location
        this.driver = driver;
        this.external = external;
    }

    PersistenceType(String connUrl, String binaryUrl, String driver, boolean external) {
        this(connUrl, driver, external);
        this.binaryUrl = binaryUrl;
    }

    public String getConnUrl() {
        return connUrl;
    }

    public String getBinaryStoreUrl() {
    	if (binaryUrl == null) {
    		return getConnUrl();
    	}
        return binaryUrl;
    }

    public String getDriver() {
        return driver;
    }

    /**
     * In cases like PGSQL, the data store is externally managed while H2 is internally managed.
     * This flag distinguishes between the two types of store.
     *
     * @return true if the data store is external to the application, false otherwise.
     */
    public boolean isExternal() {
        return external;
    }
}