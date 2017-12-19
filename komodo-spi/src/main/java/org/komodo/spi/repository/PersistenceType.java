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
package org.komodo.spi.repository;

import org.komodo.spi.constants.SystemConstants;

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

    /**
     * @return the connection url with any know system properties replaced with their values
     */
    public String getEvaluatedConnUrl() {
        String url = ApplicationProperties.substitute(connUrl, SystemConstants.ENGINE_DATA_DIR);
        url = ApplicationProperties.substitute(connUrl, SystemConstants.REPOSITORY_PERSISTENCE_HOST);
        return url;
    }

    public String getBinaryStoreUrl() {
    	if (binaryUrl == null) {
    		return getConnUrl();
    	}
        return binaryUrl;
    }

    /**
     * @return the binary store url with any know system properties replaced with their values
     */
    public String getEvaluatedBinaryStoreUrl() {
        String url = ApplicationProperties.substitute(binaryUrl, SystemConstants.ENGINE_DATA_DIR);
        url = ApplicationProperties.substitute(binaryUrl, SystemConstants.REPOSITORY_PERSISTENCE_HOST);
        return url;
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