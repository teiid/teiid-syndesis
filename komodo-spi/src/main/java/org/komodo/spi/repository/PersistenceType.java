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

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;

public enum PersistenceType {

    /**
     * H2 database for testing purposes
     */
    H2 ("jdbc:h2:file:${komodo.dataDir}/komododb;AUTO_SERVER=TRUE",
            "jdbc:h2:file:${komodo.dataDir}/storage/content/binaries;DB_CLOSE_DELAY=-1",
            "org.h2.Driver", false),

    /**
     * PostgreSQL database used for production
     */
    PGSQL (System.getProperty(SystemConstants.REPOSITORY_PERSISTENCE_CONNECTION_URL),
    		System.getProperty(SystemConstants.REPOSITORY_PERSISTENCE_CONNECTION_DRIVER), true);

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

    private String substitute(String value, String property) {
        String propValue = System.getProperty(property);
        if (propValue == null) {
            return value; // Just confuse if value returns null
        }

        String target = StringConstants.DOLLAR_SIGN + StringConstants.OPEN_BRACE +
                                                  property + StringConstants.CLOSE_BRACE;
        value = value.replace(target, propValue);
        return value;
    }

    public String getConnUrl() {
        return connUrl;
    }

    /**
     * @return the connection url with any know system properties replaced with their values
     */
    public String getEvaluatedConnUrl() {
        String url = substitute(getConnUrl(), SystemConstants.ENGINE_DATA_DIR);
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
        String url = substitute(getBinaryStoreUrl(), SystemConstants.ENGINE_DATA_DIR);
        return url;
    }

    public String getDriver() {
        return driver;
    }
    
    public String getUser() {
    	return System.getProperty(SystemConstants.REPOSITORY_PERSISTENCE_CONNECTION_USERNAME);
    }

    public String getPassword() {
    	return System.getProperty(SystemConstants.REPOSITORY_PERSISTENCE_CONNECTION_PASSWORD);
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