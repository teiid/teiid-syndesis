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
import org.komodo.spi.logging.KLogger;

public class ApplicationProperties implements SystemConstants {

    /**
     * Gets the namespace
     */
    public static String getNamespace() {
        String namespace = System.getenv("NAMESPACE");
        if (namespace == null) {
            namespace = System.getProperty("NAMESPACE");
        }
        return namespace;
    }

    /**
     * Gets the log level or defaults to INFO
     */
    public static KLogger.Level getLogLevel() {
        String value = System.getProperty("LOGLEVEL");
        KLogger.Level level = KLogger.Level.level(value); 
        return level != null ? level : KLogger.Level.INFO;
    }
    
    /**
     * Gets the type system property for connection persistence.
     * If none is defined then return the default {@link PersistenceType#PGSQL} type name
     */
    public static String getRepositoryPersistenceType() {
        String type = System.getProperty(REPOSITORY_PERSISTENCE_TYPE);
        if (type == null)
            type = PersistenceType.PGSQL.name();

        return type;
    }

    /**
     * Sets the type system property for connection persistence
     *
     * @param type
     */
    public static void setRepositoryPersistenceType(String type) {
        System.setProperty(REPOSITORY_PERSISTENCE_TYPE, type);
    }

    /**
     * Gets the host system property for connection persistence.
     * If none is defined then the null is returned.
     */
    public static String getRepositoryPersistenceHost() {
        return System.getProperty(REPOSITORY_PERSISTENCE_HOST);
    }

    /**
     * Sets the host system property for connection persistence
     *
     * @param host
     */
    public static void setRepositoryPersistenceHost(String host) {
        System.setProperty(REPOSITORY_PERSISTENCE_HOST, host);
    }

    /**
     * Gets the driver system property for connection persistence.
     * If none is defined then the null is returned.
     */
    public static String getRepositoryPersistenceDriver() {
        return System.getProperty(REPOSITORY_PERSISTENCE_CONNECTION_DRIVER);
    }

    /**
     * Sets the driver system property for connection persistence
     *
     * @param driver
     */
    public static void setRepositoryPersistenceDriver(String driver) {
        System.setProperty(REPOSITORY_PERSISTENCE_CONNECTION_DRIVER, driver);
    }

    /**
     * Gets the connection url system property for connection persistence.
     * If none is defined then the null is returned.
     *
     * Should the connection url utilised the host environment variable,
     * ie. ${
     */
    public static String getRepositoryPersistenceURL() {
        String connUrl = System.getProperty(REPOSITORY_PERSISTENCE_CONNECTION_URL);
        if (connUrl == null)
            return connUrl;

        String host = getRepositoryPersistenceHost();
        if (host != null) {
            connUrl = substitute(connUrl, REPOSITORY_PERSISTENCE_HOST);
        }
            
        return connUrl;
    }

    /**
     * Sets the connection url system property for connection persistence
     *
     * @param url
     */
    public static void setRepositoryPersistenceURL(String url) {
        System.setProperty(REPOSITORY_PERSISTENCE_CONNECTION_URL, url);
    }

    /**
     * Gets the binary store url system property for connection persistence.
     * If none is defined then the null is returned.
     */
    public static String getRepositoryPersistenceBinaryStoreURL() {
        return System.getProperty(REPOSITORY_PERSISTENCE_BINARY_STORE_URL);
    }

    /**
     * Sets the binary store url system property for connection persistence
     *
     * @param url
     */
    public static void setRepositoryPersistenceBinaryStoreURL(String url) {
        System.setProperty(REPOSITORY_PERSISTENCE_BINARY_STORE_URL, url);
    }

    /**
     * Gets the default user for connection persistence.
     */
    public static String getRepositoryPersistenceDefaultUser() {
        return REPOSITORY_PERSISTENCE_CONNECTION_USERNAME_DEFAULT;
    }

    /**
     * Gets the user system property for connection persistence.
     * If none is defined then the default is returned.
     */
    public static String getRepositoryPersistenceUser() {
        return System.getProperty(REPOSITORY_PERSISTENCE_CONNECTION_USERNAME, getRepositoryPersistenceDefaultUser());
    }

    /**
     * Sets the user system property for connection persistence
     *
     * @param user
     */
    public static void setRepositoryPersistenceUser(String user) {
        System.setProperty(REPOSITORY_PERSISTENCE_CONNECTION_USERNAME, user);
    }

    /**
     * Gets the default password for connection persistence.
     */
    public static String getRepositoryPersistenceDefaultPassword() {
        return REPOSITORY_PERSISTENCE_CONNECTION_PASSWORD_DEFAULT;
    }

    /**
     * Gets the password system property for connection persistence.
     * If none is defined then the default is returned.
     */
    public static String getRepositoryPersistencePassword() {
        return System.getProperty(REPOSITORY_PERSISTENCE_CONNECTION_PASSWORD,
                                  getRepositoryPersistenceDefaultPassword());
    }

    /**
     * Sets the password system property for connection persistence
     *
     * @param password
     */
    public static void setRepositoryPersistencePassword(String password) {
        System.setProperty(REPOSITORY_PERSISTENCE_CONNECTION_PASSWORD, password);
    }

    /**
     * Replaces in the target any instances of the given system property with its value
     * 
     * @param target
     * @param property
     * @return the value with any instances of the given system property replaced
     */
    public static String substitute(String target, String property) {
        String propValue = System.getProperty(property);
        if (propValue == null) {
            return target; // Just confuse if value returns null
        }
    
        String template = DOLLAR_SIGN + OPEN_BRACE + property + CLOSE_BRACE;
        target = target.replace(template, propValue);
        return target;
    }

    /**
     * Get a arbitrary property given by the name. first look in system properties, then in environment properties
     * @param property Name of the property
     */
    public static String getProperty(String property, String defalt) {
        String propValue = System.getProperty(property);
        if (propValue != null) {
        	return propValue;
        }
        propValue = System.getenv(property);
        if (propValue != null) {
        	return propValue;
        }
        return defalt;
    }
}
