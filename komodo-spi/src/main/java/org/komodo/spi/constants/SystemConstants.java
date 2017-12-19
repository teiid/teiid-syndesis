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
package org.komodo.spi.constants;

/**
 * Constants associated with the Komodo system and environment.
 */
public interface SystemConstants extends StringConstants {

    /**
     * The environmental variable that can be set with the directory the Komodo engine will use while running. Default is
     * <code>${user.home}/.komodo</code>.
     */
    String ENGINE_DATA_DIR = "komodo.dataDir"; //$NON-NLS-1$

    /**
     * The Komodo log file name.
     */
    String LOG_FILE_NAME = KOMODO + DOT + LOG;

    /**
     * The environmental variable that can be set with the directory VDB Builder will use while running. Default is
     * <code>${user.home}/.komodo/vdbbuilder</code>
     */
    String VDB_BUILDER_DATA_DIR = "vdbbuilder.dataDir"; //$NON-NLS-1$

    /**
     * The environment variable that defines the type of the persistence to be used for modeshape.
     * Values are either PGSQL or H2.
     */
    String REPOSITORY_PERSISTENCE_TYPE = "komodo.repositoryPersistenceType";

    /**
     * The environment variable that defines the name of the host where the persistence store is located
     * Cannot use 'komodo.' prefix since Openshift considers this invalid.
     *
     * This remains a convenience variable for changing the host of the persistence store but keeping the
     * jdbc connection url as specified by PersistenceType.PGSQL, ie.
     * jdbc:postgresql://${REPOSITORY_PERSISTENCE_HOST}/komodo.
     *
     * This value is subject to being overwritten by {@link #REPOSITORY_PERSISTENCE_CONNECTION_URL}
     * if defined.
     */
    String REPOSITORY_PERSISTENCE_HOST = "komodo.repositoryPersistenceHost";

    /**
     * The environment variable that defines the connection url of the persistence database.
     * Values should be in the format "jdbc:<type>:<path>"
     */
    String REPOSITORY_PERSISTENCE_CONNECTION_URL = "komodo.connectionUrl";

    /**
     * The environment variable that defines the connection url to the binary store of
     * the persistence database.
     * Values should be in the format "jdbc:<type>:<path>"
     */
    String REPOSITORY_PERSISTENCE_BINARY_STORE_URL = "komodo.binaryStoreUrl";

    /**
     * The environment variable that defines the driver used for connection to the persistence database
     */
    String REPOSITORY_PERSISTENCE_CONNECTION_DRIVER = "komodo.connectionDriver";

    /**
     * The environment variable that defines the user name for connection to the persistence database
     */
    String REPOSITORY_PERSISTENCE_CONNECTION_USERNAME = "komodo.user";

    /**
     * The default value of the user name for connection to the persistence database
     */
    String REPOSITORY_PERSISTENCE_CONNECTION_USERNAME_DEFAULT = "komodo";

    /**
     * The environment variable that defines the password for connection to the persistence database
     */
    String REPOSITORY_PERSISTENCE_CONNECTION_PASSWORD = "komodo.password";

    /**
     * The default value of the password for connection to the persistence database
     */
    String REPOSITORY_PERSISTENCE_CONNECTION_PASSWORD_DEFAULT = "komodo";
}
