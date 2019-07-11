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

	/**
	 * System user for transactions to be executed internally
	 */
	String SYSTEM_USER = "SYSTEM";
}
