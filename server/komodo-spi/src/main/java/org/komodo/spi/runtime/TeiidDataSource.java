/*
 * Copyright (C) 2016 Red Hat, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.spi.runtime;

import java.util.Properties;

/**
 *
 */
public interface TeiidDataSource {

    /**
     * The data source jndi property name.  Value is {@value} .
     */
    String DATASOURCE_JNDINAME = "jndi-name";  //$NON-NLS-1$

    /**
     * The data source className property name.  Value is {@value} .
     */
    String DATASOURCE_CLASSNAME = "class-name";  //$NON-NLS-1$

    /**
     * The data source driver property name.  Value is {@value} .
     */
    String DATASOURCE_DRIVERNAME = "driver-name";  //$NON-NLS-1$

    /**
     * The connection url property name. Value is {@value}.
     */
    String DATASOURCE_CONNECTION_URL = "url"; //$NON-NLS-1$

    /**
     * The display name property.
     */
    String DATASOURCE_DISPLAYNAME = "display-name"; //$NON-NLS-1$

	public interface ERROR_CODES {
		// Data Source issues
		int JDBC_DRIVER_SOURCE_NOT_FOUND = 101; // JDBC Source for Driver class {0} was not found on teiid instance {1}
		int DATA_SOURCE_TYPE_DOES_NOT_EXIST_ON_TEIID = 102; // Data Source Type {0} does not exist on teiid instance {1}
		int DATA_SOURCE_COULD_NOT_BE_CREATED = 103; // errorCreatingDataSource = Data Source {0} could not be created for type {1}
		int NO_CONNECTION_PROVIDER = 104; //
		int NO_CONNECTION_PROFILE_DEFINED_IN_MODEL = 105; //
		int NO_TEIID_RELATED_PROPERTIES_IN_PROFILE = 106; //
		int COULD_NOT_GET_OR_CREATE_DATASOURCE = 107; //
	}

    /**
     * @return display name of data source
     */
    String getDisplayName();

    /**
     * @return real name of data source, maybe different from display name
     */
    String getName();

    /**
     * Returns the data source type name
     *
     * @return the type
     */
    String getType();

    /**
     * Returns the data source jndi name
     *
     * @return the jndi name
     */
    String getJndiName();

    /**
     * Returns the data source connection url
     *
     * @return the connection url
     */
    String getConnectionUrl();

    /**
     * @return properties of data source
     */
    Properties getProperties();

    /**
     * @param name
     *
     * @return value of named property
     */
    String getPropertyValue(String name);

}
