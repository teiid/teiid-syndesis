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
package org.komodo.datasources;

import java.util.Map;
import java.util.Properties;

/**
 * Service catalog based Data Services that are available
 */
public abstract class DataSourceDefinition {
    /**
     * Returns the type of the database. Matches with the translator name
     * @return name of the source type
     */
    public abstract String getType();

    /**
     * @return return the text to include in pom.xml as dependencies for this source
     */
    public abstract String getPomDendencies();

    /**
     * Returns the matching translator type
     * @return translator name
     */
    public abstract String getTranslatorName();

    /**
     * Check to see if the properties given match to the Data Source definition, using which
     * connection can be made.
     * @param properties
     * @return true if the properties match
     */
    public boolean isTypeOf(Map<String, String> properties) {
        return false;
    }

    /**
     * Given the service specific properties to engine specific properties.
     * @param properties
     * @return return the modified property set to make the connection in the given environment
     */
    public abstract Properties getInternalTeiidDataSourceProperties(DefaultSyndesisDataSource source);

    /**
     * Given the connection properties from the Syndesis secrets generate Spring Boot
     * configuration file to configure the data source
     * @param datasource data source details
     * @return properties properties required to create a connection in target environment
     */
    public abstract Properties getPublishedImageDataSourceProperties(DefaultSyndesisDataSource datasource);

    protected void ds(Properties props, DefaultSyndesisDataSource scd, String key, String value) {
        props.setProperty(
                "spring.datasource." + scd.getName() + "." + key,
                value);
    }

    protected Properties setupResourceAdapter(String dsName, String moduleName, String className, String jndiName) {
    	throw new UnsupportedOperationException();
    }
}