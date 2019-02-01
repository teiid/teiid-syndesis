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
     * Returns the type of the database. Matches with name of driver in WildFlySwarm
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
     * @param jndiName JNDI Name of the source
     * @return properties properties required to create a connection in target environment
     */
    public abstract Properties getPublishedImageDataSourceProperties(DefaultSyndesisDataSource datasource, String jndiName);

    /**
     * Returns true if the data source type is a resource adapter, otherwise false.
     * @return true if resource adapter
     */
    public boolean isResouceAdapter() {
        return false;
    }

    protected void ds(Properties props, DefaultSyndesisDataSource scd, String key, String value) {
        if (scd.getDefinition().isResouceAdapter()) {
            props.setProperty(
                "swarm.resource-adapters.resource-adapters."
                        + scd.getName()
                        + ".connection-definitions."
                        + scd.getName()
                        + ".config-properties."
                        + key
                        + ".value",
                    value);
        } else {
            props.setProperty(
                    "spring.datasource." + scd.getName() + "." + key,
                    value);
        }
    }

    protected Properties setupResourceAdapter(String dsName, String moduleName, String className, String jndiName) {
        Properties props = new Properties();
        // consult Teiid documents for all the properties; Then map to properties from
        // OpenShift Service
        String raPrefix = "swarm.resource-adapters.resource-adapters." + dsName + ".";
        String cdPrefix = raPrefix + "connection-definitions." + dsName + ".";
        props.setProperty(raPrefix + "module", moduleName);
        props.setProperty(raPrefix + "transaction-support", "NoTransaction");
        props.setProperty(cdPrefix + "class-name", className);
        props.setProperty(cdPrefix + "jndi-name", jndiName);
        props.setProperty(cdPrefix + "enabled", "true");
        props.setProperty(cdPrefix + "use-java-context", "true");
        return props;
    }
}