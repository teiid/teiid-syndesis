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
package org.komodo.servicecatalog;

import java.util.Map;
import java.util.Properties;

import org.komodo.servicecatalog.datasources.DefaultServiceCatalogDataSource;

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
    public Properties getDataSourceProperties(DefaultServiceCatalogDataSource source) {
        Properties props = new Properties();
        if (source.getParameters() != null && source.getParameters().getData() != null) {
            props.putAll(source.getParameters().getData());
        }
        if (source.getCredentials() != null && source.getCredentials().getData() != null) {
            props.putAll(source.getCredentials().getData());
        }
        return props;
    }

    /**
     * Given the connection properties from the Service Catalog secrets generate WildFly Swarm's
     * configuration file to configure the data source
     * @param datasource data source details
     * @param jndiName JNDI Name of the source
     * @return properties properties required to create a connection in target environment
     */
    public abstract Properties getWFSDataSourceProperties(DefaultServiceCatalogDataSource datasource, String jndiName);

    /**
     * Returns true if the data source type is a resource adapter, otherwise false.
     * @return true if resource adapter
     */
    public boolean isResouceAdapter() {
        return false;
    }

    protected void ds(Properties props, DefaultServiceCatalogDataSource scd, String key, String value) {
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
                    "swarm.datasources.data-sources." + scd.getName() + "." + key,
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
    
    /**
     * If source is provisioned through a service catalog this routine returns true. For sources like file, webserive
     * this will return false.
     * @return
     */
    public boolean isServiceCatalogSource() {
        return true;
    }
}