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
package org.komodo.servicecatalog.datasources;

import java.util.Map;
import java.util.Properties;

import org.komodo.servicecatalog.DataSourceDefinition;

public class SalesforceDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
        return "salesforce";
    }

    @Override
    public String getPomDendencies() {
        return
            "<dependency>" +
            "  <groupId>org.wildfly.swarm</groupId>" +
            "  <artifactId>teiid-salesforce-41</artifactId>" +
            "</dependency>";
    }

    @Override
    public String getTranslatorName() {
        return "salesforce-41";
    }

    @Override
    public boolean isResouceAdapter() {
        return true;
    }
    
    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("SALESFORCE_URL") != null)) {
            return true;
        }
        return false;
    }
    
    @Override
    public Properties getDataSourceProperties(DefaultServiceCatalogDataSource source) {
        Properties props = new Properties();
        props.setProperty("class-name", "org.teiid.resource.adapter.salesforce.SalesForceManagedConnectionFactory");
        props.setProperty("URL", source.getProperty("url"));
        props.setProperty("username", source.getProperty("username"));
        props.setProperty("password", source.getProperty("password"));
        return props;
    }    

    @Override
    public Properties getWFSDataSourceProperties(DefaultServiceCatalogDataSource scd, String jndiName) {
        Properties props = setupResourceAdapter(scd.getName(), "org.jboss.teiid.resource-adapter.salesforce-41",
                "org.teiid.resource.adapter.salesforce.SalesForceManagedConnectionFactory", jndiName);
        ds(props, scd, "URL", scd.canonicalEnvKey("url"));
        ds(props, scd, "username", scd.canonicalEnvKey("username"));
        ds(props, scd, "password", scd.canonicalEnvKey("password"));
        return props;
    }
}
