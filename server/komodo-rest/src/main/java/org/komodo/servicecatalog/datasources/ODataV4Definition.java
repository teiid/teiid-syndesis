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

public class ODataV4Definition extends DataSourceDefinition {

    @Override
    public String getName() {
        return "odata4";
    }

    @Override
    public String getPomDendencies() {
        return
            "<dependency>" +
            "  <groupId>org.wildfly.swarm</groupId>" +
            "  <artifactId>teiid-odata-v4</artifactId>" +
            "</dependency>";
    }

    @Override
    public String getTranslatorName() {
        return "odata4";
    }

    @Override
    public boolean matches(Map<String, String> properties) {
        // this only applies to service catalog based sources. WS never will be Service Catalog Source
        return false;
    }

    @Override
    public boolean isResouceAdapter() {
        return true;
    }

    @Override
    public Properties transformProperties(Map<String, String> svcProperties) {
        Properties props = new Properties();
        props.putAll(svcProperties);
        return props;
    }

    @Override
    public Properties getDatasourceConfiguration(DefaultServiceCatalogDataSource scd, String jndiName) {
        Properties props = new Properties();

        // consult Teiid documents for all the properties; Then map to properties from OpenShift Service
        props.setProperty(" -Dswarm.resource-adapter.resource-adapters." + scd.getName() + ".module=",
                "org.jboss.teiid.resource-adapter.webservice");

        ds(props, scd, "class-name", "org.teiid.resource.adapter.ws.WSManagedConnectionFactory");
        ds(props, scd, "jndi-name", jndiName);
        ds(props, scd, "enabled", "true");
        ds(props, scd, "use-java-context", "true");
        return props;
    }
}
