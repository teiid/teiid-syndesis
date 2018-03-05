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

public class PostgreSQLDefinition extends DataSourceDefinition {

    @Override
    public String getName() {
        return "postgresql";
    }

    @Override
    public String getPomDendencies() {
        return "<dependency>" +
            "  <groupId>org.postgresql</groupId>" +
            "  <artifactId>postgresql</artifactId>" +
            "  <version>${version.postgresql}</version>" +
            "</dependency>\n" +
            "<dependency>" +
            "  <groupId>org.wildfly.swarm</groupId>" +
            "   <artifactId>teiid-jdbc</artifactId>" +
            "</dependency>";
    }

    @Override
    public String getTranslatorName() {
        return "postgresql";
    }

    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("POSTGRESQL_DATABASE") != null)) {
            return true;
        }
        return false;
    }

    @Override
    public Properties getDataSourceProperties(DefaultServiceCatalogDataSource source) {
        /*
         {
           "DATABASE_SERVICE_NAME":"postgresql",
           "MEMORY_LIMIT":"512Mi",
           "NAMESPACE":"openshift",
           "database_name":"sampledb",
           "password":"pass",
           "username":"user",
           "uri": "postgres://172.30.145.26:5432",
           "POSTGRESQL_VERSION":"9.5",
           "VOLUME_CAPACITY":"1Gi"
        }
        */
        Properties props = new Properties();
        props.setProperty("connection-url", "jdbc:postgresql://" + source.getProperty("DATABASE_SERVICE_NAME") + ":5432/"
                + source.getProperty("database_name"));
        props.setProperty("user-name", source.getProperty("username"));
        props.setProperty("password", source.getProperty("password"));
        return props;
    }

    @Override
    public Properties getWFSDataSourceProperties(DefaultServiceCatalogDataSource scd, String jndiName) {
        Properties props = new Properties();
        ds(props, scd, "driver-name", scd.getType());
        ds(props, scd, "jndi-name", jndiName);
        ds(props, scd, "connection-url", "jdbc:postgresql://" + scd.canonicalEnvKey("DATABASE_SERVICE_NAME") + ":5432/"
                + scd.canonicalEnvKey("database_name"));
        ds(props, scd, "user-name", scd.canonicalEnvKey("username"));
        ds(props, scd, "password", scd.canonicalEnvKey("password"));
        return props;
    }
}
