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

public class MongoDBDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
        return "mongodb";
    }

    @Override
    public String getPomDendencies() {
        return  "<dependency>" +
            "  <groupId>org.mongodb</groupId>" +
            "  <artifactId>mongo-java-driver</artifactId>" +
            "  <version>${version.org.mongodb}</version>" +
            "</dependency>" +
            "<dependency>" +
            "  <groupId>org.wildfly.swarm</groupId>" +
            "  <artifactId>teiid-mongodb</artifactId>" +
            "</dependency>";
    }

    @Override
    public String getTranslatorName() {
        return "mongodb";
    }

    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("MONGODB_DATABASE") != null)) {
            return true;
        }
        return false;
    }

    @Override
    public boolean isResouceAdapter() {
        return true;
    }

    @Override
    public Properties getDataSourceProperties(DefaultServiceCatalogDataSource source) {
        /*
        {
              "DATABASE_SERVICE_NAME":"mongodb",
              "MEMORY_LIMIT":"512Mi",
              "MONGODB_DATABASE":"sampledb",
              "MONGODB_VERSION":"3.2",
              "NAMESPACE":"openshift",
              "VOLUME_CAPACITY":"1Gi"
              database-password
              database-name
              database-admin-password
              database-user
        }
        */
        Properties props = new Properties();
        props.setProperty("remoteServerList", source.getProperty("DATABASE_SERVICE_NAME")+":27017");
        props.setProperty("username", source.getProperty("database-user"));
        props.setProperty("password", source.getProperty("database-password"));
        props.setProperty("database", source.getProperty("database-name"));
        props.setProperty("securityType", "SCRAM_SHA_1");
        props.setProperty("authDatabase", source.getProperty("database-name"));
        props.setProperty("ssl", "false");
        return props;
    }

    @Override
    public Properties getWFSDataSourceProperties(DefaultServiceCatalogDataSource scd, String jndiName) {
        Properties props = new Properties();

        // consult Teiid documents for all the properties; Then map to properties from OpenShift Service
        props.setProperty("swarm.resource-adapter.resource-adapters." + scd.getName() + ".module=",
                "org.jboss.teiid.resource-adapter.mongodb");

        ds(props, scd, "class-name", "org.teiid.resource.adapter.mongodb.MongoDBManagedConnectionFactory");
        ds(props, scd, "jndi-name", jndiName);
        ds(props, scd, "enabled", "true");
        ds(props, scd, "use-java-context", "true");
        ds(props, scd, "SecurityType", "SCRAM_SHA_1");
        ds(props, scd, "Ssl", "false");
        ds(props, scd, "AuthDatabase", scd.canonicalEnvKey("database_name"));
        ds(props, scd, "RemoteServerList", scd.canonicalEnvKey("DATABASE_SERVICE_NAME")+":27017");
        ds(props, scd, "Database", scd.canonicalEnvKey("database_name"));
        ds(props, scd, "Username", scd.canonicalEnvKey("database-user"));
        ds(props, scd, "Password", scd.canonicalEnvKey("database-password"));
        return props;
    }
}
