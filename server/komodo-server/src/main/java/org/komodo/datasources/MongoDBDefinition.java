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
package org.komodo.datasources;

import java.util.Map;
import java.util.Properties;

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
    public Properties getDataSourceProperties(DefaultSyndesisDataSource source) {
        /*
        {
              "DATABASE_SERVICE_NAME":"mongodb",
              "MEMORY_LIMIT":"512Mi",
              "MONGODB_ADMIN_PASSWORD" "pwd",
              "MONGODB_DATABASE":"sampledb",
              "MONGODB_PASSWORD": "pass",
              "MONGODB_USER": "user",
              "MONGODB_VERSION":"3.2",
              "NAMESPACE":"openshift",
              "VOLUME_CAPACITY":"1Gi"
              admin_password
              database_name
              password
              uri
              username
        }
        */
        Properties props = new Properties();
        props.setProperty("remoteServerList", source.getProperty("DATABASE_SERVICE_NAME")+":27017");
        props.setProperty("username", source.getProperty("username"));
        props.setProperty("password", source.getProperty("password"));
        props.setProperty("database", source.getProperty("database_name"));
        props.setProperty("class-name", "org.teiid.resource.adapter.mongodb.MongoDBManagedConnectionFactory");
        props.setProperty("securityType", "SCRAM_SHA_1");
        props.setProperty("authDatabase", source.getProperty("database_name"));
        props.setProperty("ssl", "false");
        return props;
    }

    @Override
    public Properties getWFSDataSourceProperties(DefaultSyndesisDataSource scd, String jndiName) {
        Properties props = setupResourceAdapter(scd.getName(), "org.jboss.teiid.resource-adapter.mongodb",
                "org.teiid.resource.adapter.mongodb.MongoDBManagedConnectionFactory", jndiName);
        ds(props, scd, "SecurityType", "SCRAM_SHA_1");
        ds(props, scd, "Ssl", "false");
        ds(props, scd, "AuthDatabase", scd.canonicalEnvKey("database_name"));
        ds(props, scd, "RemoteServerList", scd.canonicalEnvKey("DATABASE_SERVICE_NAME")+":27017");
        ds(props, scd, "Database", scd.canonicalEnvKey("database_name"));
        ds(props, scd, "Username", scd.canonicalEnvKey("username"));
        ds(props, scd, "Password", scd.canonicalEnvKey("password"));
        return props;
    }
    

}
