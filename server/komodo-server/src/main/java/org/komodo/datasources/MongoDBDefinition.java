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
            "  <groupId>org.teiid</groupId>" +
            "  <artifactId>thorntail-mongodb</artifactId>" +
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
    public Properties getInternalTeiidDataSourceProperties(DefaultSyndesisDataSource source) {
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
    public Properties getPublishedImageDataSourceProperties(DefaultSyndesisDataSource scd, String jndiName) {
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
