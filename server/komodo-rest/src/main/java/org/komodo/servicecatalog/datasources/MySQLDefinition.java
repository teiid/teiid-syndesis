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

public class MySQLDefinition extends DataSourceDefinition {

    @Override
    public String getName() {
        return "mysql";
    }

    @Override
    public String getPomDendencies() {
        return "<dependency>" +
          "  <groupId>mysql</groupId>" +
          "  <artifactId>mysql-connector-java</artifactId>" +
          "  <version>${version.mysql}</version>" +
          "</dependency>\n" +
          "<dependency>" +
          "  <groupId>org.wildfly.swarm</groupId>" +
          "   <artifactId>teiid-jdbc</artifactId>" +
          "</dependency>";

    }

    @Override
    public String getTranslatorName() {
        return "mysql5";
    }

    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("MYSQL_DATABASE") != null)) {
            return true;
        }
        return false;
    }

    @Override
    public Properties getDataSourceProperties(DefaultServiceCatalogDataSource source) {
        /*
        {
          "DATABASE_SERVICE_NAME":"mariadb",
          "MEMORY_LIMIT":"512Mi",
          "MYSQL_DATABASE":"sampledb",
          "NAMESPACE":"openshift",
          "VOLUME_CAPACITY":"1Gi"
          database-password
          database-name
          database-root-password
          database-user
        }
         */
        Properties props = new Properties();
        
        if (source.getProperty("database-name") != null) {
            props.setProperty("connection-url", "jdbc:mysql://" + source.getProperty("DATABASE_SERVICE_NAME") + ":3306/"
                    + source.getProperty("database-name"));
        } else if (source.getProperty("database_name") != null) {
            props.setProperty("connection-url", "jdbc:mysql://" + source.getProperty("DATABASE_SERVICE_NAME") + ":3306/"
                    + source.getProperty("database_name"));
        } 
        
        if (source.getProperty("database-user") != null) {
            props.setProperty("user-name", source.getProperty("database-user"));
        } else if (source.getProperty("username") != null) {
            props.setProperty("user-name", source.getProperty("username"));
        }
        
        if (source.getProperty("database-password") != null) {
            props.setProperty("password", source.getProperty("database-password"));
        } else if (source.getProperty("password") != null ) {
            props.setProperty("password", source.getProperty("password"));
        }
        return props;
    }

    @Override
    public Properties getWFSDataSourceProperties(DefaultServiceCatalogDataSource scd, String jndiName) {
        Properties props = new Properties();
        ds(props, scd, "driver-name", scd.getType());
        ds(props, scd, "jndi-name", jndiName);
        
        // support different templates with different properties
        if (scd.getProperty("database-name") != null) {
            ds(props, scd, "connection-url", "jdbc:mysql://" + scd.canonicalEnvKey("DATABASE_SERVICE_NAME") + ":3306/"
                    + scd.canonicalEnvKey("database-name"));
        } else if (scd.getProperty("database_name") != null) {
            ds(props, scd, "connection-url", "jdbc:mysql://" + scd.canonicalEnvKey("DATABASE_SERVICE_NAME") + ":3306/"
                    + scd.canonicalEnvKey("database_name"));
        }
        
        if (scd.getProperty("username") != null) {
            ds(props, scd, "user-name", scd.canonicalEnvKey("username"));
        } else if (scd.getProperty("database-user") != null) {
            ds(props, scd, "user-name", scd.canonicalEnvKey("database-user"));
        }
        
        if (scd.getProperty("password") != null) {
            ds(props, scd, "password", scd.canonicalEnvKey("password"));
        } else if (scd.getProperty("database-password") != null) {
            ds(props, scd, "password", scd.canonicalEnvKey("database-password"));
        }
        return props;
    }
}
