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

public class MySQLDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
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
          "  <groupId>org.teiid</groupId>" +
          "   <artifactId>thorntail-jdbc</artifactId>" +
          "</dependency>";

    }

    @Override
    public String getTranslatorName() {
        return "mysql5";
    }

    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("url") != null)
                && (properties.get("url").startsWith("jdbc:mysql:"))) {
            return true;
        }
        return false;
    }

    @Override
    public Properties getDataSourceProperties(DefaultSyndesisDataSource source) {
        Properties props = new Properties();
        
        props.setProperty("connection-url", source.getProperty("url"));
        props.setProperty("user-name", source.getProperty("user"));        
        props.setProperty("password", source.getProperty("password"));
        return props;
    }

    @Override
    public Properties getWFSDataSourceProperties(DefaultSyndesisDataSource scd, String jndiName) {
        Properties props = new Properties();
        ds(props, scd, "driver-name", scd.getType());
        ds(props, scd, "jndi-name", jndiName);
        
        ds(props, scd, "connection-url", scd.canonicalEnvKey("url"));
        ds(props, scd, "user-name", scd.canonicalEnvKey("user"));
        ds(props, scd, "password", scd.canonicalEnvKey("password"));
        return props;
    }
}
