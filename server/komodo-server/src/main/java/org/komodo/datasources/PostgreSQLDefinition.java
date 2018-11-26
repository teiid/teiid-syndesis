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

public class PostgreSQLDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
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
        if ((properties != null) && (properties.get("url") != null)
                && properties.get("url").startsWith("jdbc:postgresql:")) {
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
