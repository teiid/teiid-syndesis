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
            "</dependency>\n";
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
    public Properties getInternalTeiidDataSourceProperties(DefaultSyndesisDataSource source) {
        Properties props = new Properties();
        
        props.setProperty("jndi-name", source.getName());
        props.setProperty("driver-name", getType()); // used as translator name
        props.setProperty("display-name", source.getName());
        
        props.setProperty("url", source.getProperty("url"));
        props.setProperty("username", source.getProperty("user"));
        props.setProperty("password", source.getProperty("password"));
        props.setProperty("schema", source.getProperty("schema"));
        props.setProperty("syndesis-connector-id", source.getId());
        return props;
    }

    @Override
    public Properties getPublishedImageDataSourceProperties(DefaultSyndesisDataSource scd, String jndiName) {
        Properties props = new Properties();
        ds(props, scd, "url", scd.canonicalEnvKey("url"));
        ds(props, scd, "username", scd.canonicalEnvKey("user"));
        ds(props, scd, "password", scd.canonicalEnvKey("password"));
        return props;
    }
}
