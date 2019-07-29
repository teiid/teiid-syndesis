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

import java.util.HashMap;
import java.util.Map;

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
    public Map<String, String> getInternalTeiidDataSourceProperties(DefaultSyndesisDataSource source) {
        Map<String, String> props = new HashMap<>();
        
        props.put("jndi-name", source.getName());
        props.put("driver-name", getType()); // used as translator name
        props.put("display-name", source.getName());
        
        props.put("url", source.getProperty("url"));
        props.put("username", source.getProperty("user"));
        props.put("password", source.getProperty("password"));
        props.put("schema", source.getProperty("schema"));
        return props;
    }

    @Override
    public Map<String, String> getPublishedImageDataSourceProperties(DefaultSyndesisDataSource scd) {
    	Map<String, String> props = new HashMap<>();
        ds(props, scd, "jdbc-url", scd.getProperty("url"));
        ds(props, scd, "username", scd.getProperty("user"));
        ds(props, scd, "password", scd.getProperty("password"));

        if (scd.getProperty("schema") != null) {
        	ds(props, scd, "importer.schemaName", scd.getProperty("schema"));
        }

        // pool properties
        ds(props, scd, "maximumPoolSize", "5");
        ds(props, scd, "minimumIdle", "0");
        return props;
    }
}
