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

import org.komodo.metadata.TeiidDataSource;

public class SalesforceDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
        return "salesforce";
    }

    @Override
    public String getPomDendencies() {
        return
            "<dependency>" +
            "  <groupId>org.teiid</groupId>" +
            "  <artifactId>spring-data-salesforce</artifactId>" +
            "</dependency>\n";
    }

    @Override
    public String getTranslatorName() {
        return "salesforce";
    }

    @Override
    public boolean isTypeOf(Map<String, String> properties, String type) {
        if (type.equals("salesforce")) {
            return true;
        }
        return false;
    }

    @Override
    public TeiidDataSource createDatasource(String deploymentName, DefaultSyndesisDataSource scd) {
        throw new UnsupportedOperationException();
    }

    /**
     * Given the connection properties from the Syndesis secrets generate Spring Boot
     * configuration file to configure the data source
     * @return properties properties required to create a connection in target environment
     */
    @Override
    public Map<String, String> getPublishedImageDataSourceProperties(DefaultSyndesisDataSource scd) {
        Map<String, String> props = new HashMap<>();
        ds(props, scd, "client-id", scd.getProperty("clientId"));
        ds(props, scd, "client-secret", scd.getProperty("clientSecret"));
        ds(props, scd, "refresh-token", scd.getProperty("refreshToken"));
        return props;
    }

    @Override
    protected void ds(Map<String, String> props, DefaultSyndesisDataSource scd, String key, String value) {
        props.put("spring.teiid.data.salesforce." + scd.getKomodoName() + "." + key, value);
    }
}
