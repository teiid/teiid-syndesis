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

public class SalesforceDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
        return "salesforce";
    }

    @Override
    public String getPomDendencies() {
        return
            "<dependency>" +
            "  <groupId>org.wildfly.swarm</groupId>" +
            "  <artifactId>teiid-salesforce-41</artifactId>" +
            "</dependency>";
    }

    @Override
    public String getTranslatorName() {
        return "salesforce-41";
    }

    @Override
    public boolean isResouceAdapter() {
        return true;
    }
    
    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("SALESFORCE_URL") != null)) {
            return true;
        }
        return false;
    }
    
    @Override
    public Properties getDataSourceProperties(DefaultSyndesisDataSource source) {
        Properties props = new Properties();
        props.setProperty("class-name", "org.teiid.resource.adapter.salesforce.SalesForceManagedConnectionFactory");
        props.setProperty("URL", source.getProperty("url"));
        props.setProperty("username", source.getProperty("username"));
        props.setProperty("password", source.getProperty("password"));
        return props;
    }    

    @Override
    public Properties getWFSDataSourceProperties(DefaultSyndesisDataSource scd, String jndiName) {
        Properties props = setupResourceAdapter(scd.getName(), "org.jboss.teiid.resource-adapter.salesforce-41",
                "org.teiid.resource.adapter.salesforce.SalesForceManagedConnectionFactory", jndiName);
        ds(props, scd, "URL", scd.canonicalEnvKey("url"));
        ds(props, scd, "username", scd.canonicalEnvKey("username"));
        ds(props, scd, "password", scd.canonicalEnvKey("password"));
        return props;
    }
}
