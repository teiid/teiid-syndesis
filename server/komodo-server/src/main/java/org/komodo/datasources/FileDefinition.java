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

public class FileDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
        return "file";
    }

    @Override
    public String getPomDendencies() {
        return
            "<dependency>" +
            "  <groupId>org.teiid</groupId>" +
            "  <artifactId>thorntail-file</artifactId>" +
            "</dependency>";
    }

    @Override
    public String getTranslatorName() {
        return "file";
    }
    
    @Override
    public boolean isResouceAdapter() {
        return true;
    }

    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("PARENT_DIRECTORY") != null)) {
            return true;
        }
        return false;
    }
    
    @Override
    public Properties getInternalTeiidDataSourceProperties(DefaultSyndesisDataSource source) {
        Properties props = new Properties();
        props.setProperty("class-name", "org.teiid.resource.adapter.file.FileManagedConnectionFactory");
        props.setProperty("ParentDirectory", source.getProperty("directory"));
        return props;
    } 
    
    @Override
    public Properties getPublishedImageDataSourceProperties(DefaultSyndesisDataSource scd) {
        Properties props = setupResourceAdapter(scd.getName(), "org.jboss.teiid.resource-adapter.file",
                "org.teiid.resource.adapter.file.FileManagedConnectionFactory", scd.getName());
        ds(props, scd, "ParentDirectory", scd.getProperty("directory"));
        return props;
    }     
}
