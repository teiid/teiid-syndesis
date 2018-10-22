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

public class FileDefinition extends DataSourceDefinition {

    @Override
    public String getType() {
        return "file";
    }

    @Override
    public String getPomDendencies() {
        return
            "<dependency>" +
            "  <groupId>org.wildfly.swarm</groupId>" +
            "  <artifactId>teiid-file</artifactId>" +
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
    public Properties getDataSourceProperties(DefaultSyndesisDataSource source) {
        Properties props = new Properties();
        props.setProperty("class-name", "org.teiid.resource.adapter.file.FileManagedConnectionFactory");
        props.setProperty("ParentDirectory", source.getProperty("directory"));
        return props;
    } 
    
    @Override
    public Properties getWFSDataSourceProperties(DefaultSyndesisDataSource scd, String jndiName) {
        Properties props = setupResourceAdapter(scd.getName(), "org.jboss.teiid.resource-adapter.file",
                "org.teiid.resource.adapter.file.FileManagedConnectionFactory", jndiName);
        ds(props, scd, "ParentDirectory", scd.canonicalEnvKey("directory"));
        return props;
    }     
}
