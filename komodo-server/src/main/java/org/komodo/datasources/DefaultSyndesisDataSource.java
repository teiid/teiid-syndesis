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

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.SyndesisDataSource;

public class DefaultSyndesisDataSource implements SyndesisDataSource {
    private String name;
    private boolean bound;
    private String translator;
    private Map<String, String> properties;
    private DataSourceDefinition definition;

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getType() {
        return definition.getType();
    }

    @Override
    public boolean isBound() {
        return bound;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setBound(boolean bound) {
        this.bound = bound;
    }

    @Override
    public String getTranslatorName() {
        return translator;
    }

    public void setTranslatorName(String translator) {
        this.translator = translator;
    }

    public DataSourceDefinition getDefinition() {
        return definition;
    }

    public void setDefinition(DataSourceDefinition definition) {
        this.definition = definition;
    }
    
    public Map<String, String> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }
    
    public String getProperty(String key) {
        return this.properties.get(key);
    }

    public String canonicalKey(String key) {
        return this.name.replace(StringConstants.HYPHEN, StringConstants.UNDERSCORE).toUpperCase()
                + StringConstants.UNDERSCORE
                + key.replace(StringConstants.HYPHEN, StringConstants.UNDERSCORE).toUpperCase();
    }    
    
    protected String canonicalEnvKey(String key) {
        return "$(" + canonicalKey(key) + ")";
    }

    public Properties convertToDataSourceProperties() {
        return this.definition.getDataSourceProperties(this);
    }
}