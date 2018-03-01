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

import org.komodo.servicecatalog.DataSourceDefinition;
import org.komodo.servicecatalog.DecodedSecret;
import org.komodo.spi.runtime.ServiceCatalogDataSource;

public class DefaultServiceCatalogDataSource implements ServiceCatalogDataSource {
    private String name;
    private boolean bound;
    private String translator;
    private DecodedSecret parameters;
    private DecodedSecret credentials;
    private DataSourceDefinition definition;

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getType() {
        return definition.getName();
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

    public DecodedSecret getParameters() {
        return parameters;
    }

    public void setParameters(DecodedSecret parameters) {
        this.parameters = parameters;
    }

    public DecodedSecret getCredentials() {
        return credentials;
    }

    public void setCredentials(DecodedSecret credentials) {
        this.credentials = credentials;
    }

    public DataSourceDefinition getDefinition() {
        return definition;
    }

    public void setDefinition(DataSourceDefinition definition) {
        this.definition = definition;
    }

    public String getProperty(String key) {
        String value = null;
        if (parameters != null) {
            value = parameters.getData().get(key);
        }
        if (value == null && credentials != null) {
            value = credentials.getData().get(key);
        }
        return value;
    }

    protected String canonicalKey(String key) {
        if (getParameters() != null) {
            if (getParameters().getData().containsKey(key)) {
                return getParameters().canonicalKey(key);
            }
        }
        return getCredentials().canonicalKey(key);
    }

    protected String canonicalEnvKey(String key) {
        return "$(" + canonicalKey(key) + ")";
    }
}