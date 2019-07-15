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
package org.komodo.metadata.internal;

import java.util.Properties;

import org.komodo.metadata.runtime.TeiidDataSource;
import org.teiid.core.util.ArgCheck;

public class TeiidDataSourceImpl implements Comparable<TeiidDataSourceImpl>, TeiidDataSource {

    private final String name;
    private final Properties properties = new Properties();

    public TeiidDataSourceImpl(String name, Properties properties) {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(properties, "properties"); //$NON-NLS-1$

        this.name = name;
        for (String propName : properties.stringPropertyNames()) {
            this.properties.setProperty(propName, properties.getProperty(propName));
        }
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(TeiidDataSourceImpl dataSource) {
        return getName().compareTo(dataSource.getName());
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (obj.getClass() != getClass())
            return false;

        TeiidDataSource other = (TeiidDataSource)obj;

        if (getName().equals(other.getName()))
            return true;

        return false;
    }

    @Override
    public String getName() {
        return this.name;
    }

    @Override
    public String getDisplayName() {
        return getPropertyValue(DATASOURCE_DISPLAYNAME);
    }

    @Override
    public Properties getProperties() {
        return this.properties;
    }

    @Override
    public String getPropertyValue(String name) {
        return this.properties.getProperty(name);
    }

    /**
     * Returns the data source type name
     * 
     * @return the type
     */
    @Override
    public String getType() {
        return getPropertyValue(DATASOURCE_DRIVERNAME);
    }

    /**
     * Returns the data source jndi name
     * 
     * @return the jndi name
     */
    @Override
    public String getJndiName() {
        return getPropertyValue(DATASOURCE_JNDINAME);
    }

    /**
     * Returns the data source connection url
     * 
     * @return the connection url
     */
    @Override
    public String getConnectionUrl() {
        return getPropertyValue(DATASOURCE_CONNECTION_URL);
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        int result = 0;
        final int prime = 31;
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
        return result;
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Data Source:\t" + getName()); //$NON-NLS-1$
        if (!getType().equalsIgnoreCase("<unknown>")) { //$NON-NLS-1$
            sb.append("\nType: \t\t" + getType()); //$NON-NLS-1$
        }

        return sb.toString();
    }
}
