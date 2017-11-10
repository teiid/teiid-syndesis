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
package org.komodo.relational.commands.server;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.komodo.core.KEngine;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.StringUtils;

/**
 * Common methods used by server commands
 */
public class ServerUtils {

	private KEngine engine;
	
	public ServerUtils(KEngine engine) {
		this.engine = engine;
	}
    /**
     * @return the metadata instance instance
     */
    public MetadataInstance getMetadataInstance() throws Exception {
        return engine.getMetadataInstance();
    }

    /**
     * @return the data source type names
     */
    public Set<String> getDataSourceTypeNames() throws Exception {
        Set<String> serverTypes = getMetadataInstance().getDataSourceTemplateNames();
        return serverTypes;
    }

    /**
     * Determine if the metadata instance has a source type
     *
     * @param sourceType the source type
     * @return 'true' if the type exists on the server, 'false' if not.
     * @throws Exception the exception
     */
    public boolean hasDatasourceType(String sourceType) throws Exception {
        // Look for matching name
        Set<String> serverTypes = getDataSourceTypeNames();
        for(String serverType : serverTypes) {
            if(serverType.equals(sourceType)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Return the deployed datasource names from the metadata instance
     *
     * @return the collection of data source names
     * @throws Exception the exception
     */
    public List<String> getDatasourceNames() throws Exception {
        Collection< TeiidDataSource > sources = getMetadataInstance().getDataSources();
        if(sources.isEmpty()) return Collections.emptyList();

        List< String > existingSourceNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            existingSourceNames.add( source.getName() );
        }
        return existingSourceNames;
    }

    /**
     * Return the deployed datasource display names from the metadata instance
     *
     * @return the collection of data source display names
     * @throws Exception the exception
     */
    public List<String> getDatasourceDisplayNames() throws Exception {
        Collection< TeiidDataSource > sources = getMetadataInstance().getDataSources();
        if(sources.isEmpty()) return Collections.emptyList();
        
        List< String > existingSourceNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            existingSourceNames.add( source.getDisplayName() );
        }
        return existingSourceNames;
    }

    /**
     * Return the deployed datasource jndi names from the metadata instance
     *
     * @return the collection of data source jndi names
     * @throws Exception the exception
     */
    public List<String> getDatasourceJndiNames() throws Exception {
        Collection< TeiidDataSource > sources = getMetadataInstance().getDataSources();
        if(sources.isEmpty()) return Collections.emptyList();
        
        List< String > existingJndiNames = new ArrayList< String >();
        for ( TeiidDataSource source : sources ) {
            String jndiName = source.getPropertyValue(TeiidDataSource.DATASOURCE_JNDINAME);
            if(!StringUtils.isEmpty(jndiName)) {
                existingJndiNames.add( jndiName );
            }
        }
        return existingJndiNames;
    }

    /**
     * Return the deployed VDB names from the metadata instance
     *
     * @return the collection of vdb names
     * @throws Exception the exception
     */
    public List<String> getVdbNames() throws Exception {
        Collection< String > vdbNames = getMetadataInstance().getVdbNames();
        if(vdbNames.isEmpty()) return Collections.emptyList();
        
        return new ArrayList<>(vdbNames);
    }

    /**
     * Return the Translator names from the metadata instance
     *
     * @return the collection of translator names
     * @throws Exception the exception
     */
    public List<String> getTranslatorNames() throws Exception {
        Collection< TeiidTranslator > translators = getMetadataInstance().getTranslators();
        if(translators.isEmpty()) return Collections.emptyList();
        
        List< String > existingTranslatorNames = new ArrayList< String >();
        for ( TeiidTranslator translator : translators ) {
            existingTranslatorNames.add( translator.getName() );
        }
        return existingTranslatorNames;
    }

    public TeiidDataSource getDataSource(String sourceName) throws Exception {
        return getMetadataInstance().getDataSource(sourceName);
    }

    public boolean hasDataSource(String sourceName) throws Exception {
        if (sourceName == null)
            return false;

        Collection<TeiidDataSource> dataSources = getMetadataInstance().getDataSources();
        if (dataSources.isEmpty())
            return false;

        for (TeiidDataSource source : dataSources) {
            if (sourceName.equals(source.getName()))
                return true;
        }

        return false;
    }

    public void deleteDataSource(String sourceName) throws Exception {
        getMetadataInstance().deleteDataSource(sourceName);
    }

    public void getOrCreateDataSource(String sourceName, String jndiName, String sourceType, Properties sourceProps) throws Exception {
        getMetadataInstance().getOrCreateDataSource(sourceName, jndiName, sourceType, sourceProps);
    }

    public void deployDriver(String driverName, File driverFile) throws Exception {
        getMetadataInstance().deployDataSourceDriver(driverName, driverFile);
    }

    public void deployDynamicVdb(String vdbDeploymentName, InputStream stream) throws Exception {
        getMetadataInstance().deployDynamicVdb(vdbDeploymentName, stream);
    }

    public Collection<TeiidVdb> getVdbs() throws Exception {
        return getMetadataInstance().getVdbs();
    }

    public boolean hasVdb(String vdbName) throws Exception {
        return getMetadataInstance().getVdb(vdbName) != null;
    }

    public TeiidTranslator getTranslator(String translatorName) throws Exception {
        return getMetadataInstance().getTranslator(translatorName);
    }

    public TeiidVdb getVdb(String vdbName) throws Exception {
        return getMetadataInstance().getVdb(vdbName);
    }

    public void undeployDynamicVdb(String name) throws Exception {
        getMetadataInstance().undeployDynamicVdb(name);
    }

}
