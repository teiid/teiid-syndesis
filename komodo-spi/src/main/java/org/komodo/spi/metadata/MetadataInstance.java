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
package org.komodo.spi.metadata;

import java.io.File;
import java.io.InputStream;
import java.util.Collection;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.MetadataVersion;
import org.komodo.spi.type.DataTypeService;

public interface MetadataInstance {

    /**
     * Value representing no limit to results returned by a query
     */
    int NO_LIMIT = -1;

    /**
     * Value representing no offset to a set of results returned by a query
     */
    int NO_OFFSET = 0;

    /**
     * The server state.
     */
    public enum Condition {

        /**
         * Server has been successfully reached.
         */
        REACHABLE,

        /**
         * Server is not reachable.
         */
        NOT_REACHABLE,

        /**
         * There was an error trying to establish server.
         */
        ERROR

    }

    /**
     * @return the condition of the server
     */
    Condition getCondition();

    /**
     * Notify this metadata instance of a client event
     *
     * @param event
     */
    void notify(MetadataClientEvent event);

    /**
     * @param observer
     *        the observer to be added
     */
    void addObserver(MetadataObserver observer);

    /**
     * @param observer
     *        the observer to be removed
     */
    void removeObserver(MetadataObserver observer);

    /**
     * @return the version number of this server
     */
    MetadataVersion getVersion();

    /**
     * @return the data type service
     */
    DataTypeService getDataTypeService();

    /**
     * Query the vdb with given name
     *
     * @param vdbName the name of the vdb to query
     * @param query the SQL query
     * @param offset an offset of the results to return
     * @param limit a limit on the number of results to return
     * @return the set of results
     * @throws Exception
     */
    QSResult query(String vdbName, String query, int offset, int limit) throws Exception;

    /**
     * @return the collection of deployed vdbs
     * @throws Exception 
     */
    Collection<TeiidVdb> getVdbs() throws Exception;

    /**
     * @return the names of all the deployed vdbs
     */
    Collection<String> getVdbNames();

    /**
     * @param vdbDeploymentName
     * @return the deployed vdb
     * @throws Exception 
     */
    TeiidVdb getVdb(String vdbDeploymentName) throws Exception;

    /**
     * @param vdbName
     * @return whether a vdb with the given name exists
     * @throws Exception 
     */
    boolean hasVdb(String vdbName) throws Exception;

    /**
     * @param vdbName
     * @param version
     * @param modelName
     * @return the schema from the given model in the vdb with the given name
     */
    String getSchema(String vdbName, String version, String modelName);

    /**
     * Deploy a dynamic vdb
     *
     * @param vdbDeploymentName
     * @param stream
     * @throws Exception 
     */
    void deployDynamicVdb(String vdbDeploymentName, InputStream stream) throws Exception;

    /**
     * Undeploy the dynamic vdb with the given name
     * @param name
     */
    void undeployDynamicVdb(String name);

    /**
     * @return the collection of translators
     */
    Collection<TeiidTranslator> getTranslators();

    /**
     * @param translatorName
     * @return the translator with the given name
     */
    TeiidTranslator getTranslator(String translatorName);

    /**
     * @return the collection of data sources
     */
    Collection<TeiidDataSource> getDataSources();

    /**
     * @return the names of the data source types
     */
    Set<String> getDataSourceTypeNames();

    /**
     * @param sourceName
     * @return the data source with the given name
     */
    TeiidDataSource getDataSource(String sourceName);

    /**
     * Either creates a data source or gets it if it already exists
     *
     * @param connectionName
     * @param jndiName
     * @param sourceType
     * @param properties
     * @return the data source
     */
    TeiidDataSource getOrCreateDataSource(String connectionName, String jndiName, String sourceType, Properties properties);

    /**
     * @param sourceName
     */
    void deleteDataSource(String sourceName);

    /**
     * @return the collection of drivers
     */
    Collection<ConnectionDriver> getDrivers();

    /**
     * Deploy the given driver
     *
     * @param driverName
     * @param driverFile
     */
    void deployDriver(String driverName, File driverFile);

    /**
     * @param driverName
     * @return the template property definitions for the given driver
     */
    Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String driverName);
}
