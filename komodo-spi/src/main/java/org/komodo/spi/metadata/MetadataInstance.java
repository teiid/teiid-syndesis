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
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.MetadataVersion;
import org.komodo.spi.type.DataTypeService;

public interface MetadataInstance extends StringConstants {

    /**
     * The host of the metadata instance
     */
    String HOST = "localhost"; //$NON-NLS-1$

    /**
     * The default admin user for the metadata instance
     */
    String DEFAULT_ADMIN_USER = "admin"; //$NON-NLS-1$

    /**
     * The default admin password for the metadata instance
     */
    String DEFAULT_ADMIN_PASSWORD = "admin"; //$NON-NLS-1$

    /**
     * The default admin port for the metadata instance
     */
    int DEFAULT_ADMIN_PORT = 9990; //$NON-NLS-1$

    /**
     * The default jdbc user for the metadata instance
     */
    String DEFAULT_JDBC_USER = "user"; //$NON-NLS-1$

    /**
     * The default jdbc password for the metadata instance
     */
    String DEFAULT_JDBC_PASSWORD = "user"; //$NON-NLS-1$

    /**
     * The default jdbc port for the metadata instance
     */
    int DEFAULT_JDBC_PORT = 31000; //$NON-NLS-1$

    /**
     * The default protocol for the metadata instance
     */
    String DEFAULT_INSTANCE_PROTOCOL = "mms"; //$NON-NLS-1$

    /**
     * VDB name for the ping test
     */
    String PING_VDB_NAME = "ping"; //$NON-NLS-1$

    /**
     * VDB file name for the ping test
     */
    String PING_VDB = PING_VDB_NAME + VDB_DEPLOYMENT_SUFFIX;

    /**
     * Type of connectivity
     */
    enum ConnectivityType {
        /**
         * Admin connection of the metadata instance
         */
        ADMIN, 
        
        /**
         * JDBC connection of the metadata instance
         */
        JDBC;

        /**
         * @param type
         * @return the {@link ConnectivityType} for the given type
         */
        public static ConnectivityType findType(String type) {
            if (type == null)
                return null;

            for (ConnectivityType cType : ConnectivityType.values()) {
                if (type.equalsIgnoreCase(cType.name()))
                    return cType;
            }

            return null;
        }
    }

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
     * Ping the metadata instance to determine whether it is still connected
     * @param connectivityType
     *
     * @return {@link Outcome} describing state of ping
     * 
     * @throws KException 
     */
    Outcome ping(ConnectivityType connectivityType);

    /**
     * Query the vdb with given name
     *
     * @param vdbName the name of the vdb to query
     * @param query the SQL query
     * @param offset an offset of the results to return
     * @param limit a limit on the number of results to return
     * @return the set of results
     * @throws KException
     */
    QSResult query(String vdbName, String query, int offset, int limit) throws KException;

    /**
     * @return the collection of deployed vdbs
     * @throws KException 
     */
    Collection<TeiidVdb> getVdbs() throws KException;

    /**
     * @return the names of all the deployed vdbs
     * @throws KException 
     */
    Collection<String> getVdbNames() throws KException;

    /**
     * @param vdbDeploymentName
     * @return the deployed vdb
     * @throws KException 
     */
    TeiidVdb getVdb(String vdbDeploymentName) throws KException;

    /**
     * @param name 
     * @return whether metadata instance contains a vdb with the given name
     * @throws KException 
     */
    boolean hasVdb( String name ) throws KException;
    
    /**
     * @param vdbName
     *  
     * @return <code>true</code> if the vdb is active
     * @throws KException 
     */
    boolean isVdbActive(String vdbName) throws KException;
    
    /**
     * @param vdbName
     *  
     * @return <code>true</code> if the vdb is loading
     * @throws KException
     */
    boolean isVdbLoading(String vdbName) throws KException;
    
    /**
     * @param vdbName
     *  
     * @return <code>true</code> if the vdb failed
     * @throws KException
     */
    boolean hasVdbFailed(String vdbName) throws KException;
    
    /**
     * @param vdbName 
     * 
     * @return <code>true</code> if the vdb was removed
     * @throws KException
     */
    boolean wasVdbRemoved(String vdbName) throws KException;
    
    /**
     * @param vdbName
     * 
     * @return any validity errors from the vdb when it was deployed
     * @throws KException
     */
    List<String> retrieveVdbValidityErrors(String vdbName) throws KException;

    /**
     * @param vdbName
     * @param version
     * @param modelName
     * @return the schema from the given model in the vdb with the given name
     * @throws KException 
     */
    String getSchema(String vdbName, String version, String modelName) throws KException;

    /**
     * Deploy a dynamic vdb
     *
     * @param vdbDeploymentName
     * @param stream
     * @throws KException 
     */
    void deployDynamicVdb(String vdbDeploymentName, InputStream stream) throws KException;

    /**
     * Undeploy the dynamic vdb with the given name
     * @param name
     * @throws KException 
     */
    void undeployDynamicVdb(String name) throws KException;

    /**
     * @return the collection of translators
     * @throws KException 
     */
    Collection<TeiidTranslator> getTranslators() throws KException;

    /**
     * @param translatorName
     * @return the translator with the given name
     * @throws KException 
     */
    TeiidTranslator getTranslator(String translatorName) throws KException;

    /**
     * @return the collection of data source drivers resident on the server
     * @throws KException
     */
    Collection<ConnectionDriver> getDataSourceDrivers() throws KException;

    /**
     * Deploys a driver (jar or rar) to the related metadata instance
     * 
     * @param driverName the deployment name to use for the driver
     * @param driverFile the file to deploy
     * 
     * @throws KException if deployment fails
     */
    void deployDataSourceDriver(String driverName, File driverFile) throws KException;

    /**
     * Undeploy the named driver
     *
     * @param driver
     *
     * @throws KException if undeployment fails
     */
    void undeployDataSourceDriver(String driver) throws KException;

    /**
     * @return the collection of data sources
     * @throws KException 
     */
    Collection<TeiidDataSource> getDataSources() throws KException;

    /**
     * Get the specified DataSource, or create one if it does not exist.  When the datasource is create thru the admin API,
     * it is given a JNDI name which is the same as the sourceName.  For example if dsName 'mySource' is supplied, then the 
     * JNDI name is set as 'java:/mySource' (java context is added).  When the sources created by any other user are retrieved 
     * from the metadata instance, however, it is not guaranteed that the dsName and jndi name will match.
     * @param displayName the data source display name
     * @param dsName the data source name
     * @param typeName the translator type name
     * @param properties the list of metadata-related connection properties
     * @return true if data source is created. false if it already exists
     * @throws KException if data source creation fails
     */
     TeiidDataSource getOrCreateDataSource(String displayName,
                                                          String dsName,
                                                          String typeName,
                                                          Properties properties) throws KException;

    /**
     * @param sourceName
     * @return the data source with the given name
     * @throws KException 
     */
    TeiidDataSource getDataSource(String sourceName) throws KException;

    /**
     * @param name the name of the data source
     * @return true if data source exists with the provided name. else false.
     * @throws KException 
     */
     boolean dataSourceExists(String name) throws KException;

     /**
      * Removes the data source from the metadata instance (if exists)
      * 
      * @param dsName the data source name
      * @throws KException if failure in deleting data source on metadata instance
      */
      void deleteDataSource(String dsName) throws KException;

    /**
     * @param driverName
     * @return the template property definitions for the given driver
     * @throws KException 
     */
    Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String driverName) throws KException;

    /**
     * Get all DataSource template names
     * @return set of template names
     * @throws KException
     */
    Set<String> getDataSourceTemplateNames() throws KException;

    /**
     * Refresh this instance to clear any cached and get the latest metadata.
     * @throws KException 
     */
    void refresh() throws KException;
}
