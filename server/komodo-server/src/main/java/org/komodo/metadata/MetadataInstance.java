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
package org.komodo.metadata;

import java.util.Collection;
import java.util.List;

import org.komodo.metadata.query.QSResult;
import org.komodo.metadata.runtime.TeiidDataSource;
import org.komodo.metadata.runtime.TeiidVdb;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.impl.VDBMetaData;

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
     * @param modelName
     * @return the schema from the given model in the vdb with the given name
     * @throws KException 
     */
    String getSchema(String vdbName, String modelName) throws KException;

    /**
     * Undeploy the dynamic vdb with the given name
     * @param name
     * @throws KException 
     */
    void undeployDynamicVdb(String name) throws KException;

    /**
     * @return the collection of data sources
     * @throws KException 
     */
    Collection<TeiidDataSource> getDataSources() throws KException;

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

	Admin getAdmin() throws AdminException;

    /**
     * @param vdb
     * @return the deployment status of the vdb
     */
	DeployStatus deploy(VDBMetaData vdb) throws KException;
}
