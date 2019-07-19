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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.komodo.metadata.DataTypeService;
import org.komodo.metadata.DataTypeService.DataTypeName;
import org.komodo.metadata.Messages;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.query.QSColumn;
import org.komodo.metadata.query.QSResult;
import org.komodo.metadata.query.QSRow;
import org.komodo.metadata.runtime.TeiidDataSource;
import org.komodo.metadata.runtime.TeiidVdb;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.TeiidAdminImpl;
import org.komodo.rest.TeiidServer;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.sql.LanguageObject;

@Component
public class DefaultMetadataInstance implements MetadataInstance {

    private static DataTypeService dataTypeService = new DataTypeService();

    @Autowired
    private TeiidServer server;

    private Admin admin;

    public DefaultMetadataInstance() {
        
    }
    
    @Override
    public Admin getAdmin() throws AdminException {
    	if (this.admin == null) {
	        this.admin = new TeiidAdminImpl(server.getAdmin(), server);
	    }
		return this.admin;
    }
    
	public Connection getConnection(String vdb, String version) throws SQLException {
		Properties props = new Properties();
		//TODO: when security working the user name needs to be passed in we need to work delegation model for security
		return server.getDriver().connect("jdbc:teiid:"+vdb+"."+version, props);
	}

    /**
     * Wraps error in a {@link KException} if necessary.
     *
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error (never <code>null</code>)
     */
    protected static KException handleError(Throwable e) {
        assert (e != null);
    
        if (e instanceof KException) {
            return (KException)e;
        }
    
        return new KException(e);
    }

    protected void checkStarted() throws KException {
        if (getCondition() != Condition.REACHABLE) {
        	String msg = Messages.getString(Messages.MetadataServer.serverCanNotBeReached);
	        throw new KException(msg);
        }
    }

    @Override
    public Condition getCondition() {
        try {
			return getAdmin() != null ? Condition.REACHABLE : Condition.NOT_REACHABLE;
		} catch (AdminException e) {
			return Condition.NOT_REACHABLE;
		}
    }

    @Override
    public DataTypeService getDataTypeService() {
        return dataTypeService;
    }

    @Override
    public QSResult query(String vdb, String query, int offset, int limit) throws KException {
        checkStarted();

        QSResult result = new QSResult();

        KLog.getLogger().debug("Commencing query execution: {0}", query);

        Connection connection = null;
        Statement statement = null;
        ResultSet rs = null;

        KLog.getLogger().debug("Initialising SQL connection for vdb {0}", vdb);

        //
        // Ensure any runtime exceptions are always caught and thrown as KExceptions
        //
        try {
            connection = getConnection(vdb, "1");

            if (connection == null)
                throw new KException(Messages.getString(Messages.MetadataServer.vdbConnectionFailure, vdb));

            statement = connection.createStatement();

            KLog.getLogger().debug("Executing SQL Statement for query {0} with offset of {1} and limit of {2}",
                                   query,
                                   offset,
                                   limit);
            rs = statement.executeQuery(query);

            ResultSetMetaData rsmd = rs.getMetaData();
            int columns = rsmd.getColumnCount();

            //
            // Populate the columns
            //
            for (int i = 1; i <= columns; ++i) {
                String columnName = rsmd.getColumnName(i);
                String columnLabel = rsmd.getColumnLabel(i);
                String colTypeName = rsmd.getColumnTypeName(i);
                DataTypeName typeName = dataTypeService.getDataTypeName(colTypeName);
                QSColumn column = new QSColumn(typeName, columnName, columnLabel);
                result.addColumn(column);
            }

            int rowNum = 0;
            while (rs.next()) {
                rowNum++;

                if (offset > NO_OFFSET && rowNum < offset) {
                    continue;
                }

                if (limit > NO_LIMIT && result.getRows().size() >= limit) {
                    break;
                }

                QSRow row = new QSRow();
                for (int i = 1; i <= columns; ++i) {
                    Object value = rs.getObject(i);
                    row.add(value);
                }

                result.addRow(row);
            }

            KLog.getLogger().debug("Query executed and returning {0} results", result.getRows().size());

            return result;
        } catch (Throwable t) {
            throw new KException(t);
        } finally {
            try {
                if (rs != null)
                    rs.close();

                if (statement != null)
                    statement.close();

                if (connection != null)
                    connection.close();
            } catch (SQLException e1) {
                // ignore
            }
        }
    }

    @Override
    public boolean dataSourceExists(String name) throws KException {
        checkStarted();
        try {
            return getAdmin().getDataSourceNames().contains(name);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public TeiidDataSource getDataSource(String name) throws KException {
        checkStarted();
        Properties dataSource;
        try {
            dataSource = getAdmin().getDataSource(name);
            if (dataSource == null)
                return null;

            return new TeiidDataSourceImpl(name, dataSource);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void deleteDataSource(String dsName) throws KException {
        checkStarted();
        try {
            getAdmin().deleteDataSource(dsName);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<TeiidDataSource> getDataSources() throws KException {
        checkStarted();
        try {
            Collection<String> dsNames = getAdmin().getDataSourceNames();
            if (dsNames.isEmpty())
                return Collections.emptyList();

            List<TeiidDataSource> dsSources = new ArrayList<>();
            for (String dsName : dsNames) {
                TeiidDataSource dataSource = getDataSource(dsName);
                dsSources.add(dataSource);
            }

            return dsSources;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    private boolean isDynamic(VDB vdb) {
        if (vdb == null)
            return false;

        if (! (vdb instanceof VDBMetaData))
            return false;

        return ((VDBMetaData) vdb).isXmlDeployment();
    }

    @Override
    public Collection<String> getVdbNames() throws KException {
        checkStarted();
        try {
            Collection<? extends VDB> vdbs = getAdmin().getVDBs();
            if (vdbs.isEmpty())
                return Collections.emptyList();

            List<String> teiidVdbNames = new ArrayList<String>();
            for (VDB vdb : vdbs) {
                teiidVdbNames.add(vdb.getName());
            }

            return teiidVdbNames;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public boolean hasVdb(String name) throws KException {
        checkStarted();
        return getVdb(name) != null;
    }

    @Override
    public boolean isVdbActive(String vdbName) throws KException {
        checkStarted();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).isActive();
    }

    @Override
    public boolean isVdbLoading(String vdbName) throws KException {
        checkStarted();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).isLoading();
    }

    @Override
    public boolean hasVdbFailed(String vdbName) throws KException {
        checkStarted();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).hasFailed();
    }

    @Override
    public boolean wasVdbRemoved(String vdbName) throws KException {
        checkStarted();
        if (!hasVdb(vdbName))
            return false;

        return getVdb(vdbName).wasRemoved();
    }

    @Override
    public List<String> retrieveVdbValidityErrors(String vdbName) throws KException {
        checkStarted();
        if (!hasVdb(vdbName))
            return Collections.emptyList();

        return getVdb(vdbName).getValidityErrors();
    }

    @Override
    public Collection<TeiidVdb> getVdbs() throws KException {
        checkStarted();
        try {
            Collection<? extends VDB> vdbs = getAdmin().getVDBs();
            if (vdbs.isEmpty())
                return Collections.emptyList();

            List<TeiidVdb> teiidVdbs = new ArrayList<>();
            for (VDB vdb : vdbs) {
                if (!isDynamic(vdb))
                    continue;

                teiidVdbs.add(new TeiidVdbImpl(vdb));
            }

            return teiidVdbs;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public TeiidVdb getVdb(String name) throws KException {
        checkStarted();
        try {
            VDB vdb = getAdmin().getVDB(name, "1");
            if (vdb == null)
                return null;

            if (!isDynamic(vdb))
                return null;

            return new TeiidVdbImpl(vdb);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void deployDynamicVdb(String vdbName, String deploymentName, InputStream inStream) throws KException {
        checkStarted();

        try {
            ArgCheck.isNotNull(deploymentName, "deploymentName"); //$NONNLS1$
            ArgCheck.isNotNull(inStream, "inStream"); //$NONNLS1$

            VDB vdb = getAdmin().getVDB(vdbName, "1.0");
            if (vdb != null) {
            	getAdmin().undeploy(deploymentName);
            }
            getAdmin().deploy(deploymentName, inStream);

            // Give a 0.5 sec pause for the VDB to finish loading metadata.
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                // ignore
            }

        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void undeployDynamicVdb(String vdbName) throws KException {
        checkStarted();
        try {
            TeiidVdb vdb = getVdb(vdbName);
            if (vdb != null) {
                getAdmin().undeploy(vdbName);
            }
            vdb = getVdb(vdbName);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public String getSchema(String vdbName, String vdbVersion, String modelName) throws KException {
        checkStarted();
        try {
            return getAdmin().getSchema(vdbName, vdbVersion, modelName, null, null);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    /**
     * Attempt to parse the given sql string and return the {@link LanguageObject} tree
     *
     * @param sql
     * @return tree of {@link LanguageObject}s
     * @throws KException
     */
    public static LanguageObject parse(String sql) throws KException {
        //
        // Note: this does not require the metadata instance to be started
        //
        try {
            return QueryParser.getQueryParser().parseDesignerCommand(sql);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

	@Override
	public DeployStatus deploy(UnitOfWork uow, Vdb vdb) {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        DeployStatus status = new DeployStatus();

        try {
            String vdbName = vdb.getName(uow);
            status.addProgressMessage("Starting deployment of vdb " + vdbName); //$NON-NLS-1$

            status.addProgressMessage("Attempting to deploy VDB " + vdbName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$

            // Get VDB content
            byte[] vdbXml = vdb.export(uow, null);
            if (vdbXml == null || vdbXml.length == 0) {
                status.addErrorMessage("VDB " + vdbName + " content is empty"); //$NON-NLS-1$ //$NON-NLS-2$
                return status;
            }

            String vdbToDeployName = vdb.getName(uow);
            String vdbDeploymentName = vdbToDeployName + VDB_DEPLOYMENT_SUFFIX;
            deployDynamicVdb(vdbName, vdbDeploymentName, new ByteArrayInputStream(vdbXml));

            status.addProgressMessage("VDB deployed " + vdbName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$
        } catch (Exception ex) {
            status.addErrorMessage(ex);
        }
        
        return status;
	}
}
