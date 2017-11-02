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
package org.komodo.metadata;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import org.komodo.metadata.internal.DataTypeServiceImpl;
import org.komodo.metadata.internal.MetaArtifactFactory;
import org.komodo.spi.KEvent;
import org.komodo.spi.KException;
import org.komodo.spi.KObserver;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.metadata.MetadataClientEvent;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.metadata.MetadataObserver;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.spi.query.QSColumn;
import org.komodo.spi.query.QSResult;
import org.komodo.spi.query.QSRow;
import org.komodo.spi.runtime.ConnectionDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultMetadataVersion;
import org.komodo.spi.runtime.version.MetadataVersion;
import org.komodo.spi.type.DataTypeService;
import org.komodo.spi.type.DataTypeService.DataTypeName;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.PropertyDefinition;
import org.teiid.adminapi.Translator;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.adminapi.jboss.AdminFactory;
import org.teiid.core.util.ApplicationInfo;
import org.teiid.jdbc.TeiidDriver;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.sql.LanguageObject;

public class DefaultMetadataInstance implements MetadataInstance {

    private class JndiManager implements StringConstants {

        private static final String PREFIX = JAVA + COLON + FORWARD_SLASH;

        public String getName(String name) {
            if (! name.startsWith(PREFIX))
                return name;

            name = name.replace(PREFIX, EMPTY_STRING);
            if (name.startsWith(FORWARD_SLASH))
                name = name.substring(1);

            return name;
        }
    }

    private static final Object TEIID_INSTANCE_LOCK = new Object();

    private static final String TEST_VDB = EMPTY_STRING +
        "<vdb name=\"ping\" version=\"1\">" +
            "<model visible=\"true\" name=\"Foo\" type=\"PHYSICAL\" path=\"/dummy/Foo\">" +
                "<source name=\"s\" translator-name=\"loopback\"/>" +
                "<metadata type=\"DDL\">" +
                    "<![CDATA[CREATE FOREIGN TABLE G1 (e1 string, e2 integer);]]>" +
                "</metadata>" +
            "</model>" +
        "</vdb>";

    private static DefaultMetadataInstance instance;

    private final Set<MetadataObserver> observers = new HashSet<>();

    private DataTypeServiceImpl dataTypeService;

    private final DefaultMetadataVersion teiidVersion;

    private final MetaArtifactFactory factory = new MetaArtifactFactory();

    private final JndiManager jndiMgr = new JndiManager();

    private Admin admin;

    public static DefaultMetadataInstance getInstance() {
        if (instance == null)
            instance = new DefaultMetadataInstance();

        return instance;
    }

    private DefaultMetadataInstance() {
        ApplicationInfo appInfo = ApplicationInfo.getInstance();
        this.teiidVersion = new DefaultMetadataVersion(appInfo.getReleaseNumber());
    }

    /**
     * Wraps error in a {@link KException} if necessary.
     *
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error (never <code>null</code>)
     */
    private static KException handleError(Throwable e) {
        assert (e != null);
    
        if (e instanceof KException) {
            return (KException)e;
        }
    
        return new KException(e);
    }

    private String getAdminUser() {
        return DEFAULT_ADMIN_USER;
    }

    private String getAdminPwd() {
        return DEFAULT_ADMIN_PASSWORD;
    }

    private int getAdminPort() {
        return DEFAULT_ADMIN_PORT;
    }

    private String getJdbcUser() {
        return DEFAULT_JDBC_USER;
    }

    private String getJdbcPwd() {
        return DEFAULT_JDBC_PASSWORD;
    }

    private int getJdbcPort() {
        return DEFAULT_JDBC_PORT;
    }

    private void connect() throws KException {
        synchronized(TEIID_INSTANCE_LOCK) {
            if (this.admin == null) {
                try {
                    /*
                     * By the time this has been called the teiid version should be correct
                     * for the given host and the host should be up, otherwise admin will
                     * end up back as null anyway.
                     */
                    char[] passwordArray = null;
                    if (getAdminPwd() != null) {
                        passwordArray = getAdminPwd().toCharArray();
                    }

                    this.admin = AdminFactory.getInstance().createAdmin(HOST,
                                                                        getAdminPort(),
                                                                        getAdminUser(),
                                                                        passwordArray);

                    if (admin == null) {
                        String msg = Messages.getString(Messages.MetadataServer.cannotConnectToInstance, getAdminUser());
                        throw new KException(msg);
                    }

                } catch (Exception ex) {
                    throw handleError(ex);
                }
            }
        }
    }

    private void disconnect() {
        if (this.admin != null) {
            this.admin.close();
            this.admin = null;
        }
    }

    private void notifyObservers(KEvent<?> event) {
        final Set<MetadataObserver> copy = new HashSet<>(this.observers);
    
        for (final MetadataObserver observer : copy) {
            try {
                // Ensure all observers are informed even if one throws an exception
                observer.eventOccurred(event);
            } catch (final Exception ex) {
                observer.errorOccurred(ex);
            }
        }
    }

    private void errorObservers(Throwable ex) {
        ArgCheck.isNotNull(ex);
    
        final Set<KObserver> copy = new HashSet<>(this.observers);
    
        for (final KObserver observer : copy) {
            observer.errorOccurred(ex);
        }
    }

    private void startServer() throws KException {
        if (getCondition() == Condition.REACHABLE)
            return;

        try {
            connect();
            KEvent<MetadataInstance> event = new KEvent<MetadataInstance>(this, KEvent.Type.METADATA_SERVER_STARTED);
            notifyObservers(event);
        } catch (Throwable ex) {
            errorObservers(ex);
            throw handleError(ex);
        }
    }

    private void checkStarted() throws KException {
        if (getCondition() != Condition.REACHABLE) {
            startServer();

            Outcome outcome = ping(ConnectivityType.ADMIN);
            if (! outcome.isOK()) {
                throw new KException(outcome.getException());
            }
        }
    }

    private void stopServer() throws KException {
        if (getCondition() == Condition.NOT_REACHABLE)
            return;

        try {
            disconnect();
        } catch (Throwable ex) {
            errorObservers(ex);
            throw handleError(ex);
        } finally {
            KEvent<MetadataInstance> event = new KEvent<MetadataInstance>(this, KEvent.Type.METADATA_SERVER_STOPPED);
            notifyObservers(event);
        }
    }

    private Outcome pingAdmin() {
        if (admin == null) {
            try {
                startServer();
            } catch (Exception ex) {
                return OutcomeFactory.getInstance().createError(ex.getLocalizedMessage(), ex);
            }
        }

        try {
            admin.getSessions();
        } catch (Exception ex) {
            return OutcomeFactory.getInstance().createError(ex.getLocalizedMessage(), ex);
        }

        return OutcomeFactory.getInstance().createOK();
    }

    private Outcome pingJdbc() {
        String protocol = DEFAULT_INSTANCE_PROTOCOL;

        Connection teiidJdbcConnection = null;
        StringBuffer url = new StringBuffer();
        url.append("jdbc").append(COLON).append("teiid")
             .append(COLON).append("ping").append(AMPERSAND)
             .append(protocol).append(HOST).append(COLON)
             .append(getJdbcPort());

        try {

            admin.deploy(PING_VDB, new ByteArrayInputStream(TEST_VDB.getBytes()));

            try {
                StringBuffer urlAndCred = new StringBuffer(url);
                urlAndCred.append(SEMI_COLON)
                                     .append("user").append(EQUALS).append(getJdbcUser())
                                     .append(SEMI_COLON)
                                     .append("password").append(EQUALS).append(getJdbcPwd())
                                     .append(SEMI_COLON);

                TeiidDriver teiidDriver = TeiidDriver.getInstance();
                teiidJdbcConnection = teiidDriver.connect(urlAndCred.toString(), null);
                //pass
            } catch (SQLException ex) {
                String msg = Messages.getString(Messages.MetadataServer.instanceDeployUndeployProblemPingingTeiidJdbc, url);
                return OutcomeFactory.getInstance().createError(msg, ex);
            } finally {
                admin.undeploy(PING_VDB);

                if (teiidJdbcConnection != null) {
                    teiidJdbcConnection.close();
                }
            }
        } catch (Exception ex) {
            String msg = Messages.getString(Messages.MetadataServer.instanceDeployUndeployProblemPingingTeiidJdbc, url);
            return OutcomeFactory.getInstance().createError(msg, ex);
        }

        return OutcomeFactory.getInstance().createOK();
    }

    @Override
    public Condition getCondition() {
        return admin != null ? Condition.REACHABLE : Condition.NOT_REACHABLE;
    }

    @Override
    public void refresh() throws KException {
        try {
            stopServer();

            Thread.sleep(2000);

            // Refresh is implied in the getting of the admin object since it will
            // automatically load and refresh.
            startServer();
    
        } catch (Exception e) {
            String msg = Messages.getString(Messages.MetadataServer.refreshError, e.getLocalizedMessage()); //$NONNLS1$
            throw new KException(msg);
        }
    }

    @Override
    public void notify( MetadataClientEvent event ) {
        this.addObserver(event.getSource());

        try {
            if (event.getType() == MetadataClientEvent.EventType.STARTED) {
                startServer();
            } else if (event.getType() == MetadataClientEvent.EventType.SHUTTING_DOWN) {
                stopServer();
            }
        } catch (KException ex) {
            //
            // Error should be captured by observer so log but do not throw
            //
            KLog.getLogger().error(Messages.getString(Messages.MetadataServer.startStopFailure, ex.getLocalizedMessage()));
        }
    }

    @Override
    public void addObserver(MetadataObserver observer) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.add(observer);
    }

    @Override
    public void removeObserver(MetadataObserver observer) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.remove(observer);
    }

    @Override
    public MetadataVersion getVersion() {
        return teiidVersion;
    }

    @Override
    public DataTypeService getDataTypeService() {
        if (dataTypeService == null)
            dataTypeService = new DataTypeServiceImpl(getVersion());

        return dataTypeService;
    }

    @Override
    public Outcome ping(ConnectivityType connectivityType) {
        boolean testCausesConnect = false;
        String msg = Messages.getString(Messages.MetadataServer.cannotConnectToInstance, getAdminUser());

        if (! Condition.REACHABLE.equals(getCondition())) {
            try {
                startServer();
                testCausesConnect = true;
            } catch (Exception ex) {
                return OutcomeFactory.getInstance().createError(msg, ex);
            }
        }

        Outcome outcome = null;
        if (! Condition.REACHABLE.equals(getCondition()))
            return OutcomeFactory.getInstance().createError(msg);

        try {
            switch (connectivityType) {
                case JDBC:
                    outcome = pingJdbc();
                    break;
                default:
                    outcome = pingAdmin();
            }
        } catch (Exception ex) {
            return OutcomeFactory.getInstance().createError(ex.getLocalizedMessage(), ex);
        }

        // Only disconnect if this test ping caused
        // the connect
        if (testCausesConnect) {
            disconnect();
        }

        return outcome;
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
        // Don't add in ds.setServerName(HOST) since if it remains null then it
        // actiually connects as Embedded rather than using a socket
        //
        org.teiid.jdbc.TeiidDataSource ds = new org.teiid.jdbc.TeiidDataSource();
        ds.setDatabaseName(vdb);
        ds.setUser(getJdbcUser());
        ds.setPassword(getJdbcPwd());
        ds.setPortNumber(getJdbcPort());
        ds.setSecure(true);

        //
        // Ensure any runtime exceptions are always caught and thrown as KExceptions
        //
        try {
            connection = ds.getConnection();

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
    public Collection<ConnectionDriver> getDataSourceDrivers() throws KException {
        checkStarted();

        List<ConnectionDriver> drivers = new ArrayList<ConnectionDriver>();
        Set<String> templateNames = getDataSourceTemplateNames();
        for (String templateName : templateNames) {
            if (templateName == null)
                continue; // Seems to be a null driver installed in wildfly 10.0.1 by default
            
            drivers.add(new ConnectionDriver(templateName));
        }
        return drivers;
    }

    @Override
    public void deployDataSourceDriver(String driverName, File driverFile) throws KException {
        checkStarted();
        ArgCheck.isNotNull(driverName, "driverName"); //$NON-NLS-1$
        
        if (!driverFile.exists())
            throw new KException(Messages.getString(Messages.MetadataServer.jarDeploymentJarNotFound, driverFile.getPath()));

        if (!driverFile.canRead())
            throw new KException(Messages.getString(Messages.MetadataServer.jarDeploymentJarNotReadable, driverFile.getPath()));

        InputStream iStream = null;
        try {
            iStream = new FileInputStream(driverFile);
        } catch (FileNotFoundException ex) {
            throw handleError(ex);
        }

        try {
            admin.deploy(driverName, iStream);

            // Give a 0.5 sec pause for the driver to finish loading.
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                // ignore
            }

        } catch (Exception ex) {
            // Jar deployment failed
            throw handleError(ex);
        }
    }

    @Override
    public void undeployDataSourceDriver(String driverName) throws KException {
        checkStarted();
        ArgCheck.isNotNull(driverName, "driverName"); //$NON-NLS-1$

        try {
            admin.undeploy(driverName);
        } catch (Exception ex) {
            // Jar deployment failed
            throw handleError(ex);
        }
    }

    @Override
    public boolean dataSourceExists(String name) throws KException {
        checkStarted();
        try {
            return admin.getDataSourceNames().contains(name);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public TeiidDataSource getOrCreateDataSource(String displayName, String dsName, String typeName, Properties properties)
        throws KException {
        checkStarted();

        ArgCheck.isNotEmpty(displayName, "displayName"); //$NONNLS1$
        ArgCheck.isNotEmpty(dsName, "dsName"); //$NONNLS1$
        ArgCheck.isNotEmpty(typeName, "typeName"); //$NONNLS1$
        ArgCheck.isNotEmpty(properties, "properties"); //$NONNLS1$

        for (Entry<Object, Object> entry : properties.entrySet()) {
            Object value = entry.getValue();
            String errorMsg = "No value for the connection property '" + entry.getKey() + "'"; //$NONNLS1$ //$NONNLS2$
            ArgCheck.isNotNull(value, errorMsg);
            ArgCheck.isNotEmpty(value.toString(), errorMsg);
        }

        checkStarted();

        //
        // Check for jndi name prefix and drop it
        //
        dsName = jndiMgr.getName(dsName);

        // Check if exists, return false
        if (dataSourceExists(dsName)) {
            TeiidDataSource tds = getDataSource(dsName);
            if (tds != null) {
                return tds;
            }
        }

        // For JDBC types, find the matching installed driver.  This is done currently by matching
        // the profile driver classname to the installed driver classname
        String connProfileDriverClass = properties.getProperty("driverclass"); //$NONNLS1$

        // Verify the "typeName" exists.
        if (!getDataSourceTemplateNames().contains(typeName)) {
            if ("connectorjdbc".equals(typeName)) { //$NONNLS1$
                throw new KException(Messages.getString(Messages.MetadataServer.jdbcSourceForClassNameNotFound,
                                                       connProfileDriverClass));
            } else {
                throw new KException(Messages.getString(Messages.MetadataServer.dataSourceTypeDoesNotExist, typeName));
            }
        }

        properties.setProperty(TeiidDataSource.DATASOURCE_DISPLAYNAME, displayName);
        try {
            admin.createDataSource(dsName, typeName, properties);
        } catch (Exception ex) {
            throw handleError(ex);
        }

        // Check that local name list contains new dsName
        TeiidDataSource tds = getDataSource(dsName);
        if (tds != null) {
            return tds;
        }

        // We shouldn't get here if data source was created
        throw new KException(Messages.getString(Messages.MetadataServer.errorCreatingDataSource, dsName, typeName));
    }

    @Override
    public TeiidDataSource getDataSource(String name) throws KException {
        checkStarted();
        Properties dataSource;
        try {
            dataSource = admin.getDataSource(name);
            if (dataSource == null)
                return null;

            return factory.createDataSource(name, dataSource);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void deleteDataSource(String dsName) throws KException {
        checkStarted();
        try {
            admin.deleteDataSource(dsName);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<TeiidDataSource> getDataSources() throws KException {
        checkStarted();
        try {
            Collection<String> dsNames = admin.getDataSourceNames();
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

    @Override
    public TeiidTranslator getTranslator(String name) throws KException {
        checkStarted();
        try {
            return factory.createTranslator(admin.getTranslator(name));
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<TeiidTranslator> getTranslators() throws KException {
        checkStarted();
        try {
            Collection<? extends Translator> translators = admin.getTranslators();
            if (translators.isEmpty())
                return Collections.emptyList();

            List<TeiidTranslator> teiidTranslators = new ArrayList<>();
            for (Translator translator : translators) {
                teiidTranslators.add(factory.createTranslator(translator));
            }

            return teiidTranslators;
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
            Collection<? extends VDB> vdbs = admin.getVDBs();
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
            Collection<? extends VDB> vdbs = admin.getVDBs();
            if (vdbs.isEmpty())
                return Collections.emptyList();

            List<TeiidVdb> teiidVdbs = new ArrayList<>();
            for (VDB vdb : vdbs) {
                if (!isDynamic(vdb))
                    continue;

                teiidVdbs.add(factory.createVdb(vdb));
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
            VDB vdb = admin.getVDB(name, "1");
            if (vdb == null)
                return null;

            if (!isDynamic(vdb))
                return null;

            return factory.createVdb(vdb);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void deployDynamicVdb(String deploymentName, InputStream inStream) throws KException {
        checkStarted();

        try {
            ArgCheck.isNotNull(deploymentName, "deploymentName"); //$NONNLS1$
            ArgCheck.isNotNull(inStream, "inStream"); //$NONNLS1$

            admin.deploy(deploymentName, inStream);

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

    /**
     * Append the suffix for dynamic VDB to the vdb name if not already appended.
     * 
     * @param vdbName
     * @return
     */
    protected String appendDynamicVdbSuffix(String vdbName) {
        if (vdbName.endsWith(TeiidVdb.DYNAMIC_VDB_SUFFIX))
            return vdbName;

        return vdbName + TeiidVdb.DYNAMIC_VDB_SUFFIX;
    }

    @Override
    public void undeployDynamicVdb(String vdbName) throws KException {
        checkStarted();
        try {
            TeiidVdb vdb = getVdb(vdbName);
            if (vdb != null) {
                admin.undeploy(appendDynamicVdbSuffix(vdbName));
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
            return admin.getSchema(vdbName, vdbVersion, modelName, null, null);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Set<String> getDataSourceTemplateNames() throws KException {
        checkStarted();
        try {
            Set<String> templateNames = admin.getDataSourceTemplateNames();

            //
            // Workaround for removing vdb-builder.war
            // H2 driver which appear to return a null name
            //
            Iterator<String> iter = templateNames.iterator();
            while (iter.hasNext()) {
                String name = iter.next();
                if (name == null)
                    iter.remove();
            }

            return templateNames;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String templateName) throws KException {
        checkStarted();
        try {
            Collection<? extends PropertyDefinition> propDefs = this.admin.getTemplatePropertyDefinitions(templateName);
            if (propDefs.isEmpty())
                return Collections.emptyList();

            List<TeiidPropertyDefinition> teiidPropDefs = new ArrayList<>();
            for (PropertyDefinition propDef : propDefs) {
                teiidPropDefs.add(factory.createPropertyDefinition(propDef));
            }

            return teiidPropDefs;
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
}
