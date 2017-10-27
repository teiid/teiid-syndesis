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

import java.io.File;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import javax.jcr.Node;

import org.komodo.metadata.internal.DataTypeServiceImpl;
import org.komodo.metadata.internal.MetaArtifactFactory;
import org.komodo.metadata.internal.NodeGenerator;
import org.komodo.spi.KEvent;
import org.komodo.spi.KObserver;
import org.komodo.spi.metadata.MetadataClientEvent;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.metadata.MetadataObserver;
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
import org.teiid.adminapi.VDB;
import org.teiid.core.util.ApplicationInfo;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.sql.lang.Command;
import org.teiid.runtime.EmbeddedConfiguration;
import org.teiid.runtime.EmbeddedServer;
import org.teiid.translator.loopback.LoopbackExecutionFactory;

public class DefaultMetadataInstance implements MetadataInstance {

    private static DefaultMetadataInstance instance;

    private EmbeddedConfiguration serverConfig;

    private EmbeddedServer server;

    private final Set<MetadataObserver> observers = new HashSet<>();

    private Condition condition = Condition.NOT_REACHABLE;

    private DataTypeServiceImpl dataTypeService;

    private final DefaultMetadataVersion teiidVersion;

    private final MetaArtifactFactory factory = new MetaArtifactFactory();

    public static DefaultMetadataInstance getInstance() {
        if (instance == null)
            instance = new DefaultMetadataInstance();

        return instance;
    }

    private DefaultMetadataInstance() {
        ApplicationInfo appInfo = ApplicationInfo.getInstance();
        this.teiidVersion = new DefaultMetadataVersion(appInfo.getReleaseNumber());
    }

    @Override
    public Condition getCondition() {
        return condition;
    }

    private void configureServer() {
        if (serverConfig != null)
            return;

        serverConfig = new EmbeddedConfiguration();
        serverConfig.setUseDisk(false);
    }

    private void addTranslators() throws Exception {
        LoopbackExecutionFactory lef = new LoopbackExecutionFactory();
        lef.setSupportsDirectQueryProcedure(true);
        lef.start();
        server.addTranslator("loopback", lef);
    }

    private void startServer() {
        if (this.condition == Condition.REACHABLE)
            return;

        configureServer();

        try {
            server = new EmbeddedServer();
            server.start(serverConfig);

            addTranslators();
        } catch (Throwable ex) {
            errorObservers(ex);
            return;
        }

        this.condition = Condition.REACHABLE;

        KEvent<MetadataInstance> event = new KEvent<MetadataInstance>(this, KEvent.Type.METADATA_SERVER_STARTED);
        notifyObservers(event);
    }

    private void checkStarted() {
        if (server == null)
            startServer();
    }

    private void stopServer() {
        if (this.condition == Condition.NOT_REACHABLE)
            return;

        try {
            server.stop();
        } catch (Throwable ex) {
            errorObservers(ex);
        } finally {
            this.condition = Condition.NOT_REACHABLE;

            KEvent<MetadataInstance> event = new KEvent<MetadataInstance>(this, KEvent.Type.METADATA_SERVER_STOPPED);
            notifyObservers(event);
        }
    }

    @Override
    public void notify( MetadataClientEvent event ) {
        if (event.getType() == MetadataClientEvent.EventType.STARTED) {
            // Start the metadata instance if not already started
            startServer();
        } else if (event.getType() == MetadataClientEvent.EventType.SHUTTING_DOWN) {
            stopServer();
        }
    }

    protected void notifyObservers(KEvent<?> event) {
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

    protected void errorObservers(Throwable ex) {
        ArgCheck.isNotNull(ex);

        final Set<KObserver> copy = new HashSet<>(this.observers);

        for (final KObserver observer : copy) {
            observer.errorOccurred(ex);
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
    public QSResult query(String vdb, String query, int offset, int limit) throws Exception {
        QSResult result = new QSResult();

        KLog.getLogger().debug("Commencing query execution: {0}", query);

        Connection connection = null;
        Statement statement = null;
        ResultSet rs = null;

        try {
            KLog.getLogger().debug("Initialising SQL connection for vdb {0}", vdb);

            Driver driver = server.getDriver();
            connection = driver.connect("jdbc:teiid:" + vdb, null);
            if (connection == null)
                throw new Exception("Failed to make a connection to '" + vdb + "'");

            statement = connection.createStatement();

            KLog.getLogger().debug("Executing SQL Statement for query {0} with offset of {1} and limit of {2}",
                                                       query, offset, limit);
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
    public void convertToJcr(String sql, Object parent) throws Exception {
        checkStarted();

        if (! (parent instanceof Node))
            throw new Exception(Messages.getString(Messages.MetadataServer.NotAJcrNode));

        if (sql == null)
            return;

        Command command = QueryParser.getQueryParser().parseDesignerCommand(sql);

        NodeGenerator generator = new NodeGenerator((Node) parent, getDataTypeService(), getVersion());
        generator.visitObject(command);
        if (generator.errorOccurred())
            throw generator.getError();
    }

    @Override
    public Collection<TeiidVdb> getVdbs() throws Exception {
        
        Collection<? extends VDB> vdbs = server.getAdmin().getVDBs();
        if (vdbs.isEmpty())
            return Collections.emptyList();

        List<TeiidVdb> teiidVdbs = new ArrayList<>();
        for (VDB vdb : vdbs) {
            teiidVdbs.add(factory.createVdb(vdb));
        }

        return teiidVdbs;
    }

    @Override
    public Collection<String> getVdbNames() {
        throw new UnsupportedOperationException();
    }

    @Override
    public TeiidVdb getVdb(String vdbName) throws Exception {
        VDB vdb;

        vdb = server.getAdmin().getVDB(vdbName, "1");

        if (vdb == null)
            return null;

        return factory.createVdb(vdb);
    }

    @Override
    public boolean hasVdb(String vdbName) throws Exception {
        return getVdb(vdbName) != null;
    }

    @Override
    public String getSchema(String vdbName, String version, String modelName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deployDynamicVdb(String vdbDeploymentName, InputStream stream) throws Exception {
        server.deployVDB(stream);
    }

    @Override
    public void undeployDynamicVdb(String vdbName) {
        server.undeployVDB(vdbName);
    }

    @Override
    public Collection<TeiidTranslator> getTranslators() {
        throw new UnsupportedOperationException();
    }

    @Override
    public TeiidTranslator getTranslator(String translatorName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<TeiidDataSource> getDataSources() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<String> getDataSourceTypeNames() {
        throw new UnsupportedOperationException();
    }

    @Override
    public TeiidDataSource getDataSource(String sourceName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public TeiidDataSource
    getOrCreateDataSource(String connectionName, String jndiName, String sourceType, Properties properties) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deleteDataSource(String sourceName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<ConnectionDriver> getDrivers() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deployDriver(String driverName, File driverFile) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<TeiidPropertyDefinition> getTemplatePropertyDefns(String driverName) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void parse(String sql) throws Exception {
        QueryParser.getQueryParser().parseDesignerCommand(sql);
    }
}
