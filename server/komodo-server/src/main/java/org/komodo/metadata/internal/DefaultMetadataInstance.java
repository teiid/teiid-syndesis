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
import java.io.IOException;
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
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import javax.sql.DataSource;
import javax.xml.stream.XMLStreamException;

import org.komodo.KException;
import org.komodo.datasources.ExternalSource;
import org.komodo.metadata.Messages;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.TeiidDataSource;
import org.komodo.metadata.TeiidVdb;
import org.komodo.metadata.query.QSColumn;
import org.komodo.metadata.query.QSResult;
import org.komodo.metadata.query.QSRow;
import org.komodo.utils.KLog;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.stereotype.Component;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.AdminProcessingException;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.adminapi.impl.SourceMappingMetadata;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.adminapi.impl.VDBMetadataParser;
import org.teiid.api.exception.query.QueryMetadataException;
import org.teiid.core.TeiidComponentException;
import org.teiid.core.util.AccessibleByteArrayOutputStream;
import org.teiid.dqp.internal.datamgr.ConnectorManagerRepository.ConnectorManagerException;
import org.teiid.metadata.AbstractMetadataRecord;
import org.teiid.metadata.MetadataFactory;
import org.teiid.query.metadata.BasicQueryMetadataWrapper;
import org.teiid.query.metadata.CompositeMetadataStore;
import org.teiid.query.metadata.MetadataValidator;
import org.teiid.query.metadata.SystemMetadata;
import org.teiid.query.metadata.TransformationMetadata;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.sql.LanguageObject;
import org.teiid.query.validator.ValidatorReport;
import org.teiid.translator.TranslatorException;

import com.zaxxer.hikari.HikariDataSource;

@Component
public class DefaultMetadataInstance implements MetadataInstance {

    public static final String DEFAULT_VDB_VERSION = "1";

    @Autowired
    private TeiidServer server;

    private Admin admin;
    private Map<String, Object> datasources = new ConcurrentHashMap<>();
    private Map<String, TeiidDataSource> dsProperties = new ConcurrentHashMap<>();
    private ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1);

    public DefaultMetadataInstance() {

    }

    public DefaultMetadataInstance(TeiidServer server) {
        this.server = server;
    }

    public Admin getAdmin() throws AdminException {
        //no need to synchronize, as delegate holds no state
        if (admin == null) {
            admin = server.getAdmin();
        }
        return admin;
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
    public QSResult query(String vdb, String query, int offset, int limit) throws KException {
        checkStarted();

        QSResult result = new QSResult();

        KLog.getLogger().debug("Commencing query execution: %s", query);

        Connection connection = null;
        Statement statement = null;
        ResultSet rs = null;

        KLog.getLogger().debug("Initialising SQL connection for vdb %s", vdb);

        //
        // Ensure any runtime exceptions are always caught and thrown as KExceptions
        //
        try {
            connection = getConnection(vdb, DEFAULT_VDB_VERSION);

            if (connection == null)
                throw new KException(Messages.getString(Messages.MetadataServer.vdbConnectionFailure, vdb));

            statement = connection.createStatement();

            KLog.getLogger().debug("Executing SQL Statement for query %s with offset of %d and limit of %d",
                                   query,
                                   offset,
                                   limit);

            if (offset != NO_OFFSET || limit != NO_LIMIT) {
                //if we want more effective pagination, then
                //we need to enable result set caching and parameterize the limit/offset
                query = "SELECT * FROM (" + query + ") x LIMIT " + Math.max(0, offset) + ", " + (limit < 0?Integer.MAX_VALUE:limit);
            }


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
                QSColumn column = new QSColumn(colTypeName, columnName, columnLabel);
                result.addColumn(column);
            }

            while (rs.next()) {
                QSRow row = new QSRow();
                for (int i = 1; i <= columns; ++i) {
                    Object value = rs.getObject(i);
                    row.add(value);
                }

                result.addRow(row);
            }

            KLog.getLogger().debug("Query executed and returning %d results", result.getRows().size());

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
    public TeiidDataSource getDataSource(String name) throws KException {
        checkStarted();
        return this.dsProperties.get(name);
    }

    @Override
    public synchronized void deleteDataSource(String dsName) throws KException {
        checkStarted();
        try {
            Object ds = this.datasources.get(dsName);
            if (ds != null) {
                this.server.removeConnectionFactoryProvider(dsName);
                this.datasources.remove(dsName);
            }

            // close the underlying datasource and any connections
            if (ds instanceof HikariDataSource) {
                ((HikariDataSource)ds).close();
            }
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<TeiidDataSource> getDataSources() throws KException {
        checkStarted();
        return this.dsProperties.values();
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
                teiidVdbs.add(new TeiidVdbImpl(vdb));
            }

            return teiidVdbs;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public TeiidVdbImpl getVdb(String name) throws KException {
        checkStarted();
        try {
            VDB vdb = getAdmin().getVDB(name, DEFAULT_VDB_VERSION);
            if (vdb == null)
                return null;

            return new TeiidVdbImpl(vdb);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void deploy(VDBMetaData vdb) throws KException {
        String vdbName = vdb.getName();

        try {
            // Deploy the VDB
            AccessibleByteArrayOutputStream baos = toBytes(vdb);

            byte[] bytes = baos.getBuffer();
            InputStream inStream = new ByteArrayInputStream(bytes, 0, baos.getCount());

            checkStarted();

            String deploymentName = vdbName + VDB_DEPLOYMENT_SUFFIX;

            Admin admin = getAdmin();

            VDB existing = admin.getVDB(vdbName, DEFAULT_VDB_VERSION);
            if (existing != null) {
                admin.undeploy(existing.getName());
            }

            for (ModelMetaData model : vdb.getModelMetaDatas().values()) {
                for (SourceMappingMetadata smm : model.getSourceMappings()) {
                    addTranslator(smm.getTranslatorName());
                    if (smm.getConnectionJndiName() != null && datasources.get(smm.getConnectionJndiName()) != null) {
                        server.addConnectionFactory(smm.getName(), datasources.get(smm.getConnectionJndiName()));
                    }
                }
            }
            admin.deploy(deploymentName, inStream);

        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void undeployDynamicVdb(String vdbName) throws KException {
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
    public String getSchema(String vdbName, String modelName) throws KException {
        try {
            return getAdmin().getSchema(vdbName, DEFAULT_VDB_VERSION, modelName, null, null);
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

    public static AccessibleByteArrayOutputStream toBytes(VDBMetaData vdb) throws KException {
        AccessibleByteArrayOutputStream baos = new AccessibleByteArrayOutputStream();
        try {
            VDBMetadataParser.marshell(vdb, baos);
        } catch (XMLStreamException | IOException e) {
            throw new KException(e);
        }

        return baos;
    }

    @Override
    public void createDataSource(String deploymentName, String templateName, Map<String, String> properties)
            throws AdminException {
        switch(templateName) {
        case "postgresql":
        case "mysql":
        case "h2":
        case "teiid":
            if (datasources.get(deploymentName) == null) {
                DataSource ds = DataSourceBuilder.create().url(properties.get("url"))
                        .username(properties.get("username") != null ? properties.get("username")
                                : properties.get("user"))
                        .password(properties.get("password")).build();

                if (ds instanceof HikariDataSource) {
                    ((HikariDataSource)ds).setMaximumPoolSize(10);
                    ((HikariDataSource)ds).setMinimumIdle(0);
                    ((HikariDataSource)ds).setIdleTimeout(60000);
                    ((HikariDataSource)ds).setScheduledExecutor(executor);
                }

                this.datasources.put(deploymentName, ds);
                properties.put("type", templateName);

                this.dsProperties.put(deploymentName, new TeiidDataSourceImpl(deploymentName, properties));
            }
            break;
            default:
            throw new AdminProcessingException(
                    "Unsupported data type " + templateName + " Failed to create data source " + deploymentName);
        }
    }

    @Override
    public Set<String> getDataSourceTemplateNames() throws AdminException {
        HashSet<String> templates = new HashSet<>();
        templates.add("postgresql");
        templates.add("mysql");
        templates.add("h2");
        templates.add("teiid");
        return templates;
    }

    @Override
    public Collection<String> getDataSourceNames() throws AdminException {
        return datasources.keySet();
    }

    void addTranslator(String translatorname) {
        try {
            if (server.getExecutionFactory(translatorname) == null) {
                server.addTranslator(ExternalSource.translatorClass(translatorname, "org.komodo.rest"));
            }
        } catch (ConnectorManagerException | TranslatorException e) {
            throw new IllegalStateException("Failed to load translator " + translatorname, e);
        }
    }

    @Override
    public ValidationResult validate(String vdbName, String ddl) throws KException {
        QueryParser parser = QueryParser.getQueryParser();

        ModelMetaData m = new ModelMetaData();
        m.setName(SERVICE_VDB_VIEW_MODEL); //$NON-NLS-1$

        MetadataFactory mf = new MetadataFactory(vdbName, DEFAULT_VDB_VERSION, SystemMetadata.getInstance().getRuntimeTypeMap(),m);
        parser.parseDDL(mf, ddl);

        TeiidVdbImpl preview = getVdb(vdbName);
        if (preview == null || !preview.isActive()) {
            throw new KException("Preview VDB is not yet available");
        }


        VDBMetaData vdb = preview.getVDBMetaData();
        TransformationMetadata qmi = vdb.getAttachment(TransformationMetadata.class);

        //create an metadata facade so we can find stuff that was parsed
        CompositeMetadataStore compositeMetadataStore = new CompositeMetadataStore(mf.asMetadataStore());
        BasicQueryMetadataWrapper wrapper = new BasicQueryMetadataWrapper(qmi) {
            @Override
            public Object getGroupID(String groupName) throws TeiidComponentException, QueryMetadataException {
                try {
                    return super.getGroupID(groupName);
                } catch (QueryMetadataException e) {
                    return compositeMetadataStore.findGroup(groupName);
                }
            }
        };

        ValidatorReport report = new ValidatorReport();
        MetadataValidator validator = new MetadataValidator();
        for (AbstractMetadataRecord record : mf.getSchema().getResolvingOrder()) {
            validator.validate(vdb, m, record, report, wrapper, mf, parser);
        }

        return new ValidationResult(report, mf.getSchema());
    }

}
