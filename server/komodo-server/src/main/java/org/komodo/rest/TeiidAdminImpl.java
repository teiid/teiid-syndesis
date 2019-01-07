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
package org.komodo.rest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

import javax.sql.DataSource;
import javax.xml.stream.XMLStreamException;

import org.komodo.spi.runtime.TeiidDataSource;
import org.springframework.boot.autoconfigure.jdbc.DataSourceBuilder;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.AdminProcessingException;
import org.teiid.adminapi.CacheStatistics;
import org.teiid.adminapi.EngineStatistics;
import org.teiid.adminapi.PropertyDefinition;
import org.teiid.adminapi.Request;
import org.teiid.adminapi.Session;
import org.teiid.adminapi.Transaction;
import org.teiid.adminapi.Translator;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.VDB.ConnectionType;
import org.teiid.adminapi.WorkerPoolStatistics;
import org.teiid.adminapi.impl.ModelMetaData;
import org.teiid.adminapi.impl.PropertyDefinitionMetadata;
import org.teiid.adminapi.impl.SourceMappingMetadata;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.adminapi.impl.VDBMetadataParser;
import org.teiid.dqp.internal.datamgr.ConnectorManagerRepository.ConnectorManagerException;
import org.teiid.spring.autoconfigure.ExternalSource;
import org.teiid.spring.autoconfigure.TeiidServer;
import org.teiid.translator.TranslatorException;

public class TeiidAdminImpl implements Admin {
    private Admin delegate;
    private TeiidServer server;
    private HashMap<String, DataSource> datasources = new HashMap<>();
    private HashMap<String, Properties> dsProperties = new HashMap<>();

    public TeiidAdminImpl(Admin delegate, TeiidServer server) {
        this.delegate = delegate;
        this.server = server;
    }

    @Override
    public Collection<? extends PropertyDefinition> getTemplatePropertyDefinitions(String templateName)
            throws AdminException {
        switch(templateName) {
        case "postgresql":
        case "mysql":
        case "h2":
        case "teiid":
            ArrayList<PropertyDefinitionMetadata> props = new ArrayList<>();
            props.add(buildNode("url", "string", "Connection URL", true, false, "Connection URL"));
            props.add(buildNode("user", "string", "User Name", true, false, "User Name"));
            props.add(buildNode("password", "string", "Password", true, true, "Password"));
            props.add(buildNode("schema", "string", "Schema", true, true, "Schema"));
            return props;

            default:
            return Collections.emptyList();
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
    public void markDataSourceAvailable(String jndiName) throws AdminException {
    }

    @Override
    public void createDataSource(String deploymentName, String templateName, Properties properties) throws AdminException {
        switch(templateName) {
        case "postgresql":
        case "mysql":
        case "h2":
        case "teiid":
        	if (datasources.get(deploymentName) == null) {
	            DataSource ds = DataSourceBuilder.create()
	                .url(properties.getProperty("url"))
	                .username(properties.getProperty("username") != null ? properties.getProperty("username") :properties.getProperty("user") )
	                .password(properties.getProperty("password")).build();
	            this.datasources.put(deploymentName, ds);
	            properties.setProperty("type", templateName);
	            this.dsProperties.put(deploymentName, properties);
        	}
            break;
            default:
            throw new AdminProcessingException(
                    "Unsupported data type " + templateName + " Failed to create data source " + deploymentName);
        }
    }

    @Override
    public void deleteDataSource(String dsName) throws AdminException {
        DataSource ds = this.datasources.get(dsName);
        if (ds != null) {
            this.server.removeConnectionFactoryProvider(dsName);
            this.datasources.remove(dsName);
        }
    }

    @Override
    public Properties getDataSource(String dsName) throws AdminException {
        return this.dsProperties.get(dsName);
    }

    @Override
    public void deploy(String deployName, InputStream arg1) throws AdminException {
        if (!deployName.endsWith("-vdb.xml")) {
             //TODO: log error ?
            return;
        }
        try {
            VDBMetaData vdb =  VDBMetadataParser.unmarshell(arg1);
            for (ModelMetaData model : vdb.getModelMetaDatas().values()) {
                for (SourceMappingMetadata smm : model.getSourceMappings()) {
                    addTranslator(smm.getTranslatorName());
                    if (smm.getConnectionJndiName() != null && this.datasources.get(smm.getConnectionJndiName()) != null) {
                        server.addConnectionFactory(smm.getName(), this.datasources.get(smm.getConnectionJndiName()));
                    }
                }
            }
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            VDBMetadataParser.marshell(vdb, baos);
            delegate.deploy(deployName, new ByteArrayInputStream(baos.toByteArray()));
        } catch (XMLStreamException | IOException e) {
            throw new AdminProcessingException("Failed to load the VDB defined", e);
        }
    }

    void addTranslator(String translatorname) {
        try {
            if (server.getExecutionFactory(translatorname) == null) {
                server.addTranslator(ExternalSource.translatorClass(translatorname));
            }
        } catch (ConnectorManagerException | TranslatorException e) {
            throw new IllegalStateException("Failed to load translator " + translatorname, e);
        }
    }

    private PropertyDefinitionMetadata buildNode(String name, String type, String displyName, boolean required,
            boolean masked, String description) {
        PropertyDefinitionMetadata pdm = new PropertyDefinitionMetadata();
        pdm.setAdvanced(false);
        pdm.setAllowedValues(Arrays.asList());
        pdm.setDefaultValue(null);
        pdm.setDescription(description);
        pdm.setDisplayName(displyName);
        pdm.setMasked(masked);
        pdm.setName(name);
        pdm.setPropertyTypeClassName(type);
        pdm.setModifiable(true);
        pdm.setRequired(required);
        pdm.setCategory("");
        return pdm;
    }
    @Override
    public void addDataRoleMapping(String arg0, int arg1, String arg2, String arg3) throws AdminException {
        delegate.addDataRoleMapping(arg0, arg1, arg2, arg3);
    }

    @Override
    public void addDataRoleMapping(String arg0, String arg1, String arg2, String arg3) throws AdminException {
        delegate.addDataRoleMapping(arg0, arg1, arg2, arg3);
    }

    @Override
    public void addSource(String arg0, int arg1, String arg2, String arg3, String arg4, String arg5)
            throws AdminException {
        delegate.addSource(arg0, arg1, arg2, arg3, arg4, arg5);
    }

    @Override
    public void addSource(String arg0, String arg1, String arg2, String arg3, String arg4, String arg5)
            throws AdminException {
        delegate.addSource(arg0, arg1, arg2, arg3, arg4, arg5);
    }

    @Override
    public void cancelRequest(String arg0, long arg1) throws AdminException {
        delegate.cancelRequest(arg0, arg1);
    }

    @Override
    public void changeVDBConnectionType(String arg0, int arg1, ConnectionType arg2) throws AdminException {
        delegate.changeVDBConnectionType(arg0, arg1, arg2);
    }

    @Override
    public void changeVDBConnectionType(String arg0, String arg1, ConnectionType arg2) throws AdminException {
        delegate.changeVDBConnectionType(arg0, arg1, arg2);
    }

    @Override
    public void clearCache(String arg0, String arg1, int arg2) throws AdminException {
        delegate.clearCache(arg0, arg1, arg2);
    }

    @Override
    public void clearCache(String arg0, String arg1, String arg2) throws AdminException {
        delegate.clearCache(arg0, arg1, arg2);
    }

    @Override
    public void clearCache(String arg0) throws AdminException {
        delegate.clearCache(arg0);
    }

    @Override
    public void close() {
        delegate.close();
    }

    @Override
    public void deploy(String arg0, InputStream arg1, boolean arg2) throws AdminException {
        delegate.deploy(arg0, arg1, arg2);
    }

    @Override
    public Collection<? extends CacheStatistics> getCacheStats(String arg0) throws AdminException {
        return delegate.getCacheStats(arg0);
    }

    @Override
    public Collection<String> getCacheTypes() throws AdminException {
        return delegate.getCacheTypes();
    }

    @Override
    public Collection<String> getDataSourceNames() throws AdminException {
        return datasources.keySet();
    }

    @Override
    public List<String> getDeployments() throws AdminException {
        return delegate.getDeployments();
    }

    @Override
    public Collection<? extends EngineStatistics> getEngineStats() throws AdminException {
        return delegate.getEngineStats();
    }

    @Override
    public String getQueryPlan(String arg0, long arg1) throws AdminException {
        return delegate.getQueryPlan(arg0, arg1);
    }

    @Override
    public Collection<? extends Request> getRequests() throws AdminException {
        return delegate.getRequests();
    }

    @Override
    public Collection<? extends Request> getRequestsForSession(String arg0) throws AdminException {
        return delegate.getRequestsForSession(arg0);
    }

    @Override
    public String getSchema(String arg0, int arg1, String arg2, EnumSet<SchemaObjectType> arg3, String arg4)
            throws AdminException {
        return delegate.getSchema(arg0, arg1, arg2, arg3, arg4);
    }

    @Override
    public String getSchema(String arg0, String arg1, String arg2, EnumSet<SchemaObjectType> arg3, String arg4)
            throws AdminException {
        return delegate.getSchema(arg0, arg1, arg2, arg3, arg4);
    }

    @Override
    public Collection<? extends Session> getSessions() throws AdminException {
        return delegate.getSessions();
    }

    @Override
    public Collection<? extends Transaction> getTransactions() throws AdminException {
        return delegate.getTransactions();
    }

    @Override
    public Translator getTranslator(String arg0) throws AdminException {
        return delegate.getTranslator(arg0);
    }

    @Override
    public Collection<? extends PropertyDefinition> getTranslatorPropertyDefinitions(String arg0,
            TranlatorPropertyType arg1) throws AdminException {
        return delegate.getTranslatorPropertyDefinitions(arg0, arg1);
    }

    @Override
    public Collection<? extends PropertyDefinition> getTranslatorPropertyDefinitions(String arg0)
            throws AdminException {
        return delegate.getTranslatorPropertyDefinitions(arg0);
    }

    @Override
    public Collection<? extends Translator> getTranslators() throws AdminException {
        return delegate.getTranslators();
    }

    @Override
    public VDB getVDB(String arg0, int arg1) throws AdminException {
        return delegate.getVDB(arg0, arg1);
    }

    @Override
    public VDB getVDB(String arg0, String arg1) throws AdminException {
        return delegate.getVDB(arg0, arg1);
    }

    @Override
    public Collection<? extends VDB> getVDBs() throws AdminException {
        return delegate.getVDBs();
    }

    @Override
    public Collection<? extends WorkerPoolStatistics> getWorkerPoolStats() throws AdminException {
        return delegate.getWorkerPoolStats();
    }

    @Override
    public void removeDataRoleMapping(String arg0, int arg1, String arg2, String arg3) throws AdminException {
        delegate.removeDataRoleMapping(arg0, arg1, arg2, arg3);
    }

    @Override
    public void removeDataRoleMapping(String arg0, String arg1, String arg2, String arg3) throws AdminException {
        delegate.removeDataRoleMapping(arg0, arg1, arg2, arg3);
    }

    @Override
    public void removeSource(String arg0, int arg1, String arg2, String arg3) throws AdminException {
        delegate.removeSource(arg0, arg1, arg2, arg3);
    }

    @Override
    public void removeSource(String arg0, String arg1, String arg2, String arg3) throws AdminException {
        delegate.removeSource(arg0, arg1, arg2, arg3);
    }

    @Override
    public void restart() {
        delegate.restart();
    }

    @Override
    public void restartVDB(String arg0, int arg1, String... arg2) throws AdminException {
        delegate.restartVDB(arg0, arg1, arg2);
    }

    @Override
    public void restartVDB(String arg0, String arg1, String... arg2) throws AdminException {
        delegate.restartVDB(arg0, arg1, arg2);
    }

    @Override
    public void setAnyAuthenticatedForDataRole(String arg0, int arg1, String arg2, boolean arg3) throws AdminException {
        delegate.setAnyAuthenticatedForDataRole(arg0, arg1, arg2, arg3);
    }

    @Override
    public void setAnyAuthenticatedForDataRole(String arg0, String arg1, String arg2, boolean arg3)
            throws AdminException {
        delegate.setAnyAuthenticatedForDataRole(arg0, arg1, arg2, arg3);
    }

    @Override
    public void setProfileName(String arg0) {
        delegate.setProfileName(arg0);
    }

    @Override
    public void terminateSession(String arg0) throws AdminException {
        delegate.terminateSession(arg0);
    }

    @Override
    public void terminateTransaction(String arg0) throws AdminException {
        delegate.terminateTransaction(arg0);
    }

    @Override
    public void undeploy(String arg0) throws AdminException {
        delegate.undeploy(arg0);
    }

    @Override
    public void updateSource(String arg0, int arg1, String arg2, String arg3, String arg4) throws AdminException {
        delegate.updateSource(arg0, arg1, arg2, arg3, arg4);
    }

    @Override
    public void updateSource(String arg0, String arg1, String arg2, String arg3, String arg4) throws AdminException {
        delegate.updateSource(arg0, arg1, arg2, arg3, arg4);
    }
}
