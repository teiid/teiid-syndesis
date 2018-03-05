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
package org.komodo.rest.relational;

import java.net.URI;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.response.RestBuildStatus;
import org.komodo.rest.relational.response.RestServiceCatalogDataSource;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbCondition;
import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.rest.relational.response.RestVdbMask;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.relational.response.RestVdbModelSource;
import org.komodo.rest.relational.response.RestVdbModelTable;
import org.komodo.rest.relational.response.RestVdbModelTableColumn;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.rest.relational.response.metadata.RestMetadataConnection;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplate;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplateEntry;
import org.komodo.rest.relational.response.metadata.RestMetadataVdb;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbTranslator;
import org.komodo.servicecatalog.BuildStatus;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.ServiceCatalogDataSource;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

/**
 *
 */
public class RestEntityFactory implements V1Constants {

    private static final KLog LOGGER = KLog.getLogger();

    private void checkTransaction(UnitOfWork transaction) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
    }

    /**
     * @param kObject the object
     * @param baseUri the base uri
     * @param transaction the transaction
     * @param properties extra properties
     * @return the rest object for the given kObject
     * @throws KException if error occurs
     */
    @SuppressWarnings( "unchecked" )
    public <T extends RestBasicEntity> T create(KomodoObject kObject, URI baseUri, UnitOfWork transaction, KomodoProperties properties)
        throws KException {
        checkTransaction(transaction);

        WorkspaceManager wsMgr = WorkspaceManager.getInstance(kObject.getRepository(), transaction);
        KomodoType kType = kObject.getTypeIdentifier(transaction);

        switch (kType) {
            case VDB:
                Vdb vdb = wsMgr.resolve(transaction, kObject, Vdb.class);
                Boolean exportXml = properties.getProperty(VDB_EXPORT_XML_PROPERTY, Boolean.FALSE);
                return (T)new RestVdb(baseUri, vdb, exportXml, transaction);
            case VDB_CONDITION:
                Condition condition = wsMgr.resolve(transaction, kObject, Condition.class);
                return (T)new RestVdbCondition(baseUri, condition, transaction);
            case VDB_DATA_ROLE:
                DataRole dataRole = (kObject instanceof DataRole) ? (DataRole)kObject : wsMgr.resolve(transaction,
                                                                                                      kObject,
                                                                                                      DataRole.class);
                return (T)new RestVdbDataRole(baseUri, dataRole, transaction);
            case VDB_IMPORT:
                VdbImport vdbImport = wsMgr.resolve(transaction, kObject, VdbImport.class);
                return (T)new RestVdbImport(baseUri, vdbImport, transaction);
            case VDB_MASK:
                Mask mask = wsMgr.resolve(transaction, kObject, Mask.class);
                return (T)new RestVdbMask(baseUri, mask, transaction);
            case MODEL:
                Model model = wsMgr.resolve(transaction, kObject, Model.class);
                return (T)new RestVdbModel(baseUri, model, transaction);
            case VDB_MODEL_SOURCE:
                ModelSource source = wsMgr.resolve(transaction, kObject, ModelSource.class);
                return (T)new RestVdbModelSource(baseUri, source, transaction);
            case TABLE:
                Table table = wsMgr.resolve(transaction, kObject, Table.class);
                return (T)new RestVdbModelTable(baseUri, table, transaction);
            case COLUMN:
                Column column = wsMgr.resolve(transaction, kObject, Column.class);
                return (T)new RestVdbModelTableColumn(baseUri, column, transaction);
            case VDB_PERMISSION:
                Permission permission = wsMgr.resolve(transaction, kObject, Permission.class);
                return (T)new RestVdbPermission(baseUri, permission, transaction);
            case VDB_TRANSLATOR:
                Translator translator = wsMgr.resolve(transaction, kObject, Translator.class);
                return (T)new RestVdbTranslator(baseUri, translator, transaction);
            case CONNECTION:
                Connection connection = wsMgr.resolve(transaction, kObject, Connection.class);
                return (T)new RestConnection(baseUri, connection, transaction);
            case DATASERVICE:
                Dataservice dataService = wsMgr.resolve(transaction, kObject, Dataservice.class);
                return (T)new RestDataservice(baseUri, dataService, false, transaction);
            case UNKNOWN:
                return null;
            default:
                return (T)new RestBasicEntity(baseUri, kObject, transaction);
        }
    }

    /**
     * @param kObject the object
     * @param baseUri the base uri
     * @param transaction the transaction
     * @return the rest object for the given kObject
     * @throws KException if error occurs
     */
    public <T extends RestBasicEntity> T create(KomodoObject kObject, URI baseUri, UnitOfWork transaction) throws KException {
        return create(kObject, baseUri, transaction, new KomodoProperties());
    }

    /**
     * @param basicEntity the entity to be resolved
     * @param klazz the class to resolve it to
     * @return the resolved class or null
     */
    public static <T extends RestBasicEntity> T resolve(RestBasicEntity basicEntity, Class<T> klazz) {
        try {
            T instance = klazz.newInstance();
            basicEntity.clone(instance);

            return instance;
        } catch (Exception ex) {
            LOGGER.error("Failure to resolve entity", ex); //$NON-NLS-1$
            return null;
        }
    }

    private KomodoObject createTemporaryParent(UnitOfWork transaction, Repository repository, String primaryType) throws KException {
        String wkspPath = repository.komodoWorkspace(transaction).getAbsolutePath();
        String timeNow = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS"));
        return repository.add(transaction, wkspPath, timeNow, primaryType);
    }

    public RestMetadataVdb createMetadataVdb(UnitOfWork transaction, Repository repository, 
                                                                                       TeiidVdb teiidVdb, URI baseUri) throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only");

        KomodoObject parent = createTemporaryParent(transaction, repository, null);
        KomodoObject vdbObj = parent.getObjectFactory().exportTeiidVdb(transaction, parent, teiidVdb);
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, transaction);
        Vdb vdb = wsMgr.resolve(transaction, vdbObj, Vdb.class);

        return new RestMetadataVdb(baseUri, vdb, transaction, false);
    }

    public RestMetadataVdbTranslator createMetadataTranslator(UnitOfWork transaction, Repository repository,
                                                                                                  TeiidTranslator teiidTranslator, URI baseUri)
                                                                                                  throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only");

        KomodoObject parent = createTemporaryParent(transaction, repository, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, transaction);
        Vdb parentVdb = mgr.resolve(transaction, parent, Vdb.class);

        String type = teiidTranslator.getType() != null ? teiidTranslator.getType() : teiidTranslator.getName();
        Translator translator = RelationalModelFactory.createTranslator(transaction, repository, parentVdb, teiidTranslator.getName(), type);

        translator.setDescription(transaction, teiidTranslator.getDescription());
        Properties props = teiidTranslator.getProperties();
        for (Entry<Object, Object> entry : props.entrySet()) {
            translator.setProperty(transaction, entry.getKey().toString(), entry.getValue());
        }

        return new RestMetadataVdbTranslator(baseUri, translator, transaction);
    }

    public RestMetadataConnection createMetadataDataSource(UnitOfWork transaction, Repository repository,
                                                                                                 TeiidDataSource teiidDataSource, URI baseUri)
                                                                                                   throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only");

        KomodoObject parent = createTemporaryParent(transaction, repository, null);
        Connection connection = RelationalModelFactory.createConnection(transaction, repository,parent.getAbsolutePath(), teiidDataSource.getName());
        connection.setDriverName(transaction, teiidDataSource.getType());
        connection.setJndiName(transaction, teiidDataSource.getJndiName());

        for (Entry<Object, Object> property : teiidDataSource.getProperties().entrySet()) {
            String key = property.getKey().toString();
            if (TeiidDataSource.DATASOURCE_DRIVERNAME.equals(key) ||
                    TeiidDataSource.DATASOURCE_JNDINAME.equals(key))
                continue; // Already set as explicit fields

            connection.setProperty(transaction, key, property.getValue());
        }

        return new RestMetadataConnection(baseUri, connection, transaction);
    }

    /*
     * Get the default value for the Managed ConnectionFactory class
     * @param propDefns the collection of property definitions
     * @return default value of the ManagedConnectionFactory, null if not found.
     */
    private String getManagedConnectionFactoryClassDefault (Collection<TeiidPropertyDefinition> propDefns) {
        String resultValue = null;
        for(TeiidPropertyDefinition pDefn : propDefns) {
            if(pDefn.getName().equalsIgnoreCase(Template.CONN_FACTORY_CLASS_KEY)) {
                resultValue=(String)pDefn.getDefaultValue();
                break;
            }
        }
        return resultValue;
    }
        
    private TemplateEntry createMetadataTemplateEntry(UnitOfWork transaction, Template template, String factoryClass,
                           TeiidPropertyDefinition definition) throws KException {

        TemplateEntry templateEntry = template.addEntry(transaction, definition.getName());
        templateEntry.setDescription(transaction, definition.getDescription());
        templateEntry.setDisplayName(transaction, definition.getDisplayName());
    
        Collection<String> allowedValues = definition.getAllowedValues();
        if (allowedValues != null && ! allowedValues.isEmpty()) {
            List<Object> valuesList = new ArrayList<Object>();
            for (String value : allowedValues)
                valuesList.add(value);
    
            templateEntry.setAllowedValues(transaction, valuesList);
        }
    
        templateEntry.setCategory(transaction, definition.getCategory());
        templateEntry.setDefaultValue(transaction, definition.getDefaultValue());
        templateEntry.setTypeClassName(transaction, definition.getPropertyTypeClassName());
        templateEntry.setConstrainedToAllowedValues(transaction, definition.isConstrainedToAllowedValues());
        templateEntry.setAdvanced(transaction, definition.isAdvanced());
        templateEntry.setMasked(transaction, definition.isMasked());
        templateEntry.setModifiable(transaction, definition.isModifiable());
        templateEntry.setRequired(transaction, definition.isRequired());
        templateEntry.setCustomProperties(transaction, definition.getProperties());
    
        // Copy the 'managedconnectionfactory-class' default value into the 'class-name' default value
        if(definition.getName().equals(Template.CLASSNAME_KEY)) {
            templateEntry.setDefaultValue(transaction, factoryClass);
            templateEntry.setRequired(transaction, true);
            templateEntry.setModifiable(transaction, false);
        }

        return templateEntry;
    }

    public RestMetadataTemplate createMetadataTemplate(UnitOfWork transaction, Repository repository,
                                                                                         String templateName,
                                                                                         Collection<TeiidPropertyDefinition> propertyDefns,
                                                                                         URI baseUri)
                                                                                         throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only");

        KomodoObject parent = createTemporaryParent(transaction, repository, null);

        Template template = RelationalModelFactory.createTemplate(transaction, repository, parent, templateName);

        // Get the Managed connection factory class for rars
        String factoryClass = getManagedConnectionFactoryClassDefault(propertyDefns);

        for (TeiidPropertyDefinition definition : propertyDefns) {
            createMetadataTemplateEntry(transaction, template, factoryClass, definition);
        }

        return new RestMetadataTemplate(baseUri, template, transaction);
    }

    public List<RestMetadataTemplateEntry> createMetadataTemplateEntry(UnitOfWork transaction, Repository repository,
                                                                                                             Collection<TeiidPropertyDefinition> propertyDefns,
                                                                                                             URI baseUri)
                                                                                                             throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only");

        KomodoObject parent = createTemporaryParent(transaction, repository, null);
        Template template = RelationalModelFactory.createTemplate(transaction, repository, parent, "tempTemplate");
        
        // Get the Managed connection factory class for rars
        String factoryClass = getManagedConnectionFactoryClassDefault(propertyDefns);

        List<RestMetadataTemplateEntry> restEntries = new ArrayList<>();
        for (TeiidPropertyDefinition definition : propertyDefns) {
            TemplateEntry entry = createMetadataTemplateEntry(transaction, template, factoryClass, definition);
           RestMetadataTemplateEntry restEntry = new RestMetadataTemplateEntry(baseUri, entry, transaction);
           restEntries.add(restEntry);
        }

        return restEntries;
    }
    
	public RestServiceCatalogDataSource createServiceCatalogDataSource(UnitOfWork transaction, Repository repository,
			ServiceCatalogDataSource datasource, URI baseUri) throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only");
        
        // TODO:  phantomjinx what needs to be done here?
        KomodoObject parent = createTemporaryParent(transaction, repository, null);
        return new RestServiceCatalogDataSource(baseUri, parent, datasource, transaction);
	}

    public RestBuildStatus createBuildStatus(UnitOfWork transaction, Repository repository,
            BuildStatus status, URI baseUri) throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only");

        // TODO:  phantomjinx what needs to be done here?
        KomodoObject parent = createTemporaryParent(transaction, repository, null);
        return new RestBuildStatus(baseUri, parent, status, transaction);
    }
}
