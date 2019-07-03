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
package org.komodo.rest.relational;

import java.net.URI;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import org.komodo.openshift.BuildStatus;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.response.RestSyndesisDataSource;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.relational.response.RestVdbModelSource;
import org.komodo.rest.relational.response.RestVdbModelTable;
import org.komodo.rest.relational.response.RestVdbModelTableColumn;
import org.komodo.rest.relational.response.RestVdbModelView;
import org.komodo.rest.relational.response.virtualization.RestVirtualizationStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.SyndesisDataSource;
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
            case MODEL:
                Model model = wsMgr.resolve(transaction, kObject, Model.class);
                return (T)new RestVdbModel(baseUri, model, transaction);
            case VDB_MODEL_SOURCE:
                ModelSource source = wsMgr.resolve(transaction, kObject, ModelSource.class);
                return (T)new RestVdbModelSource(baseUri, source, transaction);
            case VIEW:
                View view = wsMgr.resolve(transaction, kObject, View.class);
                return (T)new RestVdbModelView(baseUri, view, transaction);
            case TABLE:
                Table table = wsMgr.resolve(transaction, kObject, Table.class);
                return (T)new RestVdbModelTable(baseUri, table, transaction);
            case COLUMN:
                Column column = wsMgr.resolve(transaction, kObject, Column.class);
                return (T)new RestVdbModelTableColumn(baseUri, column, transaction);
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
        String timeNow = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss-SSS")); //$NON-NLS-1$
        return repository.add(transaction, wkspPath, timeNow, primaryType);
    }

    /**
	 * Create RestSyndesis data source
	 * @param transaction the transaction
	 * @param repository the repo
	 * @param datasource syndesis data source
	 * @param baseUri the uri
	 * @return the rest source
	 * @throws Exception if error occurs
	 */
	public RestSyndesisDataSource createSyndesisDataSource(UnitOfWork transaction, 
	                                                       Repository repository,
			                                               SyndesisDataSource datasource, 
			                                               URI baseUri) throws Exception {
        checkTransaction(transaction);
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback-only"); //$NON-NLS-1$
        
        // TODO:  phantomjinx what needs to be done here?
        KomodoObject parent = createTemporaryParent(transaction, repository, null);
        return new RestSyndesisDataSource(baseUri, parent, datasource, transaction);
	}

    /**
     * Create RestVirtualizationStatus
     * @param status the build status
     * @param baseUri the base uri
     * @return RestVirtualizationStatus
     * @throws Exception if error occurs
     */
    public RestVirtualizationStatus createBuildStatus(BuildStatus status, URI baseUri) throws Exception {
        return new RestVirtualizationStatus(baseUri, status);
    }
}
