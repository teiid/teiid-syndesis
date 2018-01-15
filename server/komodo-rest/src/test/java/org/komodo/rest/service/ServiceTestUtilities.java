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
package org.komodo.rest.service;

import java.io.InputStream;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.komodo.core.KEngine;
import org.komodo.core.repository.SynchronousCallback;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.internal.DataserviceConveyor;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.model.Model;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

public final class ServiceTestUtilities {

    private final Repository repository;

    private final MetadataInstance metadataInstance;

    private Set<String> objectPaths = new LinkedHashSet<String>();

    public ServiceTestUtilities(KEngine kEngine) throws Exception {
        this.repository = kEngine.getDefaultRepository();
        this.metadataInstance = kEngine.getMetadataInstance();
    }

    public void logObjectPath(String objectPath) {
        objectPaths.add(objectPath);
    }

    public void deleteLogged(String user) {
        if (objectPaths.isEmpty())
            return;
    
        for (String objectPath : objectPaths) {
            try {
                deleteObject(objectPath, user);
            } catch (Exception e) {
                System.err.println("Failed to clean up object at path: " + objectPath);
            }
        }
    }

    /**
     * Import a dataservice into the komodo engine
     *
     * @param dsStream dataservice input stream
     * @param user initiating import
     *
     * @throws Exception if error occurs
     */
    public void importDataservice(InputStream dsStream, String user) throws Exception {
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Import Dataservice", false, callback); //$NON-NLS-1$
    
        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
    
        KomodoObject workspace = repository.komodoWorkspace(uow);
        DataserviceConveyor dsConveyor = new DataserviceConveyor(repository, metadataInstance);
        dsConveyor.dsImport(uow, dsStream, workspace, importOptions, importMessages);
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    /**
     * @param user initiating call
     * @return a transaction allowing for reading of information in the repository
     * @throws KException if error occurs
     */
    public UnitOfWork createReadTransaction(String user) throws KException {
    
        return repository.createTransaction(user, "Read-Only Tx", true, null); //$NON-NLS-1$
    }

    /**
     * @param user initiating call
     * @param name of the vdb to find
     *
     * @return the vdb directly from the kEngine
     * @throws Exception if error occurs
     */
    public Vdb getVdb(String user, String vdbName) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdb " + vdbName, true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Vdb[] vdbs = mgr.findVdbs(uow);
        Vdb theVdb = null;
        for(Vdb vdb : vdbs) {
            if (vdbName.equals(vdb.getName(uow))) {
                theVdb = vdb;
                break;
            }
        }
        uow.commit();
    
        return theVdb;
    }

    /**
     * @param user initiating call
     *
     * @return the vdbs directly from the kEngine
     * @throws Exception if error occurs
     */
    public Vdb[] getVdbs(String user) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdbs", true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Vdb[] vdbs = mgr.findVdbs(uow);
    
        uow.commit();
    
        return vdbs;
    }

    /**
     * @param user initiaiting the call
     * @param vdbName the name of the parent vdb
     * @param vdbModelName the name of the model
     *
     * @return the model directly from the kEngine
     * @throws Exception if error occurs
     */
    public Model getVdbModel(String user, String vdbName, String vdbModelName) throws Exception {
        Vdb vdb = getVdb(user, vdbName);
        if (vdb == null)
            return null;
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdb model " + vdbModelName, true, null); //$NON-NLS-1$
        Model[] models = vdb.getModels(uow, vdbModelName);
        Model theVdbModel = null;
        for(Model model : models) {
            if (vdbModelName.equals(model.getName(uow))) {
                theVdbModel = model;
                break;
            }
        }
        uow.commit();
    
        return theVdbModel;
    }

    /**
     * Create a dataservice in the komodo engine (used for mostly test purposes)
     *
     * @param dataserviceName the service name
     * @param populateWithSamples true if dataservice should be populated with example vdbs
     * @param user initiating transaction
     *
     * @throws Exception if error occurs
     */
    public void createDataservice(String dataserviceName, boolean populateWithSamples, String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Create Dataservice", false, callback); //$NON-NLS-1$
    
        KomodoObject wkspace = repository.komodoWorkspace(uow);
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
    
        VdbImporter importer = new VdbImporter(repository);
        ImportMessages importMessages = new ImportMessages();
        ImportOptions importOptions = new ImportOptions();
        importOptions.setOption(OptionKeys.HANDLE_EXISTING, ExistingNodeOptions.RETURN);
    
        String portfolioSample = KomodoUtilService.SAMPLES[1];
        String nwSample = KomodoUtilService.SAMPLES[4];
        InputStream portSampleStream = KomodoUtilService.getVdbSample(portfolioSample);
        InputStream nwindSampleStream = KomodoUtilService.getVdbSample(nwSample);
    
        importer.importVdb(uow, portSampleStream, wkspace, importOptions, importMessages);
        importer.importVdb(uow, nwindSampleStream, wkspace, importOptions, importMessages);
    
        KomodoObject pfSampleObj = wkspace.getChild(uow, "Portfolio");
        Vdb pfVdb = wsMgr.resolve(uow, pfSampleObj, Vdb.class);
    
        KomodoObject nwSampleObj = wkspace.getChild(uow, "Northwind");
        Vdb nwVdb = wsMgr.resolve(uow, nwSampleObj, Vdb.class);
    
        Dataservice dataservice = wsMgr.createDataservice(uow, wkspace, dataserviceName);
        dataservice.setDescription(uow, "This is my dataservice");
    
        dataservice.addVdb(uow, pfVdb);
        dataservice.setServiceVdb(uow, nwVdb);

        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
        logObjectPath(dataservice.getAbsolutePath());
    }

    /**
     * @param user initiating call
     * @param name of the dataservice to find
     *
     * @return the dataservice directly from the kEngine
     * @throws Exception if error occurs
     */
    public Dataservice getDataservice(String user, String dataserviceName) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find dataservice " + dataserviceName, true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Dataservice[] dataservices = mgr.findDataservices(uow);
        Dataservice theDataservice = null;
        for(Dataservice ds : dataservices) {
            if (dataserviceName.equals(ds.getName(uow))) {
                theDataservice = ds;
                break;
            }
        }
        uow.commit();
    
        return theDataservice;
    }

    /**
     * @param user initiating call
     *
     * @return the dataservices directly from the kEngine
     * @throws Exception if error occurs
     */
    public Dataservice[] getDataservices(String user) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find dataservices", true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Dataservice[] services = mgr.findDataservices(uow);
        uow.commit();
    
        return services;
    }

    /**
     * Create a Vdb in the komodo engine
     *
     * @param vdbName the vdb name
     * @param user initiating call
     * @throws Exception if error occurs
     */
    public void createVdb(String vdbName, String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Create VDB", false, callback); //$NON-NLS-1$
    
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        Vdb vdb = wsMgr.createVdb(uow, null, vdbName, vdbName);
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
        logObjectPath(vdb.getAbsolutePath());
    }

    /**
     * Create a Model within a vdb in the komodo engine
     *
     * @param vdbName the vdb name
     * @param modelName the vdb name
     * @param user initiating call
     * @throws Exception if error occurs
     */
    public void createVdbModel(String vdbName, String modelName, String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Create Model", false, callback); //$NON-NLS-1$
    
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        if(!wsMgr.hasChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
            wsMgr.createVdb(uow, null, vdbName, vdbName);
        }
        
        KomodoObject kobj = wsMgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        Vdb vdb = Vdb.RESOLVER.resolve(uow, kobj);
        
        vdb.addModel(uow, modelName);
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    public void deleteVdbs(String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Delete vdbs", false, callback); //$NON-NLS-1$
        
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        Vdb[] vdbs = wsMgr.findVdbs(uow);
        for (Vdb vdb : vdbs) {
            wsMgr.delete(uow, vdb);
        }
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    /**
     * Create a connection in the komodo engine
     *
     * @param connectionName the connection name
     * @param user initiating call
     * @throws Exception if error occurs
     */
    public void createConnection(String connectionName, String user) throws Exception {
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Create Connection", false, callback); //$NON-NLS-1$
    
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        Connection connection = wsMgr.createConnection(uow, null, connectionName);

        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
        logObjectPath(connection.getAbsolutePath());
    }

    /**
     * Remove an object directly from the repostory
     *
     * @param absPath the absolute path of the object in the repository
     * @param user the user initiating the call
     * @throws Exception if error occurs
     */
    public void deleteObject(String absPath, String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Delete object at path " + absPath, false, callback); //$NON-NLS-1$
        
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        KomodoObject kObject = repository.getFromWorkspace(uow, absPath);
        if (kObject != null) {
            wsMgr.delete(uow, kObject);
        }
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    /**
     * @param user initiating call
     * @param name of the connection to find
     *
     * @return the connection directly from the kEngine
     * @throws Exception if error occurs
     */
    public Connection getConnection(String user, String connectionName) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find connection " + connectionName, true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Connection[] connections = mgr.findConnections(uow);
        Connection theConnection = null;
        for(Connection connection : connections) {
            if (connectionName.equals(connection.getName(uow))) {
                theConnection = connection;
                break;
            }
        }
        uow.commit();
    
        return theConnection;
    }

    /**
     * @param user initiating call
     *
     * @return the connections directly from the kEngine
     * @throws Exception if error occurs
     */
    public Connection[] getConnections(String user) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find connections", true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Connection[] sources = mgr.findConnections(uow);
        uow.commit();
    
        return sources;
    }

    /**
     * Create a Driver in the komodo engine
     *
     * @param driverName the driver name
     * @throws Exception if error occurs
     */
    public void createDriver(String driverName) throws Exception {
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(Repository.SYSTEM_USER, "Create Driver", false, callback); //$NON-NLS-1$
    
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        Driver driver = wsMgr.createDriver(uow, null, driverName);
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
        logObjectPath(driver.getAbsolutePath());
    }

    /**
     * @param user initiating call
     * @param name of the driver to find
     *
     * @return the driver directly from the kEngine
     * @throws Exception if error occurs
     */
    public Driver getDriver(String user, String driverName) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find driver " + driverName, true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Driver[] drivers = mgr.findDrivers(uow);
        Driver theDriver = null;
        for(Driver driver : drivers) {
            if (driverName.equals(driver.getName(uow))) {
                theDriver = driver;
                break;
            }
        }
        uow.commit();
    
        return theDriver;
    }

    /**
     * @return the drivers directly from the kEngine
     * @throws Exception if error occurs
     */
    public Driver[] getDrivers() throws Exception {
        UnitOfWork uow = repository.createTransaction(Repository.SYSTEM_USER, "Find drivers", true, null); //$NON-NLS-1$
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Driver[] drivers = mgr.findDrivers(uow);
        uow.commit();
    
        return drivers;
    }

    /**
     * @param user
     * @return the absolute path of the workspace of the given user
     * @throws Exception
     */
    public String getWorkspace(String user) throws Exception {
        UnitOfWork uow = repository.createTransaction(user, "Get Workspace", true, null); //$NON-NLS-1$
        KomodoObject workspace = repository.komodoWorkspace(uow);
        return workspace.getAbsolutePath();
    }
}
