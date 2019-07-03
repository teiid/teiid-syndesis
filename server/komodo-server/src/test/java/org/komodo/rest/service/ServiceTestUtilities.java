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
package org.komodo.rest.service;

import static org.junit.Assert.assertNotNull;

import java.io.InputStream;
import java.util.LinkedHashSet;
import java.util.Map;
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
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.View;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

public final class ServiceTestUtilities implements StringConstants {

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
        UnitOfWork uow = repository.createTransaction(user, "Import Dataservice", false, callback, user); //$NON-NLS-1$
    
        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
    
        KomodoObject workspace = repository.komodoWorkspace(uow);
        DataserviceConveyor dsConveyor = new DataserviceConveyor(repository);
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
    
        return repository.createTransaction(user, "Read-Only Tx", true, null, user); //$NON-NLS-1$
    }

    /**
     * @param user initiating call
     * @param name of the vdb to find
     *
     * @return the vdb directly from the kEngine
     * @throws Exception if error occurs
     */
    public Vdb getVdb(String user, String vdbName) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdb " + vdbName, true, null, user); //$NON-NLS-1$
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
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdbs", true, null, user); //$NON-NLS-1$
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
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdb model " + vdbModelName, true, null, user); //$NON-NLS-1$
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
     * @param user initiaiting the call
     * @param vdbName the name of the parent vdb
     * @param vdbModelName the name of the model
     * @param viewName the name of the view
     *
     * @return the view directly from the kEngine
     * @throws Exception if error occurs
     */
    public View getVdbModelView(String user, String vdbName, String vdbModelName, String viewName) throws Exception {
        Model vdbModel = getVdbModel(user, vdbName, vdbModelName);
        if (vdbModel == null)
            return null;
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdb model view " + viewName, true, null, user); //$NON-NLS-1$
        View[] views = vdbModel.getViews(uow, viewName);
        View theView = null;
        for(View view : views) {
            if (viewName.equals(view.getName(uow))) {
                theView = view;
                break;
            }
        }
        uow.commit();
    
        return theView;
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
        UnitOfWork uow = repository.createTransaction(user, "Create Dataservice", false, callback, user); //$NON-NLS-1$
    
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
    
        UnitOfWork uow = repository.createTransaction(user, "Find dataservice " + dataserviceName, true, null, user); //$NON-NLS-1$
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
    
        UnitOfWork uow = repository.createTransaction(user, "Find dataservices", true, null, user); //$NON-NLS-1$
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
        UnitOfWork uow = repository.createTransaction(user, "Create VDB", false, callback, user); //$NON-NLS-1$
    
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
        UnitOfWork uow = repository.createTransaction(user, "Create Model", false, callback, user); //$NON-NLS-1$
    
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

    /**
     * Create a View within a vdb model in the komodo engine
     *
     * @param vdbName the vdb name
     * @param modelName the model name
     * @param viewName the view name
     * @param user initiating call
     * @throws Exception if error occurs
     */
    public void createVdbModelView(String vdbName, String modelName, String viewName, String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Create View", false, callback, user); //$NON-NLS-1$
    
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        if(!wsMgr.hasChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
            wsMgr.createVdb(uow, null, vdbName, vdbName);
        }
        
        KomodoObject kobj = wsMgr.getChild(uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        Vdb vdb = Vdb.RESOLVER.resolve(uow, kobj);
        
        if(!vdb.hasChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
        	vdb.addModel(uow, modelName);
        }
        
        Model[] models = vdb.getModels(uow, modelName);
        models[0].addView(uow, viewName);
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    public void deleteVdbs(String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Delete vdbs", false, callback, user); //$NON-NLS-1$
        
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        Vdb[] vdbs = wsMgr.findVdbs(uow);
        for (Vdb vdb : vdbs) {
            wsMgr.delete(uow, vdb);
        }
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
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
        UnitOfWork uow = repository.createTransaction(user, "Delete object at path " + absPath, false, callback, user); //$NON-NLS-1$
        
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(repository, uow);
        KomodoObject kObject = repository.getFromWorkspace(uow, absPath);
        if (kObject != null) {
            wsMgr.delete(uow, kObject);
        }
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    /**
     * @param user
     * @return the absolute path of the workspace of the given user
     * @throws Exception
     */
    public String getWorkspace(String user) throws Exception {
        UnitOfWork uow = repository.createTransaction(user, "Get Workspace", true, null, user); //$NON-NLS-1$
        KomodoObject workspace = repository.komodoWorkspace(uow);
        return workspace.getAbsolutePath();
    }

    public ViewEditorState addViewEditorState(String user, String stateId,
                                                           String undoId,
                                                           Map<String, String> undoArguments,
                                                           String redoId,
                                                           Map<String, String> redoArguments,
                                                           String viewDefnName,
                                                           String viewDefnDescription,
                                                           String[] sourcePaths,
                                                           String compName,
                                                           String compDescr,
                                                           String compLeftSource,
                                                           String compRightSource,
                                                           String leftColumn,
                                                           String rightColumn,
                                                           String type,
                                                           String operator) throws Exception {
        UnitOfWork uow = repository.createTransaction(user, "Create View Editor State", false, null, user); //$NON-NLS-1$
        KomodoObject profileObj = repository.komodoProfile(uow);
        assertNotNull(profileObj);

        Profile profile = new AdapterFactory().adapt(uow, profileObj, Profile.class);
        ViewEditorState viewEditorState = profile.addViewEditorState(uow, stateId);
        
        // Add the Undo-Redo commands
        StateCommandAggregate stateCmdAgg = viewEditorState.addCommand(uow);
        stateCmdAgg.setUndo(uow, undoId, undoArguments);
        stateCmdAgg.setRedo(uow, redoId, redoArguments);
        
        // Add the ViewDefinition
        ViewDefinition viewDefn = viewEditorState.setViewDefinition(uow);
        viewDefn.setViewName(uow, viewDefnName);
        viewDefn.setDescription(uow, viewDefnDescription);
        for(String sourcePath: sourcePaths) {
            viewDefn.addSourcePath(uow, sourcePath);
        }
        viewDefn.setComplete(uow, true);
        SqlComposition comp = viewDefn.addSqlComposition(uow, compName);
        comp.setDescription(uow, compDescr);
        comp.setLeftSourcePath(uow, compLeftSource);
        comp.setRightSourcePath(uow, compRightSource);
        comp.setLeftCriteriaColumn(uow, leftColumn);
        comp.setRightCriteriaColumn(uow, rightColumn);
        comp.setType(uow, type);
        comp.setOperator(uow, operator);
        uow.commit();
        return viewEditorState;
    }

    public ViewEditorState getViewEditorState( final String user,
                                               final String stateId ) throws Exception {
        UnitOfWork uow = repository.createTransaction(user, "getViewEditorState", true, null, user); //$NON-NLS-1$

        try {
            KomodoObject profileObj = repository.komodoProfile(uow);
            assertNotNull(profileObj);
    
            Profile profile = new AdapterFactory().adapt(uow, profileObj, Profile.class);
            ViewEditorState[] viewEditorStates = profile.getViewEditorStates(uow, stateId);
    
            if (viewEditorStates == null || viewEditorStates.length == 0)
                return null;
    
            for (ViewEditorState editorState : viewEditorStates) {
                if (editorState.getName(uow).equals(stateId)) {
                    return editorState;
                }
            }
    
            return null;
        } finally {
            uow.commit();
        }
    }

    public boolean viewEditorStateExists(String user, String stateId) throws Exception {
        return getViewEditorState(user, stateId) != null;
    }

    public void removeViewEditorState(String user, String stateId) throws Exception {
        UnitOfWork uow = repository.createTransaction(user, "Remove View Editor State", false, null, user); //$NON-NLS-1$
        KomodoObject profileObj = repository.komodoProfile(uow);
        assertNotNull(profileObj);

        Profile profile = new AdapterFactory().adapt(uow, profileObj, Profile.class);
        ViewEditorState[] viewEditorStates = profile.getViewEditorStates(uow, stateId);
        if (viewEditorStates == null || viewEditorStates.length == 0)
            return;

        profile.removeViewEditorState(uow, stateId);
        uow.commit();
    }
}
