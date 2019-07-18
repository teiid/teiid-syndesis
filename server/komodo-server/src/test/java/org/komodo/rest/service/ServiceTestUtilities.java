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

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.komodo.core.KEngineImpl;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.relational.WorkspaceManager;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.View;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.workspace.WorkspaceManagerImpl;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.SynchronousCallback;
import org.komodo.spi.repository.UnitOfWork;

public final class ServiceTestUtilities implements StringConstants {

    private final Repository repository;

    private Set<String> objectPaths = new LinkedHashSet<String>();

    public ServiceTestUtilities(KEngineImpl kEngine) throws Exception {
        this.repository = kEngine.getDefaultRepository();
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
    public VdbImpl getVdb(String user, String vdbName) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find vdb " + vdbName, true, null, user); //$NON-NLS-1$
        WorkspaceManagerImpl mgr = WorkspaceManagerImpl.getInstance(repository, uow);
        VdbImpl theVdb = mgr.findVdb(uow, vdbName);
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
        WorkspaceManager mgr = WorkspaceManagerImpl.getInstance(repository, uow);
        Vdb[] vdbs = mgr.findVdbs(uow, null);
    
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
        View theView = vdbModel.getView(uow, viewName);
        uow.commit();
    
        return theView;
    }

    /**
     * @param user initiating call
     * @param name of the dataservice to find
     *
     * @return the dataservice directly from the kEngine
     * @throws Exception if error occurs
     */
    public DataserviceImpl getDataservice(String user, String dataserviceName) throws Exception {
    
        UnitOfWork uow = repository.createTransaction(user, "Find dataservice " + dataserviceName, true, null, user); //$NON-NLS-1$
        WorkspaceManagerImpl mgr = WorkspaceManagerImpl.getInstance(repository, uow);
        DataserviceImpl[] dataservices = mgr.findDataservices(uow, null);
        DataserviceImpl theDataservice = null;
        for(DataserviceImpl ds : dataservices) {
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
        WorkspaceManager mgr = WorkspaceManagerImpl.getInstance(repository, uow);
        Dataservice[] services = mgr.findDataservices(uow, null);
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
    
        WorkspaceManagerImpl wsMgr = WorkspaceManagerImpl.getInstance(repository, uow);
        VdbImpl vdb = wsMgr.createVdb(uow, vdbName);
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
        logObjectPath(vdb.getAbsolutePath());
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

        WorkspaceManager wsMgr = WorkspaceManagerImpl.getInstance(repository, uow);

        Vdb vdb = wsMgr.findVdb(uow, vdbName);
        
        if(vdb == null) {
            vdb = wsMgr.createVdb(uow, vdbName);
        }
        
        Model model = vdb.getModel(uow, modelName);
        if (model == null) {
        	model = vdb.addModel(uow, modelName);
        }
        
        model.addView(uow, viewName);
    
        uow.commit();
        callback.await(3, TimeUnit.MINUTES);
    }

    public void deleteVdbs(String user) throws Exception {
    
        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(user, "Delete vdbs", false, callback, user); //$NON-NLS-1$
        
        WorkspaceManager wsMgr = WorkspaceManagerImpl.getInstance(repository, uow);
        Vdb[] vdbs = wsMgr.findVdbs(uow, null);
        for (Vdb vdb : vdbs) {
            wsMgr.deleteVdb(uow, vdb);
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
        
        WorkspaceManagerImpl wsMgr = WorkspaceManagerImpl.getInstance(repository, uow);
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
