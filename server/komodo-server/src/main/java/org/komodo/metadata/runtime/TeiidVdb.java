/*************************************************************************************
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
package org.komodo.metadata.runtime;

import java.util.Collection;
import java.util.List;
import java.util.Properties;

import org.teiid.adminapi.VDBImport;
import org.teiid.metadata.Schema;

/**
 *
 */
public interface TeiidVdb {

    /**
     * Extension of a vdb file
     */
    static final String VDB_EXTENSION = "vdb"; //$NON-NLS-1$
    
    /**
     * Extension of a vdb file with dot appended
     */
    static final String VDB_DOT_EXTENSION = ".vdb"; //$NON-NLS-1$
    
    /**
     * @return the name
     */
    String getName();

    /**
     * @return deployed name
     */
    String getDeployedName();

    /**
     * @return the version
     */
    String getVersion();

    /**
     * @return <code>true</code> if this is a preview VDB
     */
    boolean isPreviewVdb();

    /**
     * @return <code>true</code> if this VDB is active
     */
    boolean isActive();

    /**
     * @return <code>true</code> if this VDB is loading
     */
    boolean isLoading();

    /**
     * @return <code>true</code> if this VDB failed
     */
    boolean hasFailed();

    /**
     * @return <code>true</code> if this VDB is removed
     */
    boolean wasRemoved();

    /**
     * @return any validity errors
     */
    List<String> getValidityErrors();

    /**
     * Does the VDB contain any models 
     * 
     * @return <code>true</code> if the vdb has any models 
     */
    boolean hasModels();

    /**
     * Get the names of all the models in this vdb
     * 
     * @return {@link Collection} of model names
     */
    Collection<String> getModelNames();

    /**
     * @param key
     * 
     * @return value of property or null
     */
    String getPropertyValue(String key);
    
    /**
     * 
     * @return vdb properties
     */
    Properties getProperties( );
    
	List<? extends VDBImport> getImports();
	
	/**
	 * Return the live metadata {@link Schema} instance
	 * WARNING: do not modify
	 * @param name
	 * @return
	 */
	Schema getSchema(String name);

}
