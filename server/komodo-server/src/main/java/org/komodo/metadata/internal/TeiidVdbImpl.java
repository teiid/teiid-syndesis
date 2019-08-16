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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.komodo.metadata.Messages;
import org.komodo.metadata.TeiidVdb;
import org.teiid.adminapi.Model;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.VDB.Status;
import org.teiid.adminapi.VDBImport;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.core.util.ArgCheck;
import org.teiid.metadata.Schema;
import org.teiid.query.metadata.TransformationMetadata;

public class TeiidVdbImpl implements TeiidVdb, Comparable<TeiidVdbImpl> {

    private static final String PREVIEW = "preview"; //$NON-NLS-1$

    private static final String DEPLOYMENT_NAME = "deployment-name"; //$NON-NLS-1$

    private String name;

    private String version;

    private boolean hasModels = false;

    private List<String> errors;

    private Collection<String> modelNames = new ArrayList<>();

    private Properties properties = new Properties();

    private VDBMetaData vdb;

    public TeiidVdbImpl(VDB vdb) throws Exception {
        ArgCheck.isNotNull(vdb, "vdb"); //$NON-NLS-1$

        if (! (vdb instanceof VDBMetaData))
            throw new Exception(Messages.getString(Messages.MetadataServer.onlySupportingDynamicVdbs));

        name = vdb.getName();

        // Autoboxing first if version is an int as defined in teiid 8
        Object o = vdb.getVersion();
        version = o.toString();

        hasModels = !vdb.getModels().isEmpty();

        errors = vdb.getValidityErrors();

        for (Model model : vdb.getModels()) {
            modelNames.add(model.getName());
        }

        for (String name : vdb.getProperties().stringPropertyNames()) {
            properties.setProperty(name, vdb.getPropertyValue(name));
        }

        this.vdb = (VDBMetaData)vdb;
    }

    @Override
    public int compareTo(TeiidVdbImpl vdb) {
        ArgCheck.isNotNull(vdb, "vdb"); //$NON-NLS-1$
        return getName().compareTo(vdb.getName());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (obj.getClass() != getClass())
            return false;

        TeiidVdb other = (TeiidVdb)obj;

        if (getName().equals(other.getName()))
            return true;

        return false;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
        return result;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getVersion() {
        return version;
    }

    @Override
    public boolean isActive() {
        return Status.ACTIVE.equals(vdb.getStatus());
    }

    @Override
    public boolean isLoading() {
        return Status.LOADING.equals(vdb.getStatus());
    }

    @Override
    public boolean hasFailed() {
        return Status.FAILED.equals(vdb.getStatus());
    }

    @Override
    public boolean wasRemoved() {
        return Status.REMOVED.equals(vdb.getStatus());
    }

    @Override
    public List<String> getValidityErrors() {
        if (this.errors != null)
            return Collections.unmodifiableList(this.errors);

        return Collections.emptyList();
    }

    @Override
    public boolean hasModels() {
        return hasModels;
    }

    @Override
    public Collection<String> getModelNames() {
        if (!hasModels())
            return Collections.emptyList();

        return modelNames;
    }

    @Override
    public String getPropertyValue(String key) {
        return properties.getProperty(key);
    }

    @Override
    public Properties getProperties() {
        return properties;
    }

    @Override
    public List<? extends VDBImport> getImports() {
        return this.vdb.getVDBImports();
    }

    public VDBMetaData getVDBMetaData() {
        return this.vdb;
    }

    @Override
    public Schema getSchema(String name) {
        if (!isActive()) {
            return null;
        }
        TransformationMetadata qmi = vdb.getAttachment(TransformationMetadata.class);
        return qmi.getMetadataStore().getSchema(name);
    }

}
