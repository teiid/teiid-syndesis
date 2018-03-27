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
package org.komodo.rest.relational.response.virtualization;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.komodo.rest.AbstractKEntity;
import org.komodo.servicecatalog.BuildStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

public class RestVirtualizationStatus extends AbstractKEntity {
    
    public static final String VDB_NAME_LABEL = "vdb_name";

    public static final String BUILD_NAME_LABEL = "build_name";

    public static final String DEPLOYMENT_NAME_LABEL = "deployment_name";

    public static final String NAMESPACE_LABEL = "namespace";

    public static final String STATUS_LABEL = "build_status";

    public static final String STATUS_MSG_LABEL = "build_status_message";

    public static final String LAST_UPDATED_LABEL = "last_updated";

    private static SimpleDateFormat sdf = new SimpleDateFormat("M-dd-yyyy hh:mm:ss");

    public RestVirtualizationStatus() {
    }

    public RestVirtualizationStatus(URI baseUri, KomodoObject parent, BuildStatus status,
            UnitOfWork uow) throws KException {

        ArgCheck.isNotNull(status, "status"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setVdbName(status.getVdbName());
        setBuildName(status.getBuildName());
        setDeploymentName(status.getDeploymentName());
        setStatus(status.getStatus());
        setStatusMsg(status.getStatusMessage());
        setNamespace(status.getNamespace());
        Date date = new Date(status.getLastUpdated());
        setLastUpdated(sdf.format(date))  ;

    }

    public String getVdbName() {
        Object name = tuples.get(VDB_NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setVdbName(String vdbName) {
        tuples.put(VDB_NAME_LABEL, vdbName);        
    }

    public String getBuildName() {
        Object name = tuples.get(BUILD_NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setBuildName(String buildName) {
        tuples.put(BUILD_NAME_LABEL, buildName);        
    }

    public String getDeploymentName() {
        Object name = tuples.get(DEPLOYMENT_NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setDeploymentName(String deploymentName) {
        tuples.put(DEPLOYMENT_NAME_LABEL, deploymentName);        
    }

    public String getNamespace() {
        Object name = tuples.get(NAMESPACE_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setNamespace(String namespace) {
        tuples.put(NAMESPACE_LABEL, namespace);        
    }

    public String getLastUpdated() {
        Object name = tuples.get(LAST_UPDATED_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setLastUpdated(String lastUpdated) {
        tuples.put(LAST_UPDATED_LABEL, lastUpdated);        
    }

    public String getStatus() {
        Object name = tuples.get(STATUS_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setStatus(String status) {
        tuples.put(STATUS_LABEL, status);        
    }

    public String getStatusMsg() {
        Object name = tuples.get(STATUS_MSG_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setStatusMsg(String statusMsg) {
        tuples.put(STATUS_MSG_LABEL, statusMsg);        
    }
}