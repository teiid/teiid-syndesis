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
package org.komodo.rest.relational.response;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.komodo.rest.AbstractKEntity;
import org.komodo.servicecatalog.BuildStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

public class RestBuildStatus extends AbstractKEntity {
    public static final String VDB_NAME = "vdb-name";
    public static final String BUILD_NAME = "build-name";
    public static final String DEPLOYMENT_NAME = "deployment-name";
    public static final String NAMESPACE = "namespace";
    public static final String STATUS = "build-status";
    public static final String STATUS_MSG = "build-status-message";
    public static final String LAST_UPDATED = "last-updated";

    private static SimpleDateFormat sdf = new SimpleDateFormat("M-dd-yyyy hh:mm:ss");

    public RestBuildStatus() {
    }

    public RestBuildStatus(URI baseUri, KomodoObject parent, BuildStatus status,
            UnitOfWork uow) throws KException {

        ArgCheck.isNotNull(status, "status"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        Date date = new Date(status.getLastUpdated());
        addTuple(VDB_NAME, status.getVdbName());
        addTuple(BUILD_NAME, status.getBuildName());
        addTuple(DEPLOYMENT_NAME, status.getDeploymentName());
        addTuple(STATUS, status.getStatus());
        addTuple(STATUS_MSG, status.getStatusMessage());
        addTuple(NAMESPACE, status.getNamespace());
        addTuple(LAST_UPDATED, sdf.format(date))  ;

//        Properties settings = getUriBuilder().createSettings(SettingNames.CONNECTION_NAME, getId());
//        URI parentUri = getUriBuilder().mServerConnectionsUri();
//        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);
//
//        addLink(new RestLink(LinkType.SELF, getUriBuilder().connectionUri(LinkType.SELF, settings)));
//        addLink(new RestLink(LinkType.PARENT, getUriBuilder().connectionUri(LinkType.PARENT, settings)));
    }
}