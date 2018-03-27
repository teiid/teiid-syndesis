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
package org.komodo.servicecatalog;

public class BuildStatus {

    public enum Status {
        NOTFOUND,
        BUILDING,
        DEPLOYING,
        RUNNING,
        FAILED,
        CANCELLED
    }

    protected String buildName;
    protected String deploymentName;
    protected String vdbName;
    protected String namespace;
    protected long lastUpdated = 0L;
    protected String statusMessage;
    protected Status status = Status.NOTFOUND;

    protected PublishConfiguration publishConfiguration;
    
    public String getBuildName() {
        return buildName;
    }
    public String getDeploymentName() {
        return deploymentName;
    }
    public String getVdbName() {
        return vdbName;
    }
    public String getNamespace() {
        return namespace;
    }
    public String getStatusMessage() {
        return statusMessage;
    }
    public long getLastUpdated() {
        return lastUpdated;
    }
    public String getStatus() {
        return status.name();
    }
}