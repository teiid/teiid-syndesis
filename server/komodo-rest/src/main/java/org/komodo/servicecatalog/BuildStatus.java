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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class BuildStatus {

    public enum Status {
        NOTFOUND,
        SUBMITTED,
        CONFIGURING,
        BUILDING,
        DEPLOYING,
        RUNNING,
        FAILED,
        CANCELLED
    }

    public static class RouteStatus {
        private final String name;
        private final ProtocolType kind;
        private String host;
        private String path;
        private String target;
        private String port;
        private boolean secure;

        public RouteStatus(String name, ProtocolType kind) {
            this.name = name;
            this.kind = kind;
        }

        public String getName() {
            return name;
        }

        public ProtocolType getKind() {
            return kind;
        }

        public String getHost() {
            return host;
        }

        public void setHost(String host) {
            this.host = host;
        }

        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }

        public String getTarget() {
            return target;
        }

        public void setTarget(String target) {
            this.target = target;
        }

        public String getPort() {
            return port;
        }

        public void setPort(String port) {
            this.port = port;
        }

        public boolean isSecure() {
            return this.secure;
        }

        public void setSecure(boolean secure) {
            this.secure = secure;
        }
    }

    private Status status = Status.NOTFOUND;
    private PublishConfiguration publishConfiguration;

    private String buildName;
    private String deploymentName;
    private final String vdbName;
    private String namespace;
    private long lastUpdated = 0L;
    private String statusMessage;

    private List<RouteStatus> routes = null;

    public BuildStatus(String vdbName) {
        this.vdbName = vdbName;
    }

    public PublishConfiguration publishConfiguration() {
        return publishConfiguration;
    }

    public synchronized void setPublishConfiguration(PublishConfiguration publishConfiguration) {
        this.publishConfiguration = publishConfiguration;
    }

    public String buildName() {
        return buildName;
    }

    public synchronized void setBuildName(String buildName) {
        this.buildName = buildName;
    }

    public String deploymentName() {
        return deploymentName;
    }

    public synchronized void setDeploymentName(String deploymentName) {
        this.deploymentName = deploymentName;
    }

    public String vdbName() {
        return vdbName;
    }

    public String namespace() {
        return namespace;
    }

    public synchronized void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    public String statusMessage() {
        return statusMessage;
    }

    public synchronized void setStatusMessage(String statusMessage) {
        this.statusMessage = statusMessage;
    }

    public long lastUpdated() {
        return lastUpdated;
    }

    public synchronized void setLastUpdated() {
        this.lastUpdated = System.currentTimeMillis();
    }

    public Status status() {
        return status;
    }

    public synchronized void setStatus(Status status) {
        this.status = status;
    }

    public List<RouteStatus> routes() {
        if (this.routes == null)
            return Collections.emptyList();

        return this.routes;
    }

    public void addRoute(RouteStatus route) {
        if (route == null)
            return;

        if (this.routes == null)
            this.routes = new ArrayList<RouteStatus>();

        this.routes.add(route);
    }

    public void setRoutes(List<RouteStatus> routes) {
        this.routes = routes;
    }
}
