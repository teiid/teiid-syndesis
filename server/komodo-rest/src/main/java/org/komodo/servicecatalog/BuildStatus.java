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
    }

    protected String buildName;
    protected String deploymentName;
    protected String vdbName;
    protected String namespace;
    protected long lastUpdated = 0L;
    protected String statusMessage;
    protected Status status = Status.NOTFOUND;
    protected List<RouteStatus> routes = null;

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

    public List<RouteStatus> getRoutes() {
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