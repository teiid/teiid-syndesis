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
package org.komodo.openshift;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

@JsonSerialize(as = BuildStatus.class)
@JsonInclude(Include.NON_NULL)
public class BuildStatus {
	
    public static final String VDB_NAME_LABEL = "vdb_name";

    public static final String BUILD_NAME_LABEL = "build_name";

    public static final String DEPLOYMENT_NAME_LABEL = "deployment_name";

    public static final String NAMESPACE_LABEL = "namespace";

    public static final String STATUS_LABEL = "build_status";

    public static final String STATUS_MSG_LABEL = "build_status_message";

    public static final String LAST_UPDATED_LABEL = "last_updated";

    public static final String ROUTES_LABEL = "routes";

    public enum Status {
        NOTFOUND,
        SUBMITTED,
        CONFIGURING,
        BUILDING,
        DEPLOYING,
        RUNNING,
        FAILED,
        CANCELLED,
        DELETE_SUBMITTED,
        DELETE_REQUEUE,
        DELETE_DONE
    }

    @JsonSerialize(as = RouteStatus.class)
    @JsonInclude(Include.NON_NULL)
    public static class RouteStatus {
        private final String name;
        private final ProtocolType protocol;
        private String host;
        private String path;
        private String target;
        private String port;
        private boolean secure;

        public RouteStatus(String name, ProtocolType kind) {
            this.name = name;
            this.protocol = kind;
        }

        public String getName() {
            return name;
        }

        public ProtocolType getProtocol() {
            return protocol;
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

    @JsonProperty(STATUS_LABEL)
    private volatile Status status = Status.NOTFOUND;
    @JsonIgnore
    private volatile PublishConfiguration publishConfiguration;
    @JsonProperty(BUILD_NAME_LABEL)
    private volatile String buildName;
    @JsonProperty(DEPLOYMENT_NAME_LABEL)
    private volatile String deploymentName;
    @JsonProperty(VDB_NAME_LABEL)
    private final String vdbName;
    @JsonProperty(NAMESPACE_LABEL)
    private volatile String namespace;
    @JsonIgnore
    private volatile String publishPodName;
    @JsonProperty(LAST_UPDATED_LABEL)
    private volatile long lastUpdated = 0L;
    @JsonProperty(STATUS_MSG_LABEL)
    private volatile String statusMessage;
    @JsonProperty(ROUTES_LABEL)
	private List<RouteStatus> routes = null;

    public BuildStatus(String vdbName) {
        this.vdbName = vdbName;
    }

    public PublishConfiguration publishConfiguration() {
        return publishConfiguration;
    }

    public void setPublishConfiguration(PublishConfiguration publishConfiguration) {
        this.publishConfiguration = publishConfiguration;
    }

    public String buildName() {
        return buildName;
    }

    public void setBuildName(String buildName) {
        this.buildName = buildName;
    }

    public String deploymentName() {
        return deploymentName;
    }

    public void setDeploymentName(String deploymentName) {
        this.deploymentName = deploymentName;
    }

    public String vdbName() {
        return vdbName;
    }

    public String namespace() {
        return namespace;
    }

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    public String statusMessage() {
        return statusMessage;
    }

    public void setStatusMessage(String statusMessage) {
        this.statusMessage = statusMessage;
    }

    public long lastUpdated() {
        return lastUpdated;
    }

    public void setLastUpdated() {
        this.lastUpdated = System.currentTimeMillis();
    }
    
    public void setPublishPodName(String name) {
    	this.publishPodName = name;
    }
    
    public String publishPodName() {
    	return this.publishPodName;
    }

    public Status status() {
        return status;
    }

    public void setStatus(Status status) {
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
