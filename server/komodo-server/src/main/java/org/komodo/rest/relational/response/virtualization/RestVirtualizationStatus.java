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
package org.komodo.rest.relational.response.virtualization;

import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.komodo.openshift.BuildStatus;
import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.rest.AbstractKEntity;
import org.komodo.spi.KException;
import org.komodo.utils.ArgCheck;

public class RestVirtualizationStatus extends AbstractKEntity {
    
    public static final String VDB_NAME_LABEL = "vdb_name";

    public static final String BUILD_NAME_LABEL = "build_name";

    public static final String DEPLOYMENT_NAME_LABEL = "deployment_name";

    public static final String NAMESPACE_LABEL = "namespace";

    public static final String STATUS_LABEL = "build_status";

    public static final String STATUS_MSG_LABEL = "build_status_message";

    public static final String LAST_UPDATED_LABEL = "last_updated";

    public static final String ROUTES_LABEL = "routes";

    private static SimpleDateFormat sdf = new SimpleDateFormat("M-dd-yyyy hh:mm:ss");

    private List<RestRouteStatus> routes = new ArrayList<RestRouteStatus>();

    public RestVirtualizationStatus() {
    }

    public RestVirtualizationStatus(URI baseUri, BuildStatus status) throws KException {

        ArgCheck.isNotNull(status, "status"); //$NON-NLS-1$

        setVdbName(status.vdbName());
        setBuildName(status.buildName());
        setDeploymentName(status.deploymentName());
        setStatus(status.status().name());
        setStatusMsg(status.statusMessage());
        setNamespace(status.namespace());
        Date date = new Date(status.lastUpdated());
        setLastUpdated(sdf.format(date));

        List<RouteStatus> routeStatuses = status.routes();
        if (routeStatuses != null && routeStatuses.size() > 0) {
            this.routes = new ArrayList<>(routeStatuses.size());
            for (RouteStatus route : routeStatuses) {
                this.routes.add(new RestRouteStatus(route));
            }
        }
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

    public List<RestRouteStatus> getRoutes() {
        return this.routes;
    }

    public void setRoutes(RestRouteStatus[] routes) {
        if (routes == null || routes.length == 0) {
            this.routes = Collections.emptyList();
            return;
        }

        this.routes = new ArrayList<>(routes.length);
        for (RestRouteStatus route : routes) {
            this.routes.add(route);
        }
    }
}