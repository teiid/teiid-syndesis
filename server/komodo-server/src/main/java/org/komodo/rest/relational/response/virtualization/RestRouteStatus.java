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

import org.komodo.openshift.BuildStatus.RouteStatus;
import org.komodo.rest.AbstractKEntity;

public class RestRouteStatus extends AbstractKEntity {

    /**
     * Label for the name of the route
     */
    public static final String NAME_LABEL = "name";

    /**
     * Label for the kind of the route
     */
    public static final String KIND_LABEL = "protocol";

    /**
     * Label for the target of the route
     */
    public static final String TARGET_LABEL = "target";

    /**
     * Label for the secure flag of the route
     */
    public static final String SECURE_LABEL = "secure";

    /**
     * Label for the host of the route
     */
    public static final String HOST_LABEL = "host";

    /**
     * Label for the path of the route
     */
    public static final String PATH_LABEL = "path";

    /**
     * Label for the port of the route
     */
    public static final String PORT_LABEL = "port";

    public RestRouteStatus() {
    }

    public RestRouteStatus(RouteStatus route) {
        setName(route.getName());
        setKind(route.getKind().id());
        setTarget(route.getTarget());
        setPath(route.getPath());
        setPort(route.getPort());
        setHost(route.getHost());
        setSecure(route.isSecure());
    }

    public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setName(String name) {
        addTuple(NAME_LABEL, name);
    }

    public String getKind() {
        Object name = tuples.get(KIND_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setKind(String kind) {
        addTuple(KIND_LABEL, kind);
    }

    public String getHost() {
        Object name = tuples.get(HOST_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setHost(String host) {
        addTuple(HOST_LABEL, host);
    }

    public String getPort() {
        Object name = tuples.get(PORT_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setPort(String port) {
        addTuple(PORT_LABEL, port);
    }

    public String getPath() {
        Object name = tuples.get(PATH_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setPath(String path) {
        addTuple(PATH_LABEL, path);
    }

    public String getTarget() {
        Object name = tuples.get(TARGET_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setTarget(String target) {
        addTuple(TARGET_LABEL, target);
    }

    public boolean isSecure() {
        Object value = tuples.get(SECURE_LABEL);
        if (value == null)
            return false;

        return Boolean.parseBoolean(value.toString());
    }

    public void setSecure(boolean secure) {
        addTuple(SECURE_LABEL, secure);
    }
}
