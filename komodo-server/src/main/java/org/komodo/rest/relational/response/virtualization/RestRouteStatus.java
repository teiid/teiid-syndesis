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

import org.komodo.rest.AbstractKEntity;
import org.komodo.servicecatalog.BuildStatus.RouteStatus;

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
