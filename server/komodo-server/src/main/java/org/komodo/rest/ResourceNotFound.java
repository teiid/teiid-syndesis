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

package org.komodo.rest;

import org.komodo.utils.ArgCheck;

/**
 * indicates the resource was not found.
 */
public class ResourceNotFound {

    private final String resourceName;

    /**
     * @param resourceName
     *        the name of the resource that was not found (cannot be empty)
     */
    public ResourceNotFound(final String resourceName) {
        super();
        ArgCheck.isNotEmpty(resourceName, "resourceName"); //$NON-NLS-1$

        this.resourceName = resourceName;
    }

    /**
     * @return the resource name (never empty)
     */
    public String getResourceName() {
        return this.resourceName;
    }

}