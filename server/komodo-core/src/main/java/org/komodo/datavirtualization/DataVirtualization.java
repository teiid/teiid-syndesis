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

package org.komodo.datavirtualization;

import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;

import org.komodo.StringConstants;

@Entity
@DiscriminatorValue("v")
public class DataVirtualization extends BaseDataVirtualization {

    /**
     * Get the preview vdb name for the virtualization name -
     * the suffix is added to not conflict with the source and
     * main preview vdbs
     * @param name
     * @return
     */
    public static String getPreviewVdbName(String name) {
        return name + StringConstants.SERVICE_VDB_SUFFIX;
    }

    /**
     * Get the OpenShift name, requires lower case and must start/end with
     * alpha - which we have already validated
     * @param name
     * @return
     */
    public static String getOpenShiftName(String name) {
        return "dv-" + name.toLowerCase(); //$NON-NLS-1$
    }

    private String description;

    protected DataVirtualization() {
    }

    public DataVirtualization(String name) {
        setName(name);
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

}
