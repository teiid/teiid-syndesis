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
package org.komodo.core.internal.repository;

import org.modeshape.jcr.JcrRepository;

/**
 *
 */
public class WorkspaceIdentifier {

    private final String workspace;

    private JcrRepository repository;

    /**
     * Create a new instance
     * @param workspace the workspace name
     */
    public WorkspaceIdentifier(String workspace) {
        this.workspace = workspace;
    }

    /**
     * Create a new instance
     * @param workspace the workspace name
     * @param repository the repository
     */
    public WorkspaceIdentifier(String workspace, JcrRepository repository) {
        this.workspace = workspace;
        this.repository = repository;
    }

    /**
     * @return the workspace
     */
    public String getWorkspace() {
        return workspace;
    }

    /**
     * @return the repository
     */
    public JcrRepository getRepository() {
        return repository;
    }

    public void setRepository(JcrRepository repository) {
        this.repository = repository;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.repository == null) ? 0 : this.repository.hashCode());
        result = prime * result + ((this.workspace == null) ? 0 : this.workspace.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        WorkspaceIdentifier other = (WorkspaceIdentifier)obj;
        if (this.repository == null) {
            if (other.repository != null)
                return false;
        } else if (!this.repository.equals(other.repository))
            return false;
        if (this.workspace == null) {
            if (other.workspace != null)
                return false;
        } else if (!this.workspace.equals(other.workspace))
            return false;
        return true;
    }
}
