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
package org.komodo.spi.repository;

/**
 * The artifact descriptor.
 */
public interface ArtifactDescriptor {

    /**
     * An empty array of artifact descriptors.
     */
    ArtifactDescriptor[] EMPTY = new ArtifactDescriptor[0];

    /**
     * @return the type of the artifact (never empty)
     */
    String getArtifactType();

    /**
     * @return the artifact description (never empty)
     */
    String getDescription();

    /**
     * @return the artifact path in the repository library (never empty)
     */
    String getPath();

    /**
     * @return the repository where the artifact is located (never <code>null</code>)
     */
    Repository getRepository();

    /**
     * @return the version of the artifact (never empty)
     */
    String getVersion();

    /**
     * @return <code>true</code> if the artifact cannot be modified
     */
    boolean isReadOnly();

}
