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
package org.komodo.storage.git;

import java.util.Properties;
import java.util.Set;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageConnector.Descriptor;
import org.komodo.spi.storage.StorageService;

public class GitStorageService implements StorageService {

    public static final String STORAGE_ID = StorageConnector.Types.GIT.id();

    public static final String DESCRIPTION = "Storage of files in a git repository";

    public GitStorageService() {
    }

    @Override
    public String getStorageId() {
        return STORAGE_ID;
    }

    @Override
    public String getDescription() throws Exception {
        return DESCRIPTION;
    }

    @Override
    public Set<Descriptor> getDescriptors() throws Exception {
        return GitStorageConnector.DESCRIPTORS;
    }

    @Override
    public StorageConnector getConnector(Properties parameters) throws Exception {
        return new GitStorageConnector(parameters);
    }
}
