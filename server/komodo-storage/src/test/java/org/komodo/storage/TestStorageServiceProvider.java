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
package org.komodo.storage;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Properties;
import java.util.Set;
import org.junit.Test;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageConnector.Descriptor;
import org.komodo.spi.storage.StorageConnectorConstants;
import org.komodo.spi.storage.StorageService;
import org.komodo.spi.storage.git.GitStorageConnectorConstants;

public class TestStorageServiceProvider implements StringConstants {

    protected final StorageServiceProvider service = StorageServiceProvider.getInstance();

    @Test
    public void testGetGitStorageConnector() throws Exception {
        String storageType = StorageConnector.Types.GIT.id();
        Set<String> storageTypes = service.getSupportedStorageTypes();
        assertTrue(storageTypes.contains(storageType));

        StorageService storageService = service.getStorageService(storageType);
        assertNotNull(storageService);

        Properties parameters = new Properties();
        parameters.setProperty(GitStorageConnectorConstants.REPO_PATH_PROPERTY, "http://github.com/test/blob.git");
        StorageConnector connector = storageService.getConnector(parameters);
        assertNotNull(connector);

        Set<Descriptor> descriptors = connector.getDescriptors();
        assertNotNull(descriptors);
        assertTrue(descriptors.size() > 0);
    }

    @Test
    public void testGetFileStorageConnector() throws Exception {
        String storageType = StorageConnector.Types.FILE.id();
        Set<String> storageTypes = service.getSupportedStorageTypes();
        assertTrue(storageTypes.contains(storageType));

        StorageService storageService = service.getStorageService(storageType);
        assertNotNull(storageService);

        Properties parameters = new Properties();
        parameters.setProperty(StorageConnectorConstants.FILE_PATH_PROPERTY, "/blah/blah.txt");
        StorageConnector connector = storageService.getConnector(parameters);
        assertNotNull(connector);

        Set<Descriptor> descriptors = connector.getDescriptors();
        assertNotNull(descriptors);
        assertTrue(descriptors.size() > 0);
    }
}
