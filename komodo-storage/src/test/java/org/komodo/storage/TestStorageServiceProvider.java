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
