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

import java.util.Set;
import org.komodo.spi.storage.StorageService;
import org.komodo.spi.utils.KeyInValueMap;
import org.komodo.spi.utils.KeyInValueMap.KeyFromValueAdapter;
import org.komodo.storage.file.FileStorageService;
import org.komodo.storage.git.GitStorageService;

public class StorageServiceProvider {

    private class StorageServiceKeyAdapter implements KeyFromValueAdapter<String, StorageService> {

        @Override
        public String getKey(StorageService service) {
            try {
                return service.getStorageId();
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        }
    }

    private static StorageServiceProvider instance;

    public static StorageServiceProvider getInstance() {
        if (instance == null)
            instance = new StorageServiceProvider();
    
        return instance;
    }

    private KeyInValueMap<String, StorageService> storageServices = new KeyInValueMap<>(new StorageServiceKeyAdapter());

    public StorageServiceProvider() {
        GitStorageService gitStorage = new GitStorageService();
        storageServices.add(gitStorage);

        FileStorageService fileStorage = new FileStorageService();
        storageServices.add(fileStorage);
    }

    /**
     * @return the set of supported storage types
     */
    public Set<String> getSupportedStorageTypes() {
        return storageServices.keySet();
    }

    /**
     * @param storageId
     * @return the storage service for the storage id
     * @throws Exception
     */
    public synchronized StorageService getStorageService(String storageId) throws Exception {
        StorageService storageService = storageServices.get(storageId);
        if (storageService != null)
            return storageService;

     
        throw new UnsupportedStorageException(storageId);
    }
}
