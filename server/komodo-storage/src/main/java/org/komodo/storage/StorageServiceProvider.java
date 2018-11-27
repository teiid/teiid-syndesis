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
