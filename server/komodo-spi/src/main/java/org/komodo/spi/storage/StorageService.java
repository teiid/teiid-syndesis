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
package org.komodo.spi.storage;

import java.util.Properties;
import java.util.Set;
import org.komodo.spi.storage.StorageConnector.Descriptor;

public interface StorageService {

    /**
     * @return the storage Id for this service
     * @throws Exception 
     */
    String getStorageId() throws Exception;

    /**
     * @return the description of this service
     * @throws Exception
     */
    String getDescription() throws Exception;

    /**
     * @return the set of applicable parameters for this storage connector
     * @throws Exception
     */
    Set<Descriptor> getDescriptors() throws Exception;

    /**
     * @param parameters
     * @return an instance of the {@link StorageConnector}
     * @throws Exception
     */
    StorageConnector getConnector(Properties parameters) throws Exception;
}
