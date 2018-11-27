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
import org.komodo.spi.repository.DocumentType;

public class StorageReference {

    /**
     * property key for supplied driver name
     */
    public static final String DRIVER_NAME_KEY = "driverName";  //$NON-NLS-1$
    /**
     * default name for driver
     */
    public static final String DRIVER_NAME_DEFAULT = "defaultDriver";  //$NON-NLS-1$

    private final String storageType;

    private final Properties parameters;

    private DocumentType docType;

    public StorageReference(String storageType, Properties parameters, DocumentType documentType) {
        this.storageType = storageType;
        this.parameters = parameters;
        this.docType = documentType;
    }

    /**
     * @return the type of storage in which this reference is located
     */
    public String getStorageType() {
        return storageType;
    }

    /**
     * @return the parameters used to access the storage
     *                  in which this reference is located
     */
    public Properties getParameters() {
        return parameters;
    }

    /**
     * @return the relative reference to the exact location of this reference
     *                  within the storage
     */
    public String getRelativeRef() {
        return parameters.getProperty(StorageConnector.FILE_PATH_PROPERTY);
    }

    /**
     * @return the document type of this reference
     */
    public DocumentType getDocumentType() {
        return docType == null ? DocumentType.UNKNOWN : docType;
    }
}
