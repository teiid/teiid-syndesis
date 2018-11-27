/*************************************************************************************
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
package org.komodo.relational;

import static org.junit.Assert.assertNotNull;
import java.io.File;
import java.io.InputStream;
import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.KLog;


/**
 * AbstractImporterTest
 */
@SuppressWarnings( {"nls"} )
public abstract class AbstractImporterTest extends AbstractLocalRepositoryTest {

    protected static final String VDB_DIRECTORY = "vdb";

    protected static final String BOOKS_DIRECTORY = "books";

    protected static final String DDL_DIRECTORY = "ddl";

    protected static final String CONNECTION_DIRECTORY = "connection";

    protected abstract void runImporter(Repository repository,
                                                InputStream inputStream,
                                                KomodoObject parentObject,
                                                ImportOptions importOptions,
                                                ImportMessages importMessages) throws Exception;

    protected abstract void runImporter(Repository repository,
                                                File file,
                                                KomodoObject parentObject,
                                                ImportOptions importOptions,
                                                ImportMessages importMessages) throws Exception;

    protected void runImporter(Repository repository, Object content,
                                                                 KomodoObject parentObject, ImportOptions importOptions,
                                                                 ImportMessages importMessages) throws Exception {
        if (content instanceof File)
            runImporter(repository, (File) content, parentObject, importOptions, importMessages);
        else if (content instanceof InputStream)
            runImporter(repository, (InputStream) content, parentObject, importOptions, importMessages);

    }

    protected void executeImporter(Object content, KomodoObject parentObject,
                                                                        ImportOptions importOptions,
                                                                        ImportMessages importMessages)
                                                                        throws Exception {
        assertNotNull(_repo);
        assertNotNull(content);
        assertNotNull(parentObject);
        assertNotNull(importOptions);
        assertNotNull(importMessages);

        runImporter(_repo, content, parentObject, importOptions, importMessages);

        if (importMessages.hasError()) {
            KLog.getLogger().debug(importMessages.errorMessagesToString());
        }

//        traverse(getTransaction(), parentObject.getAbsolutePath());

    }
}
