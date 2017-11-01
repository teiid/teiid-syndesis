/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
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

        traverse(getTransaction(), parentObject.getAbsolutePath());

    }
}
