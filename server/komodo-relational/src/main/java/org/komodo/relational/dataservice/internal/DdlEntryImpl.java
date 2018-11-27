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
package org.komodo.relational.dataservice.internal;

import org.komodo.relational.Messages;
import org.komodo.relational.dataservice.DdlEntry;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.resource.DdlFile;
import org.komodo.relational.resource.internal.DdlFileImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;

/**
 * An implementation of a DDL entry in a data service.
 */
public class DdlEntryImpl extends RelationalObjectImpl implements DdlEntry {

    private static final String ARCHIVE_FOLDER = "metadata/"; //$NON-NLS-1$

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a DDL entry
     */
    public DdlEntryImpl( final UnitOfWork uow,
                         final Repository repository,
                         final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.DataServiceResourceEntry#getArchiveFolder()
     */
    @Override
    public String getArchiveFolder() {
        return ARCHIVE_FOLDER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.DataServiceEntry#getReference(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public DdlFile getReference( final UnitOfWork uow ) throws KException {
        if ( hasProperty( uow, DataVirtLexicon.ConnectionEntry.CONNECTION_REF ) ) {
            final String refId = getProperty( uow, DataVirtLexicon.ConnectionEntry.CONNECTION_REF ).getStringValue( uow );
            final KomodoObject kobj = getRepository().getUsingId( uow, refId );

            if ( kobj == null ) {
                throw new KException( Messages.getString( Messages.Relational.REFERENCED_RESOURCE_NOT_FOUND,
                                                          DataVirtLexicon.ResourceFile.DDL_FILE_NODE_TYPE,
                                                          refId ) );
            }

            return new DdlFileImpl( uow, getRepository(), kobj.getAbsolutePath() );
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.DataServiceResourceEntry#getResource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public DdlFile getResource( final UnitOfWork uow ) throws KException {
        return getReference( uow );
    }

}
