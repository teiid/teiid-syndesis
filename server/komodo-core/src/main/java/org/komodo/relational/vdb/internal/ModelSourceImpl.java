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
package org.komodo.relational.vdb.internal;

import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a VDB model source.
 */
public final class ModelSourceImpl extends RelationalChildRestrictedObject implements ModelSource {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ModelSourceImpl( final UnitOfWork uow,
                            final Repository repository,
                            final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    @Override
    public KomodoType getTypeIdentifier( UnitOfWork uow ) {
        return ModelSource.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.ModelSource#getJndiName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJndiName( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getJndiName", VdbLexicon.Source.JNDI_NAME ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Model getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent( transaction );
        final Model result = Model.RESOLVER.resolve( transaction, grouping.getParent( transaction ) );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.ModelSource#getTranslatorName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getTranslatorName( final UnitOfWork uow ) throws KException {
        return getObjectProperty( uow, PropertyValueType.STRING, "getTranslatorName", VdbLexicon.Source.TRANSLATOR ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.ModelSource#setJndiName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setJndiName( final UnitOfWork uow,
                             final String newJndiName ) throws KException {
        setObjectProperty( uow, "setJndiName", VdbLexicon.Source.JNDI_NAME, newJndiName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.ModelSource#setTranslatorName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setTranslatorName( final UnitOfWork uow,
                                   final String newTranslatorName ) throws KException {
        setObjectProperty( uow, "setTranslatorName", VdbLexicon.Source.TRANSLATOR, newTranslatorName ); //$NON-NLS-1$
    }

}
