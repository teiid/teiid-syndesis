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
package org.komodo.relational.model.internal;

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;

/**
 * An implementation of a relational model view.
 */
public final class ViewImpl extends TableImpl implements View {
	
    /**
     * The resolver of a {@link View}.
     */
    public static final TypeResolver< ViewImpl > RESOLVER = new TypeResolver< ViewImpl >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return View.IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< ViewImpl > owningClass() {
            return ViewImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject, CreateTable.VIEW_STATEMENT );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public ViewImpl resolve( final UnitOfWork transaction,
                             final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == View.TYPE_ID ) {
                return ( ViewImpl )kobject;
            }

            return new ViewImpl( transaction, RepositoryImpl.getRepository(transaction), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ViewImpl( final UnitOfWork uow,
                     final Repository repository,
                     final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return View.IDENTIFIER;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ModelImpl getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( transaction );
        final ModelImpl result = ModelImpl.RESOLVER.resolve( transaction, parent );
        return result;
    }

}
