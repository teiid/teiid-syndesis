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

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a VDB model source.
 */
public final class ModelSourceImpl extends RelationalChildRestrictedObject implements ModelSource {
	
    /**
     * The resolver of a {@link ModelSource}.
     */
	public static final TypeResolver< ModelSourceImpl > RESOLVER = new TypeResolver< ModelSourceImpl >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< ModelSourceImpl > owningClass() {
            return ModelSourceImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( kobject, VdbLexicon.Source.SOURCE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public ModelSourceImpl resolve( final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == ModelSource.TYPE_ID ) {
                return ( ModelSourceImpl )kobject;
            }

            return new ModelSourceImpl( kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

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
    public KomodoType getTypeIdentifier( ) {
        return ModelSource.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.ModelSource#getJndiName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJndiName( ) throws KException {
        return getObjectProperty( getTransaction(), PropertyValueType.STRING, "getJndiName", VdbLexicon.Source.JNDI_NAME ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent()
     */
    @Override
    public ModelImpl getParent() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent( );
        final ModelImpl result = ModelImpl.RESOLVER.resolve( grouping.getParent( ) );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.ModelSource#getTranslatorName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getTranslatorName( ) throws KException {
        return getObjectProperty( getTransaction(), PropertyValueType.STRING, "getTranslatorName", VdbLexicon.Source.TRANSLATOR ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getTypeId()
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
    public void setJndiName( final String newJndiName ) throws KException {
        setObjectProperty( getTransaction(), "setJndiName", VdbLexicon.Source.JNDI_NAME, newJndiName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.ModelSource#setTranslatorName(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setTranslatorName( final String newTranslatorName ) throws KException {
        setObjectProperty( getTransaction(), "setTranslatorName", VdbLexicon.Source.TRANSLATOR, newTranslatorName ); //$NON-NLS-1$
    }
    
    @Override
    public ModelImpl getRelationalParent() throws KException {
    	return this.getParent();
    }

}
