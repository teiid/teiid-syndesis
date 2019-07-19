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
import org.komodo.core.repository.Descriptor;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.vdb.Translator;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a VDB translator.
 */
public final class TranslatorImpl extends RelationalChildRestrictedObject implements Translator {
	
    /**
     * The resolver of a {@link Translator}.
     */
	public static final TypeResolver< TranslatorImpl > RESOLVER = new TypeResolver< TranslatorImpl >() {

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
        public Class< TranslatorImpl > owningClass() {
            return TranslatorImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( kobject, VdbLexicon.Translator.TRANSLATOR );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public TranslatorImpl resolve( final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Translator.TYPE_ID ) {
                return ( TranslatorImpl )kobject;
            }

            return new TranslatorImpl( kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath() );
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
    public TranslatorImpl( final UnitOfWork uow,
                           final Repository repository,
                           final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier() {
        return Translator.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Translator#getDescription()
     */
    @Override
    public String getDescription( ) throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.STRING, "getDescription", VdbLexicon.Translator.DESCRIPTION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent()
     */
    @Override
    public KomodoObject getParent() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject parent = super.getParent();
        Descriptor parentType = parent.getPrimaryType();

        if (VdbLexicon.Vdb.TRANSLATORS.equals(parentType.getName())) {
            //
            // Ignore the translators grouping node
            // and return the parent vdb
            //
            return parent.getParent();
        }

        //
        // Otherwise, eg. translator is below a CachedTeiid
        // return the parent as is.
        //
        return parent;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Translator#getType()
     */
    @Override
    public String getType( ) throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.STRING, "getType", VdbLexicon.Translator.TYPE); //$NON-NLS-1$
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
     * @see org.komodo.relational.vdb.Translator#setDescription(java.lang.String)
     */
    @Override
    public void setDescription( final String newDescription ) throws KException {
        setObjectProperty(getTransaction(), "setDescription", VdbLexicon.Translator.DESCRIPTION, newDescription); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Translator#setType(java.lang.String)
     */
    @Override
    public void setType( final String newType ) throws KException {
        ArgCheck.isNotEmpty(newType, "newType"); //$NON-NLS-1$
        setObjectProperty(getTransaction(), "setType", VdbLexicon.Translator.TYPE, newType); //$NON-NLS-1$
    }

}
