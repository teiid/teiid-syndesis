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

import java.util.Calendar;

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * Implementation of data service instance model.
 */
public class DataserviceImpl extends RelationalObjectImpl implements Dataservice {
	
    /**
     * An empty collection of VDB entries.
     */
    KomodoObject[] NO_ENTRIES = new KomodoObject[ 0 ];
	
    /**
     * The resolver of a {@link Dataservice}.
     */
    public static final TypeResolver< DataserviceImpl > RESOLVER = new TypeResolver< DataserviceImpl >() {

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
        public Class< DataserviceImpl > owningClass() {
            return DataserviceImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( kobject,
                                            DataVirtLexicon.DataService.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public DataserviceImpl resolve(final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Dataservice.TYPE_ID ) {
                return ( DataserviceImpl )kobject;
            }
            return new DataserviceImpl( kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };


    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public DataserviceImpl(final UnitOfWork transaction,
                      final Repository repository,
                      final String path ) throws KException {
        super(transaction, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier() {
        return Dataservice.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDescription()
     */
    @Override
    public String getDescription( ) throws KException {
        return getObjectProperty( getTransaction(),
                                  PropertyValueType.STRING,
                                  "getDescription", //$NON-NLS-1$
                                  DataVirtLexicon.DataService.DESCRIPTION );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getLastModified(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Calendar getLastModified() throws KException {
        return getObjectProperty( getTransaction(),
                                  PropertyValueType.DATE,
                                  "getLastModified", //$NON-NLS-1$
                                  DataVirtLexicon.DataService.LAST_MODIFIED );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getModifiedBy(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getModifiedBy() throws KException {
        return getObjectProperty( getTransaction(),
                                  PropertyValueType.STRING,
                                  "getModifiedBy", //$NON-NLS-1$
                                  DataVirtLexicon.DataService.MODIFIED_BY );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setDescription(java.lang.String)
     */
    @Override
    public void setDescription(
                                final String newDescription ) throws KException {
        setObjectProperty( getTransaction(),
                           "setDescription", //$NON-NLS-1$
                           DataVirtLexicon.DataService.DESCRIPTION,
                           newDescription );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setLastModified(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.util.Calendar)
     */
    @Override
    public void setLastModified(
                                 final Calendar newLastModified ) throws KException {
        setObjectProperty( getTransaction(),
                           "setLastModified", //$NON-NLS-1$
                           DataVirtLexicon.DataService.LAST_MODIFIED,
                           newLastModified );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setModifiedBy(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setModifiedBy(
                               final String newModifiedBy ) throws KException {
        setObjectProperty( getTransaction(),
                           "setModifiedBy", //$NON-NLS-1$
                           DataVirtLexicon.DataService.MODIFIED_BY,
                           newModifiedBy );
    }
    
}
