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
import org.komodo.core.repository.Property;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.core.repository.RepositoryImpl;
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
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction,
                                            kobject,
                                            DataVirtLexicon.DataService.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.core.repository.KomodoObject)
         */
        @Override
        public DataserviceImpl resolve( final UnitOfWork transaction,
                                    final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Dataservice.TYPE_ID ) {
                return ( DataserviceImpl )kobject;
            }
            return new DataserviceImpl( transaction, RepositoryImpl.getRepository(transaction), kobject.getAbsolutePath() );
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
    public KomodoType getTypeIdentifier(UnitOfWork transaction) {
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
     * @see org.komodo.relational.dataservice.Dataservice#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
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
    public Calendar getLastModified( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
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
    public String getModifiedBy( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getModifiedBy", //$NON-NLS-1$
                                  DataVirtLexicon.DataService.MODIFIED_BY );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setDescription(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setObjectProperty( transaction,
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
    public void setLastModified( final UnitOfWork transaction,
                                 final Calendar newLastModified ) throws KException {
        setObjectProperty( transaction,
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
    public void setModifiedBy( final UnitOfWork transaction,
                               final String newModifiedBy ) throws KException {
        setObjectProperty( transaction,
                           "setModifiedBy", //$NON-NLS-1$
                           DataVirtLexicon.DataService.MODIFIED_BY,
                           newModifiedBy );
    }
    
    @Override
    public String getServiceVdbName(UnitOfWork uow) throws KException {
    	KomodoObject child = this.getChild(uow, "serviceVdb", DataVirtLexicon.VdbEntry.NODE_TYPE);
    	if (child != null) {
    		Property p = child.getProperty(uow, DataVirtLexicon.VdbEntry.VDB_NAME);
    		if (p != null) {
    			return p.getStringValue(uow);
    		}
    	}
    	return null;
    }
    
    @Override
    public void setServiceVdbName(UnitOfWork uow, String name) throws KException {
    	KomodoObject child = this.addChild(uow, "serviceVdb", DataVirtLexicon.VdbEntry.NODE_TYPE);
    	child.setProperty(uow, DataVirtLexicon.VdbEntry.VDB_NAME, name);
    }
   
}
