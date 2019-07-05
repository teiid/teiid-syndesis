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
package org.komodo.relational.dataservice;

import java.util.Calendar;

import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A model of a dataservice instance
 */
public interface Dataservice extends Exportable, RelationalObject, VdbEntryContainer {

    /**
     * The type identifier.
     */
    int TYPE_ID = Dataservice.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.DATASERVICE;

    /**
     * An empty array of Dataservices.
     */
    Dataservice[] NO_DATASERVICES = new Dataservice[ 0 ];

    /**
     * The resolver of a {@link Dataservice}.
     */
    TypeResolver< Dataservice > RESOLVER = new TypeResolver< Dataservice >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< DataserviceImpl > owningClass() {
            return DataserviceImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction,
                                            kobject.getRepository(),
                                            kobject,
                                            DataVirtLexicon.DataService.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Dataservice resolve( final UnitOfWork transaction,
                                    final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Dataservice.TYPE_ID ) {
                return ( Dataservice )kobject;
            }
            return new DataserviceImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param vdb
     *        the VDB being added (cannot be <code>null</code>)
     * @return the VDB entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addVdb( final UnitOfWork transaction,
                     final Vdb vdb ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param vdbEntryName
     *        the name of the VDB entry to create (cannot be empty)
     * @return the VDB entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addVdbEntry( final UnitOfWork transaction,
                          final String vdbEntryName ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param serviceVdb
     *        the service VDB being set (can be <code>null</code> when deleting current value)
     * @return the service VDB being replaced or <code>null</code> if one is not being replaced
     * @throws KException
     *         if an error occurs
     */
    Vdb setServiceVdb( final UnitOfWork uow,
                       final Vdb serviceVdb ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the service VDB (may be <code>null</code> if not defined)
     * @throws KException
     *         if an error occurs
     */
    Vdb getServiceVdb( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the service VDB entry (may be <code>null</code> if not defined)
     * @throws KException
     *         if an error occurs
     */
    ServiceVdbEntry getServiceVdbEntry( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the names of the ViewDefinitions for the dataservice (may be empty if not found)
     * @throws KException
     *         if an error occurs
     */
    String[] getViewDefinitionNames( UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the name of the dataservice view model (may be <code>null</code> if not found)
     * @throws KException
     *         if an error occurs
     */
    String getServiceViewModelName( UnitOfWork uow ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    DataServiceEntry< ? > getChild( final UnitOfWork transaction,
                                    final String name ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    DataServiceEntry< ? > getChild( final UnitOfWork transaction,
                                    final String name,
                                    final String typeName ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    DataServiceEntry< ? >[] getChildren( final UnitOfWork transaction,
                                         final String... namePatterns ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    DataServiceEntry< ? >[] getChildrenOfType( final UnitOfWork transaction,
                                               final String type,
                                               final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the last time the manifest was modified (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Calendar getLastModified( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the name of the user who last modified the data service (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getModifiedBy( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDB entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry[] getVdbEntries( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newLastModified
     *        the new value of the <code>last modified date</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLastModified( final UnitOfWork transaction,
                          final Calendar newLastModified ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newModifiedBy
     *        the new value of the <code>modified by</code> property
     * @throws KException
     *         if an error occurs
     */
    void setModifiedBy( final UnitOfWork transaction,
                        final String newModifiedBy ) throws KException;

}
