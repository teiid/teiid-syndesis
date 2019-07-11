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

import java.util.Properties;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.core.visitor.DdlNodeVisitor;
import org.komodo.metadata.MetadataInstance;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Schema;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * A named schema fragment
 */
public class SchemaImpl extends RelationalObjectImpl implements Schema {
	
    /**
     * The resolver of a {@link Schema}.
     */
    public static final TypeResolver< Schema > RESOLVER = new TypeResolver< Schema >() {

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
        public Class< SchemaImpl > owningClass() {
            return SchemaImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject, KomodoLexicon.Schema.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Schema resolve( final UnitOfWork transaction,
                               final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Schema.TYPE_ID ) {
                return ( Schema )kobject;
            }

            return new SchemaImpl( transaction, RepositoryImpl.getRepository(transaction), kobject.getAbsolutePath() );
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
    public SchemaImpl( final UnitOfWork uow,
                       final Repository repository,
                       final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork transaction,
                          final Properties properties ) throws KException {
        // Is there a situation where this schema fragment is just Teiid SQL?
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            final StringBuffer result = new StringBuffer();
            MetadataInstance metadata = getRepository().getMetadataInstance();
            final DdlNodeVisitor visitor = new DdlNodeVisitor(metadata.getDataTypeService(), false );
            visitor.visit(transaction, this);
            result.append( visitor.getDdl() );

            return result.toString().getBytes();
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Schema#getRendition(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getRendition( final UnitOfWork uow ) throws KException {
        final String rendition = getObjectProperty( uow, PropertyValueType.STRING, "getRendition", //$NON-NLS-1$
                                                    KomodoLexicon.Schema.RENDITION );

        return rendition == null ? EMPTY_STRING : rendition;
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
     * @see org.komodo.core.repository.ObjectImpl#getTypeIdentifier(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoType getTypeIdentifier( final UnitOfWork uow ) {
        return Schema.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Schema#setRendition(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setRendition( final UnitOfWork uow,
                              final String rendition ) throws KException {
        setObjectProperty( uow, "setRendition", KomodoLexicon.Schema.RENDITION, rendition ); //$NON-NLS-1$
    }

}
