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
package org.komodo.relational.resource;

import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.dataservice.DataServiceResource;
import org.komodo.relational.resource.internal.DdlFileImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;

/**
 * Represents a DDL file of a data service.
 */
public interface DdlFile extends DataServiceResource {

    /**
     * The file extension of DDL files.
     */
    DocumentType DOC_TYPE = new DocumentType( ".ddl" ); //$NON-NLS-1$

    /**
     * The type identifier.
     */
    int TYPE_ID = DdlFile.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.DDL_FILE;

    /**
     * An empty array of DDL files.
     */
    DdlFile[] NO_DDLS = new DdlFile[ 0 ];

    /**
     * The resolver of a {@link DdlFile}.
     */
    TypeResolver< DdlFile > RESOLVER = new TypeResolver< DdlFile >() {

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
        public Class< DdlFileImpl > owningClass() {
            return DdlFileImpl.class;
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
                                            DataVirtLexicon.ResourceFile.DDL_FILE_NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public DdlFile resolve( final UnitOfWork transaction,
                                final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == DdlFile.TYPE_ID ) {
                return ( DdlFile )kobject;
            }

            return new DdlFileImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    };

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#getDocumentType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    default DocumentType getDocumentType( final UnitOfWork transaction ) {
        return DOC_TYPE;
    }
}
