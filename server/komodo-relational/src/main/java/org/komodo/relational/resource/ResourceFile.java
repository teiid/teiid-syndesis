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
import org.komodo.relational.resource.internal.ResourceFileImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;

/**
 * Represents a resource file of a data service.
 */
public interface ResourceFile extends DataServiceResource {

    /**
     * The type identifier.
     */
    int TYPE_ID = ResourceFile.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.RESOURCE;

    /**
     * An empty array of resource files.
     */
    ResourceFile[] NO_RESOURCES = new ResourceFile[ 0 ];

    /**
     * The resolver of a {@link ResourceFile}.
     */
    TypeResolver< ResourceFile > RESOLVER = new TypeResolver< ResourceFile >() {

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
        public Class< ResourceFileImpl > owningClass() {
            return ResourceFileImpl.class;
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
                                            DataVirtLexicon.ResourceFile.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public ResourceFile resolve( final UnitOfWork transaction,
                                     final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == ResourceFile.TYPE_ID ) {
                return ( ResourceFile )kobject;
            }

            return new ResourceFileImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    };

}
