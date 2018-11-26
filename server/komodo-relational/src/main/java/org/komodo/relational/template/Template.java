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
package org.komodo.relational.template;

import java.util.List;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.template.internal.TemplateImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.lexicon.datavirt.DataVirtLexicon;

/**
 * A model of a template instance
 */
public interface Template extends RelationalObject {

    /**
     * Connection factory class property key
     */
    String CONN_FACTORY_CLASS_KEY = "managedconnectionfactory-class";  //$NON-NLS-1$

    /**
     * Class name property key
     */
    String CLASSNAME_KEY = "class-name";  //$NON-NLS-1$

    /**
     * The type identifier.
     */
    int TYPE_ID = Template.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.TEMPLATE;

    /**
     * An empty array of templates.
     */
    Template[] NO_TEMPLATES = new Template[0];

    /**
     * The resolver of a {@link Template}.
     */
    TypeResolver< Template > RESOLVER = new TypeResolver< Template >() {

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
        public Class< TemplateImpl > owningClass() {
            return TemplateImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, DataVirtLexicon.Template.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Template resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Template.TYPE_ID ) {
                return ( Template )kobject;
            }
            return new TemplateImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return id of this template
     * @throws KException
     */
    String getId(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return 'true' if a JDBC source, 'false' if not.
     * @throws KException if error occurs
     */
    boolean isJdbc(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param entryName
     * @return
     * @throws KException
     */
    TemplateEntry addEntry( final UnitOfWork transaction, final String entryName ) throws KException;

     /**
      * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
      * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
      * @return
      * @throws KException
      */
    List<TemplateEntry> getEntries(UnitOfWork transaction, String... namePatterns) throws KException;
}
