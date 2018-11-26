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
package org.komodo.relational.profile;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.profile.internal.ViewDefinitionImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents the configuration of a view definition
 */
public interface ViewDefinition  extends RelationalObject, StringConstants {

    /**
     * The type identifier.
     */
    int TYPE_ID = ViewDefinition.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VIEW_DEFINITION;


    /**
     * The resolver of a {@link ViewDefinition}.
     */
    TypeResolver<ViewDefinition> RESOLVER = new TypeResolver<ViewDefinition>() {

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
        public Class<ViewDefinitionImpl> owningClass() {
            return ViewDefinitionImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(transaction, kobject.getRepository(), kobject, KomodoLexicon.ViewDefinition.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public ViewDefinition resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == ViewDefinition.TYPE_ID) {
                return (ViewDefinition)kobject;
            }

            return new ViewDefinitionImpl(transaction, kobject.getRepository(), kobject.getAbsolutePath());
        }

    };
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param compositionName
     *        the name of the sql composition being added (cannot be empty)
     * @return the new sql composition (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    SqlComposition addSqlComposition( final UnitOfWork transaction, String compositionName ) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the sql compositions (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    SqlComposition[] getSqlCompositions( final UnitOfWork transaction, final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param sqlCompositionToRemove
     *        the name of the sql composition being removed (cannot be empty)       
     * @throws KException
     *         if an error occurs
     */
    void removeSqlComposition( final UnitOfWork transaction,
                         final String sqlCompositionToRemove ) throws KException;
    
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the view name
     * @throws KException
     *         if an error occurs   
     */
    String getViewName(final UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name the view name
     * @throws KException
     *         if an error occurs
     */
    void setViewName(UnitOfWork transaction, String name) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the description
     * @throws KException
     *         if an error occurs
     */
    String getDescription(final UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param description value of description
     * @throws KException
     *         if an error occurs         
     */
    void setDescription(final UnitOfWork transaction, String description) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param complete value for isComplete
     * @throws KException
     *         if an error occurs         
     */
    void setComplete(final UnitOfWork transaction, boolean complete) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return boolean value of isComplete
     * @throws KException
     *         if an error occurs         
     */
    boolean isComplete(final UnitOfWork transaction) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the string array of source paths
     * @throws KException
     *         if an error occurs     
     */
    String[] getSourcePaths(final UnitOfWork transaction,  final String... namePatterns ) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param sourcePathToRemove
     *        the source path being removed (cannot be empty)       
     * @return the source paths
     * @throws KException
     *         if an error occurs
     */
    String[] removeSourcePath( final UnitOfWork transaction,
                                final String sourcePathToRemove ) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param sourcePath 
     *        the name of the source path (cannot be empty)
     * @return the source paths
     * @throws KException
     *         if an error occurs
     */
    String[] addSourcePath( final UnitOfWork transaction, String sourcePath ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param columnName
     *        the name of the projected column being added (cannot be empty)
     * @return the new projected column (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    SqlProjectedColumn addProjectedColumn( final UnitOfWork transaction, String columnName ) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param projectedColumnToRemove
     *        the name of the projected column being removed (cannot be empty)       
     * @throws KException
     *         if an error occurs
     */
    void removeProjectedColumn( final UnitOfWork transaction,
                                final String projectedColumnToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the sql projected columns (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    SqlProjectedColumn[] getProjectedColumns( final UnitOfWork transaction, final String... namePatterns ) throws KException;

}
