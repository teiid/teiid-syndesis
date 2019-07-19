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
package org.komodo.relational.model;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents a relational model.
 */
public interface Model extends Exportable, RelationalObject {

    /**
     * The default value for the <code>metadataType</code> property. Value is {@value} .
     */
    String DEFAULT_METADATA_TYPE = "DDL"; //$NON-NLS-1$

    /**
     * The default value for the <code>is visible</code> property. Value is {@value} .
     */
    boolean DEFAULT_VISIBLE = true;

    /**
     * The type identifier.
     */
    int TYPE_ID = Model.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.MODEL;

    @Override
    Vdb getRelationalParent( ) throws KException;

    /**
     * The type of a model.
     */
    enum Type {

        PHYSICAL,
        VIRTUAL;

        /**
         * The default model type. Value is {@value} .
         */
        public static final Type DEFAULT_VALUE = PHYSICAL;

        public static Type findType(String typeId) {
            for (Type type : Type.values()) {
                if (type.name().equalsIgnoreCase(typeId))
                    return type;
            }

            return null;
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param sourceName
     *        the name of the model source to create (cannot be empty)
     * @return the new model source (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ModelSource addSource( 
                           final String sourceName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param tableName
     *        the name of the table to create (cannot be empty)
     * @return the new table (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Table addTable( 
                    final String tableName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param viewName
     *        the name of the view to create (cannot be empty)
     * @return the new view (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    View addView( 
                  final String viewName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the model definition of this model (can be empty)
     * @throws KException
     *         if error occurs
     */
    String getModelDefinition( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return model type of this model (never <code>null</code>)
     * @throws KException
     *         if error occurs
     * @see Type#DEFAULT_VALUE
     */
    Type getModelType( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the model sources found in this model (can be empty)
     * @throws KException
     *         if an error occurs
     */
    ModelSource[] getSources( 
                              final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the tables found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Table[] getTables( 
                       final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the views found in this model (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    View[] getViews( 
                     final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and its state must be {@link State#NOT_STARTED})
     * @return <code>true</code> if this model is visible
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VISIBLE
     */
    boolean isVisible( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param sourceName
     *        the name of the model source being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeSource( 
                       final String sourceName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param tableName
     *        the name of the table being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeTable( 
                      final String tableName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param viewName
     *        the name of the view being deleted (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeView( 
                     final String viewName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setDescription( 
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param modelDefinition
     *        the model definition, eg. a string of ddl
     * @throws KException
     *         if error occurs
     */
    void setModelDefinition( 
                             final String modelDefinition ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newModelType
     *        the new model type (can be <code>null</code>)
     * @throws KException
     *         if error occurs
     * @see Type#DEFAULT_VALUE
     */
    void setModelType( 
                       final Type newModelType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param newVisible
     *        the new value for the <code>visible</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VISIBLE
     */
    void setVisible( 
                     final boolean newVisible ) throws KException;

    /**
     * @param name
     * @return the view found in this model
     * @throws KException
     *         if an error occurs
     */
	View getView(String name) throws KException;

}
