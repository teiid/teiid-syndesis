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

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents a relational model column.
 */
public interface Column extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Column.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.COLUMN;

    /**
     * The values for a column's searchable property.
     */
    public enum Searchable {

        /**
         * The column is searchable only when NOT using LIKE.
         */
        ALL_EXCEPT_LIKE,

        /**
         * The column is searchable only when using LIKE.
         */
        LIKE_ONLY,

        /**
         * The column is searchable.
         */
        SEARCHABLE,

        /**
         * The column is not searchable.
         */
        UNSEARCHABLE;

        /**
         * The default value for the searchable property. Value is {@value} .
         */
        public static final Searchable DEFAULT_VALUE = SEARCHABLE;

    }

    /**
     * The default value for the <code>auto-incremented</code> property. Value is {@value} .
     */
    boolean DEFAULT_AUTO_INCREMENTED = false;

    /**
     * The default value for the <code>case-sensitive</code> property. Value is {@value} .
     */
    boolean DEFAULT_CASE_SENSITIVE = false;

    /**
     * The default value for the <code>character octet length</code> property. Value is {@value} .
     */
    long DEFAULT_CHAR_OCTET_LENGTH = 0;

    /**
     * The default value for the <code>currency</code> property. Value is {@value} .
     */
    boolean DEFAULT_CURRENCY = false;

    /**
     * The default value for the <code>distinct values</code> property. Value is {@value} .
     */
    long DEFAULT_DISTINCT_VALUES = -1;

    /**
     * The default value for the <code>fixed length</code> property. Value is {@value} .
     */
    boolean DEFAULT_FIXED_LENGTH = false;

    /**
     * The default value for the <code>null value count</code> property. Value is {@value} .
     */
    long DEFAULT_NULL_VALUE_COUNT = -1;

    /**
     * The default value for the <code>datatype radix</code> property. Value is {@value} .
     */
    long DEFAULT_RADIX = 0;

    /**
     * The default value indicating if this column is selectable. Value is {@value} .
     */
    boolean DEFAULT_SELECTABLE = true;

    /**
     * The default value for the <code>signed</code> property. Value is {@value} .
     */
    boolean DEFAULT_SIGNED = false;

    /**
     * The default value indicating if this column is updatable. Value is {@value} .
     */
    boolean DEFAULT_UPDATABLE = true;

    @Override
    Table getRelationalParent( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>char octet length</code> property
     * @throws KException
     *         if an error occurs
     */
    long getCharOctetLength( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getCollationName( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_DATATYPE_NAME
     */
    String getDatatypeName( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDefaultValue( ) throws KException;

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
     * @return the number of distinct values
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DISTINCT_VALUES
     */
    long getDistinctValues( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    long getLength( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>maximum value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getMaxValue( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>minimum value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getMinValue( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>name in source</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNameInSource( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>native type</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNativeType( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>nullable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    Nullable getNullable( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>null value count</code> property
     * @throws KException
     *         if an error occurs
     */
    long getNullValueCount( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    long getPrecision( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype radix</code> property
     * @throws KException
     *         if an error occurs
     */
    long getRadix( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    long getScale( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>searchable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Searchable#DEFAULT_VALUE
     */
    Searchable getSearchable( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>UUID</code> option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getUuid( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if auto-incremented
     * @throws KException
     *         if an error occurs
     * @see Column#DEFAULT_AUTO_INCREMENTED
     */
    boolean isAutoIncremented( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if case-sensitive
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CASE_SENSITIVE
     */
    boolean isCaseSensitive( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column holds a currency value
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CURRENCY
     */
    boolean isCurrency( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column has a fixed length
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_FIXED_LENGTH
     */
    boolean isFixedLength( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column is selectable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SELECTABLE
     */
    boolean isSelectable( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column's value is signed
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SIGNED
     */
    boolean isSigned( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column is updatable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    boolean isUpdatable( ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAutoIncremented
     *        the new value for the <code>auto-incremented</code> property
     * @throws KException
     *         if an error occurs
     * @see Column#DEFAULT_AUTO_INCREMENTED
     */
    void setAutoIncremented( 
                             final boolean newAutoIncremented ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCaseSensitive
     *        the new value for the <code>case-sensitive</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CASE_SENSITIVE
     */
    void setCaseSensitive( 
                           final boolean newCaseSensitive ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCharOctetLength
     *        the new value of the <code>char octet length</code> property
     * @throws KException
     *         if an error occurs
     */
    void setCharOctetLength( 
                             final long newCharOctetLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCollationName
     *        the new value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setCollationName( 
                           final String newCollationName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCurrency
     *        the new value for the <code>currency</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CURRENCY
     */
    void setCurrency( 
                      final boolean newCurrency ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newTypeName
     *        the new value of the <code>datatype name</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_DATATYPE_NAME
     */
    void setDatatypeName( 
                          final String newTypeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDefaultValue
     *        the new value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setDefaultValue( 
                          final String newDefaultValue ) throws KException;

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
     * @param newDistinctValues
     *        the new value for the <code>distinct values</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DISTINCT_VALUES
     */
    void setDistinctValues( 
                            final long newDistinctValues ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newFixedLength
     *        the new value for the <code>fixed length</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_FIXED_LENGTH
     */
    void setFixedLength( 
                         final boolean newFixedLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newLength
     *        the new value of the <code>datatype length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    void setLength( 
                    final long newLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newMaxValue
     *        the new maximum value (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setMaxValue( 
                      final String newMaxValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newMinValue
     *        the new minimum value (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setMinValue( 
                      final String newMinValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNameInSource
     *        the new name in source (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNameInSource( 
                          final String newNameInSource ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNativeType
     *        the new value of the <code>native type</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNativeType( 
                        final String newNativeType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNullable
     *        the new value of the <code>nullable</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    void setNullable( 
                      final Nullable newNullable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNullValueCount
     *        the new value of the <code>null value count</code> property
     * @throws KException
     *         if an error occurs
     */
    void setNullValueCount( 
                            final long newNullValueCount ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPrecision
     *        the new value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    void setPrecision( 
                       final long newPrecision ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newRadix
     *        the new value of the <code>datatype radix</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRadix( 
                   final long newRadix ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newScale
     *        the new value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    void setScale( 
                   final long newScale ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newSearchable
     *        the new value of the <code>searchable</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Searchable#DEFAULT_VALUE
     */
    void setSearchable( 
                        final Searchable newSearchable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newSelectable
     *        the new value for the <code>selectable</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SELECTABLE
     */
    void setSelectable( 
                        final boolean newSelectable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newSigned
     *        the new value for the <code>signed</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SIGNED
     */
    void setSigned( 
                    final boolean newSigned ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newUpdatable
     *        the new value for the <code>updatable</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    void setUpdatable( 
                       final boolean newUpdatable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newUuid
     *        the new value of the <code>UUID</code> option (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setUuid( 
                  final String newUuid ) throws KException;

    /**
     * Get the number or array dimensions or null if this is not an array type
     * @return
     * @throws KException
     */
	Long getArrayDimensions() throws KException;

}
