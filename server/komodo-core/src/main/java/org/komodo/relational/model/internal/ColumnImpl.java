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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.Descriptor;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.Property;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.internal.OptionContainer;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Column;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;

/**
 * An implementation of a relational model column.
 */
public final class ColumnImpl extends RelationalChildRestrictedObject implements Column, OptionContainer {
	
    /**
     * The resolver of a {@link Column}.
     */
    public static final TypeResolver< ColumnImpl > RESOLVER = new TypeResolver< ColumnImpl >() {

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
        public Class< ColumnImpl > owningClass() {
            return ColumnImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( kobject, CreateTable.TABLE_ELEMENT );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public ColumnImpl resolve( final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Column.TYPE_ID ) {
                return ( ColumnImpl )kobject;
            }

            return new ColumnImpl( kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    private enum StandardOption {

        ANNOTATION( null ),
        CASE_SENSITIVE( Boolean.toString( Column.DEFAULT_CASE_SENSITIVE ) ),
        CHAR_OCTET_LENGTH( Long.toString( Column.DEFAULT_CHAR_OCTET_LENGTH ) ),
        CURRENCY( Boolean.toString( Column.DEFAULT_CURRENCY ) ),
        DISTINCT_VALUES( Long.toString( Column.DEFAULT_DISTINCT_VALUES ) ),
        FIXED_LENGTH( Boolean.toString( Column.DEFAULT_FIXED_LENGTH ) ),
        MAX_VALUE( null ),
        MIN_VALUE( null ),
        NAMEINSOURCE( null ),
        NATIVE_TYPE( null ),
        NULL_VALUE_COUNT( Long.toString( Column.DEFAULT_NULL_VALUE_COUNT ) ),
        RADIX( Long.toString( Column.DEFAULT_RADIX ) ),
        SEARCHABLE( Column.Searchable.DEFAULT_VALUE.name() ),
        SELECTABLE( Boolean.toString( Column.DEFAULT_SELECTABLE ) ),
        SIGNED( Boolean.toString( Column.DEFAULT_SIGNED ) ),
        UPDATABLE( Boolean.toString( Column.DEFAULT_UPDATABLE ) ),
        UUID( null );

        private static Map< String, String > _defaultValues = null;

        /**
         * @return an unmodifiable collection of the names and default values of all the standard options (never <code>null</code>
         *         or empty)
         */
        static Map< String, String > defaultValues() {
            if ( _defaultValues == null ) {
                final StandardOption[] options = values();
                final Map< String, String > temp = new HashMap< >();

                for ( final StandardOption option : options ) {
                    temp.put( option.name(), option.defaultValue );
                }

                _defaultValues = Collections.unmodifiableMap( temp );
            }

            return _defaultValues;
        }

        /**
         * @param name
         *        the name being checked (can be <code>null</code>)
         * @return <code>true</code> if the name is the name of a standard option
         */
        static boolean isValid( final String name ) {
            for ( final StandardOption option : values() ) {
                if ( option.name().equals( name ) ) {
                    return true;
                }
            }

            return false;
        }

        private final String defaultValue;

        private StandardOption( final String defaultValue ) {
            this.defaultValue = defaultValue;
        }

    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a column
     */
    public ColumnImpl( final UnitOfWork uow,
                       final Repository repository,
                       final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getCharOctetLength() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.CHAR_OCTET_LENGTH.name() );

        if ( option == null ) {
            return Column.DEFAULT_CHAR_OCTET_LENGTH;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getCollationName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getCollationName() throws KException {
        return getObjectProperty( getTransaction(), PropertyValueType.STRING, "getCollationName", //$NON-NLS-1$
                                  StandardDdlLexicon.COLLATION_NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#getCustomOptions()
     */
    @Override
    public StatementOption[] getCustomOptions() throws KException {
        return OptionContainerUtils.getCustomOptions( getTransaction(), this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDatatypeName() throws KException {
        final String value = getObjectProperty( getTransaction(), PropertyValueType.STRING, "getDatatypeName", //$NON-NLS-1$
                                                StandardDdlLexicon.DATATYPE_NAME );

        if ( StringUtils.isBlank( value ) ) {
            return RelationalConstants.DEFAULT_DATATYPE_NAME;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDefaultValue() throws KException {
        return getObjectProperty( getTransaction(), PropertyValueType.STRING, "getDefaultValue", //$NON-NLS-1$
                                  StandardDdlLexicon.DEFAULT_VALUE );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDescription()
     */
    @Override
    public String getDescription() throws KException {
        return OptionContainerUtils.getOption( getTransaction(), this, StandardOption.ANNOTATION.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getDistinctValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getDistinctValues() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.DISTINCT_VALUES.name() );

        if ( option == null ) {
            return Column.DEFAULT_DISTINCT_VALUES;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getLength() throws KException {
        final Long value = getObjectProperty( getTransaction(), PropertyValueType.LONG, "getLength", //$NON-NLS-1$
                                              StandardDdlLexicon.DATATYPE_LENGTH );

        if ( value == null ) {
            return RelationalConstants.DEFAULT_LENGTH;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getMaxValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMaxValue() throws KException {
        return OptionContainerUtils.getOption( getTransaction(), this, StandardOption.MAX_VALUE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getMinValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMinValue() throws KException {
        return OptionContainerUtils.getOption( getTransaction(), this, StandardOption.MIN_VALUE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNameInSource(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNameInSource() throws KException {
        return OptionContainerUtils.getOption( getTransaction(), this, StandardOption.NAMEINSOURCE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNativeType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNativeType() throws KException {
        return OptionContainerUtils.getOption( getTransaction(), this, StandardOption.NATIVE_TYPE.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNullable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Nullable getNullable() throws KException {
        final String value = getObjectProperty( getTransaction(), PropertyValueType.STRING, "getNullable", //$NON-NLS-1$
                                                StandardDdlLexicon.NULLABLE );

        if ( StringUtils.isBlank( value ) ) {
            return Nullable.DEFAULT_VALUE;
        }

        return Nullable.fromValue( value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getNullValueCount(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getNullValueCount() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.NULL_VALUE_COUNT.name() );

        if ( option == null ) {
            return Column.DEFAULT_NULL_VALUE_COUNT;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getPrecision(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getPrecision() throws KException {
        final Long value = getObjectProperty( getTransaction(), PropertyValueType.LONG, "getPrecision", //$NON-NLS-1$
                                                 StandardDdlLexicon.DATATYPE_PRECISION );

        if ( value == null ) {
            return RelationalConstants.DEFAULT_PRECISION;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getPrimaryType()
     */
    @Override
    public Descriptor getPrimaryType( ) throws KException {
        return OptionContainerUtils.createPrimaryType(getTransaction(), this, super.getPrimaryType( ));
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getProperty(java.lang.String)
     */
    @Override
    public Property getProperty(
                                 final String name ) throws KException {
        return OptionContainerUtils.getProperty( getTransaction(), this, name, super.getProperty( name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getPropertyNames()
     */
    @Override
    public String[] getPropertyNames() throws KException {
        return OptionContainerUtils.getPropertyNames( getTransaction(), this, super.getPropertyNames( ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getRadix(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getRadix() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.RADIX.name() );

        if ( option == null ) {
            return Column.DEFAULT_RADIX;
        }

        return Long.parseLong( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getScale(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public long getScale() throws KException {
        final Long value = getObjectProperty( getTransaction(), PropertyValueType.LONG, "getScale", //$NON-NLS-1$
                                                 StandardDdlLexicon.DATATYPE_SCALE );

        if ( value == null ) {
            return RelationalConstants.DEFAULT_SCALE;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getSearchable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Searchable getSearchable() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.SEARCHABLE.name() );

        if ( option == null ) {
            return Searchable.DEFAULT_VALUE;
        }

        return Searchable.valueOf( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#getStandardOptions()
     */
    @Override
    public Map< String, String > getStandardOptions() {
        return StandardOption.defaultValues();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#getStatementOptionNames()
     */
    @Override
    public String[] getStatementOptionNames() throws KException {
        return OptionContainerUtils.getOptionNames( getTransaction(), this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#getStatementOptions()
     */
    @Override
    public StatementOption[] getStatementOptions() throws KException {
        return OptionContainerUtils.getOptions( getTransaction(), this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getTypeIdentifier()
     */
    @Override
    public KomodoType getTypeIdentifier() {
        return Column.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#getUuid(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getUuid() throws KException {
        return OptionContainerUtils.getOption( getTransaction(), this, StandardOption.UUID.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent()
     */
    @Override
    public TableImpl getParent() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( );
        final TableImpl result = TableImpl.RESOLVER.resolve( parent );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#hasProperties()
     */
    @Override
    public boolean hasProperties() throws KException {
        return OptionContainerUtils.hasProperties( getTransaction(), this, super.hasProperties( ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasProperty(java.lang.String)
     */
    @Override
    public boolean hasProperty(
                                final String name ) throws KException {
        return OptionContainerUtils.hasProperty( getTransaction(), this, name, super.hasProperty( name ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isAutoIncremented() throws KException {
        final Boolean value = getObjectProperty( getTransaction(), PropertyValueType.BOOLEAN, "isAutoIncremented", //$NON-NLS-1$
                                                 CreateTable.AUTO_INCREMENT );

        if ( value == null ) {
            return Column.DEFAULT_AUTO_INCREMENTED;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isCaseSensitive(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isCaseSensitive() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.CASE_SENSITIVE.name() );

        if ( option == null ) {
            return Column.DEFAULT_CASE_SENSITIVE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isCurrency(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isCurrency() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.CURRENCY.name() );

        if ( option == null ) {
            return Column.DEFAULT_CURRENCY;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#isCustomOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean isCustomOption(
                                   final String name ) throws KException {
        return OptionContainerUtils.hasCustomOption( getTransaction(), this, name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isFixedLength(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isFixedLength() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.FIXED_LENGTH.name() );

        if ( option == null ) {
            return Column.DEFAULT_FIXED_LENGTH;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isSelectable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isSelectable() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.SELECTABLE.name() );

        if ( option == null ) {
            return Column.DEFAULT_SELECTABLE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isSigned(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isSigned() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.SIGNED.name() );

        if ( option == null ) {
            return Column.DEFAULT_SIGNED;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#isStandardOption(java.lang.String)
     */
    @Override
    public boolean isStandardOption( final String name ) {
        return StandardOption.isValid( name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#isUpdatable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isUpdatable() throws KException {
        final String option = OptionContainerUtils.getOption( getTransaction(), this, StandardOption.UPDATABLE.name() );

        if ( option == null ) {
            return Column.DEFAULT_UPDATABLE;
        }

        return Boolean.parseBoolean( option );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#removeStatementOption(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void removeStatementOption(
                                       final String optionToRemove ) throws KException {
        OptionContainerUtils.removeOption( getTransaction(), this, optionToRemove );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setAutoIncremented(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setAutoIncremented(
                                    final boolean newAutoIncremented ) throws KException {
        setObjectProperty( getTransaction(), "setAutoIncremented", CreateTable.AUTO_INCREMENT, newAutoIncremented ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCaseSensitive(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setCaseSensitive(
                                  final boolean newCaseSensitive ) throws KException {
        setStatementOption( StandardOption.CASE_SENSITIVE.name(), Boolean.toString( newCaseSensitive ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCharOctetLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setCharOctetLength(
                                    final long newCharOctetLength ) throws KException {
        setStatementOption( StandardOption.CHAR_OCTET_LENGTH.name(), Long.toString( newCharOctetLength ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCollationName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setCollationName(
                                  final String newCollationName ) throws KException {
        setObjectProperty( getTransaction(), "setCollationName", StandardDdlLexicon.COLLATION_NAME, newCollationName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setCurrency(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setCurrency(
                             final boolean newCurrency ) throws KException {
        setStatementOption( StandardOption.CURRENCY.name(), Boolean.toString( newCurrency ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDatatypeName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDatatypeName(
                                 final String newTypeName ) throws KException {
        setObjectProperty( getTransaction(), "setDatatypeName", StandardDdlLexicon.DATATYPE_NAME, newTypeName ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDefaultValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDefaultValue(
                                 final String newDefaultValue ) throws KException {
        setObjectProperty( getTransaction(), "setDefaultValue", StandardDdlLexicon.DEFAULT_VALUE, newDefaultValue ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setDescription(java.lang.String)
     */
    @Override
    public void setDescription(
                                final String newDescription ) throws KException {
        setStatementOption( StandardOption.ANNOTATION.name(), newDescription );
    }

    @Override
    public void setDistinctValues(
                                   final long newDistinctValues ) throws KException {
        setStatementOption( StandardOption.DISTINCT_VALUES.name(), Long.toString( newDistinctValues ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setFixedLength(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setFixedLength(
                                final boolean newFixedLength ) throws KException {
        setStatementOption( StandardOption.FIXED_LENGTH.name(), Boolean.toString( newFixedLength ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setLength(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setLength(
                           final long newLength ) throws KException {
        setObjectProperty( getTransaction(), "setLength", StandardDdlLexicon.DATATYPE_LENGTH, newLength ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setMaxValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMaxValue(
                             final String newMaxValue ) throws KException {
        setStatementOption( StandardOption.MAX_VALUE.name(), newMaxValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setMinValue(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMinValue(
                             final String newMinValue ) throws KException {
        setStatementOption( StandardOption.MIN_VALUE.name(), newMinValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNameInSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNameInSource(
                                 final String newNameInSource ) throws KException {
        setStatementOption( StandardOption.NAMEINSOURCE.name(), newNameInSource );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNativeType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setNativeType(
                               final String newNativeType ) throws KException {
        setStatementOption( StandardOption.NATIVE_TYPE.name(), newNativeType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNullable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.RelationalConstants.Nullable)
     */
    @Override
    public void setNullable(
                             final Nullable newNullable ) throws KException {
        setObjectProperty( getTransaction(), "setNullable", //$NON-NLS-1$
                           StandardDdlLexicon.NULLABLE,
                           ( newNullable == null ) ? Nullable.DEFAULT_VALUE.toValue() : newNullable.toValue() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setNullValueCount(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setNullValueCount(
                                   final long newNullValueCount ) throws KException {
        setStatementOption( StandardOption.NULL_VALUE_COUNT.name(), Long.toString( newNullValueCount ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setPrecision(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setPrecision(
                              final long newPrecision ) throws KException {
        setObjectProperty( getTransaction(), "setPrecision", StandardDdlLexicon.DATATYPE_PRECISION, newPrecision ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#setProperty(java.lang.String, java.lang.Object[])
     */
    @Override
    public void setProperty(
                             final String propertyName,
                             final Object... values ) throws KException {
        // if an option was not set then set a property
        if ( !OptionContainerUtils.setProperty( getTransaction(), this, propertyName, values ) ) {
            super.setProperty( propertyName, values );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setRadix(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setRadix(
                          final long newRadix ) throws KException {
        setStatementOption( StandardOption.RADIX.name(), Long.toString( newRadix ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setScale(org.komodo.spi.repository.Repository.UnitOfWork, long)
     */
    @Override
    public void setScale(
                          final long newScale ) throws KException {
        setObjectProperty( getTransaction(), "setScale", StandardDdlLexicon.DATATYPE_SCALE, newScale ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSearchable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column.Searchable)
     */
    @Override
    public void setSearchable(
                               final Searchable newSearchable ) throws KException {
        final String value = ( ( newSearchable == null ) ? Searchable.DEFAULT_VALUE.toString() : newSearchable.toString() );
        setStatementOption( StandardOption.SEARCHABLE.name(), value );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSelectable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSelectable(
                               final boolean newSelectable ) throws KException {
        setStatementOption( StandardOption.SELECTABLE.name(), Boolean.toString( newSelectable ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setSigned(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setSigned(
                           final boolean newSigned ) throws KException {
        setStatementOption( StandardOption.SIGNED.name(), Boolean.toString( newSigned ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.OptionContainer#setStatementOption(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public StatementOption setStatementOption(
                                               final String optionName,
                                               final String optionValue ) throws KException {
        return OptionContainerUtils.setOption( getTransaction(), this, optionName, optionValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setUpdatable(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setUpdatable(
                              final boolean newUpdatable ) throws KException {
        setStatementOption( StandardOption.UPDATABLE.name(), Boolean.toString( newUpdatable ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Column#setUuid(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setUuid(
                         final String newUuid ) throws KException {
        setStatementOption( StandardOption.UUID.name(), newUuid );
    }
    
    @Override
    public TableImpl getRelationalParent() throws KException {
    	return getParent();
    }
    
    @Override
    public Long getArrayDimensions() throws KException {
    	if (hasRawProperty(getTransaction(), StandardDdlLexicon.DATATYPE_ARRAY_DIMENSIONS)) {
            Property colArrDimsProp = getRawProperty(getTransaction(), StandardDdlLexicon.DATATYPE_ARRAY_DIMENSIONS);
            if (colArrDimsProp != null) {
            	return colArrDimsProp.getLongValue(getTransaction());
            }
    	}
    	return null;
    }

}
