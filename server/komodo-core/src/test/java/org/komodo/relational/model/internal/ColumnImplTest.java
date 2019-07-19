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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.PropertyDescriptor;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Column.Searchable;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private ColumnImpl column;
    private TableImpl table;

    @Before
    public void init() throws Exception {
        this.table = createTable();
        this.column = this.table.addColumn( NAME );
//        commit();
    }

    @Test
    public void shouldAllowEmptyDescription() throws Exception {
        this.column.setDescription( "blah" );
        this.column.setDescription( StringConstants.EMPTY_STRING );
        assertThat( this.column.getDescription( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMaxValue() throws Exception {
        this.column.setMaxValue( "blah" );
        this.column.setMaxValue( StringConstants.EMPTY_STRING );
        assertThat( this.column.getMaxValue( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyMinValue() throws Exception {
        this.column.setMinValue( "blah" );
        this.column.setMinValue( StringConstants.EMPTY_STRING );
        assertThat( this.column.getMinValue( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNameInSource() throws Exception {
        this.column.setNameInSource( "blah" );
        this.column.setNameInSource( StringConstants.EMPTY_STRING );
        assertThat( this.column.getNameInSource( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNativeType() throws Exception {
        this.column.setNativeType( "blah" );
        this.column.setNativeType( StringConstants.EMPTY_STRING );
        assertThat( this.column.getNativeType( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescription() throws Exception {
        this.column.setDescription( "blah" );
        this.column.setDescription( null );
        assertThat( this.column.getDescription( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMaxValue() throws Exception {
        this.column.setMaxValue( "blah" );
        this.column.setMaxValue( null );
        assertThat( this.column.getMaxValue( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullMinValue() throws Exception {
        this.column.setMinValue( "blah" );
        this.column.setMinValue( null );
        assertThat( this.column.getMinValue( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNameInSource() throws Exception {
        this.column.setNameInSource( "blah" );
        this.column.setNameInSource( null );
        assertThat( this.column.getNameInSource( ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNativeType() throws Exception {
        this.column.setNativeType( "blah" );
        this.column.setNativeType( null );
        assertThat( this.column.getNativeType( ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.column.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotColumn() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ColumnImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( null );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.column.getName( ), is( NAME ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.column.getTypeIdentifier( ), is(KomodoType.COLUMN));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getLength( ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDatatypeName( ), is( RelationalConstants.DEFAULT_DATATYPE_NAME ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.DATATYPE_NAME ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getPrecision( ), is( RelationalConstants.DEFAULT_PRECISION ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.DATATYPE_PRECISION ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getScale( ), is( RelationalConstants.DEFAULT_SCALE ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.DATATYPE_SCALE ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultAutoIncrementPropertyValueAfterConstruction() throws Exception {
        assertThat( this.column.isAutoIncremented( ), is( Column.DEFAULT_AUTO_INCREMENTED ) );
        assertThat( this.column.hasProperty( TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultCaseSensitiveAfterConstruction() throws Exception {
        assertThat( this.column.isCaseSensitive(), is( Column.DEFAULT_CASE_SENSITIVE ) );
    }

    @Test
    public void shouldHaveDefaultCharOctetLengthAfterConstruction() throws Exception {
        assertThat( this.column.getCharOctetLength( ), is( Column.DEFAULT_CHAR_OCTET_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultCurrencyAfterConstruction() throws Exception {
        assertThat( this.column.isCurrency(), is( Column.DEFAULT_CURRENCY ) );
    }

    @Test
    public void shouldHaveDefaultDistinctValuesAfterConstruction() throws Exception {
        assertThat( this.column.getDistinctValues( ), is( Column.DEFAULT_DISTINCT_VALUES ) );
    }

    @Test
    public void shouldHaveDefaultFixedLengthAfterConstruction() throws Exception {
        assertThat( this.column.isFixedLength(), is( Column.DEFAULT_FIXED_LENGTH ) );
    }

    @Test
    public void shouldHaveDefaultNullValueCountAfterConstruction() throws Exception {
        assertThat( this.column.getNullValueCount( ), is( Column.DEFAULT_NULL_VALUE_COUNT ) );
    }

    @Test
    public void shouldHaveDefaultRadixAfterConstruction() throws Exception {
        assertThat( this.column.getRadix( ), is( Column.DEFAULT_RADIX ) );
    }

    @Test
    public void shouldHaveDefaultSearchableAfterConstruction() throws Exception {
        assertThat( this.column.getSearchable( ), is( Searchable.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldHaveDefaultSelectableAfterConstruction() throws Exception {
        assertThat( this.column.isSelectable(), is( Column.DEFAULT_SELECTABLE ) );
    }

    @Test
    public void shouldHaveDefaultSignedAfterConstruction() throws Exception {
        assertThat( this.column.isSigned(), is( Column.DEFAULT_SIGNED ) );
    }

    @Test
    public void shouldHaveDefaultUpdatableAfterConstruction() throws Exception {
        assertThat( this.column.isUpdatable(), is( Column.DEFAULT_UPDATABLE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( );
        final String[] rawProps = this.column.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.column.getNullable( ), is( RelationalConstants.Nullable.DEFAULT_VALUE ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.NULLABLE ).getStringValue( getTransaction() ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveParentTable() throws Exception {
        assertThat( this.column.getParent( ), is( instanceOf( Table.class ) ) );
        assertThat( this.column.getParent( ), is( ( KomodoObject )this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.column.addChild( getTransaction(), "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( );
        final Filter[] filters = ((ColumnImpl)this.column).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.column.setCaseSensitive( true );
        this.column.setStatementOption( "sledge", "hammer" );
        assertThat( this.column.getChildren( ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveCollationNameAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( StandardDdlLexicon.COLLATION_NAME ), is( false ) );
        assertThat( this.column.getCollationName( ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDefaultValue( ), is( nullValue() ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithEmptyString() throws Exception {
        this.column.setCollationName( "collationName" );
        this.column.setCollationName( StringConstants.EMPTY_STRING );
        assertThat( this.column.getCollationName( ), is( nullValue() ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveCollationNameWithNull() throws Exception {
        this.column.setCollationName( "collationName" );
        this.column.setCollationName( null );
        assertThat( this.column.getCollationName( ), is( nullValue() ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.COLLATION_NAME ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithEmptyString() throws Exception {
        this.column.setDefaultValue( "defaultValue" );
        this.column.setDefaultValue( StringConstants.EMPTY_STRING );
        assertThat( this.column.getDefaultValue( ), is( nullValue() ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithNull() throws Exception {
        this.column.setDefaultValue( "defaultValue" );
        this.column.setDefaultValue( null );
        assertThat( this.column.getDefaultValue( ), is( nullValue() ) );
        assertThat( this.column.hasProperty( StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.column.rename( getTransaction(), newName );
        assertThat( this.column.getName( ), is( newName ) );
    }

    @Test
    public void shouldSetAutoIncrementedProperty() throws Exception {
        final boolean value = true;
        this.column.setAutoIncremented( value );
        assertThat( this.column.isAutoIncremented(), is( value ) );
        assertThat( this.column.getProperty( TeiidDdlLexicon.CreateTable.AUTO_INCREMENT ).getBooleanValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetCaseSensitive() throws Exception {
        final boolean value = !Column.DEFAULT_CASE_SENSITIVE;
        this.column.setCaseSensitive( value );
        assertThat( this.column.isCaseSensitive(), is( value ) );
    }

    @Test
    public void shouldSetCharOctetLength() throws Exception {
        final long value = 10;
        this.column.setCharOctetLength( value );
        assertThat( this.column.getCharOctetLength( ), is( value ) );
    }

    @Test
    public void shouldSetCollationNameProperty() throws Exception {
        final String value = "collationname";
        this.column.setCollationName( value );
        assertThat( this.column.getCollationName( ), is( value ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.COLLATION_NAME ).getStringValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetCurrency() throws Exception {
        final boolean value = !Column.DEFAULT_CURRENCY;
        this.column.setCurrency( value );
        assertThat( this.column.isCurrency(), is( value ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.column.setLength( value );
        assertThat( this.column.getLength( ), is( value ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.column.setDatatypeName( value );
        assertThat( this.column.getDatatypeName( ), is( value ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.DATATYPE_NAME ).getStringValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final long value = 10;
        this.column.setPrecision( value );
        assertThat( this.column.getPrecision( ), is( value ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.DATATYPE_PRECISION ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final long value = 10;
        this.column.setScale( value );
        assertThat( this.column.getScale( ), is( value ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.DATATYPE_SCALE ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.column.setDefaultValue( value );
        assertThat( this.column.getDefaultValue( ), is( value ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.DEFAULT_VALUE ).getStringValue( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.column.setDescription( value );
        assertThat( this.column.getDescription( ), is( value ) );
    }

    @Test
    public void shouldSetDistinctValues() throws Exception {
        final long value = ( Column.DEFAULT_DISTINCT_VALUES + 5 );
        this.column.setDistinctValues( value );
        assertThat( this.column.getDistinctValues( ), is( value ) );
    }

    @Test
    public void shouldSetFixedLength() throws Exception {
        final boolean value = !Column.DEFAULT_FIXED_LENGTH;
        this.column.setFixedLength( value );
        assertThat( this.column.isFixedLength(), is( value ) );
    }

    @Test
    public void shouldSetMaxValue() throws Exception {
        final String value = "maxValue";
        this.column.setMaxValue( value );
        assertThat( this.column.getMaxValue( ), is( value ) );
    }

    @Test
    public void shouldSetMinValue() throws Exception {
        final String value = "minValue";
        this.column.setMinValue( value );
        assertThat( this.column.getMinValue( ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.column.setNameInSource( value );
        assertThat( this.column.getNameInSource( ), is( value ) );
    }

    @Test
    public void shouldSetNativeType() throws Exception {
        final String value = "nativeType";
        this.column.setNativeType( value );
        assertThat( this.column.getNativeType( ), is( value ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.column.setNullable( value );
        assertThat( this.column.getNullable( ), is( value ) );
        assertThat( this.column.getProperty( StandardDdlLexicon.NULLABLE ).getStringValue( getTransaction() ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetNullValueCount() throws Exception {
        final long value = 10;
        this.column.setNullValueCount( value );
        assertThat( this.column.getNullValueCount( ), is( value ) );
    }

    @Test
    public void shouldSetRadix() throws Exception {
        final long value = 10;
        this.column.setRadix( value );
        assertThat( this.column.getRadix( ), is( value ) );
    }

    @Test
    public void shouldSetSearchable() throws Exception {
        final Searchable value = Searchable.ALL_EXCEPT_LIKE;
        this.column.setSearchable( value );
        assertThat( this.column.getSearchable( ), is( value ) );
    }

    @Test
    public void shouldSetSelectable() throws Exception {
        final boolean value = !Column.DEFAULT_SELECTABLE;
        this.column.setSelectable( value );
        assertThat( this.column.isSelectable(), is( value ) );
    }

    @Test
    public void shouldSetSigned() throws Exception {
        final boolean value = !Column.DEFAULT_SIGNED;
        this.column.setSigned( value );
        assertThat( this.column.isSigned(), is( value ) );
    }

    @Test
    public void shouldSetUpdatable() throws Exception {
        final boolean value = !Column.DEFAULT_UPDATABLE;
        this.column.setUpdatable( value );
        assertThat( this.column.isUpdatable(), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.column.setUuid( value );
        assertThat( this.column.getUuid( ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.column.setStatementOption( customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( );
        boolean found = false;

        for ( final PropertyDescriptor descriptor : propDescriptors ) {
            if ( customName.equals( descriptor.getName() ) ) {
                found = true;
                break;
            }
        }

        if ( !found ) {
            fail( "Custom option '" + customName + "'was not included in the property descriptors" );
        }
    }

    @Test
    public void shouldIncludeStandardOptionDefaultValuesWithPropertyDescriptors() throws Exception {
        final Map< String, String > options = this.column.getStandardOptions();
        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( );

        for ( final Entry< String, String > entry : options.entrySet() ) {
            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( entry.getKey().equals( descriptor.getName() ) ) {
                    final String value = entry.getValue();
                    final Object[] defaultValues = descriptor.getDefaultValues();

                    if ( StringUtils.isBlank( value ) ) {
                        assertThat( defaultValues.length, is( 0 ) );
                    } else {
                        assertThat( defaultValues.length, is( 1 ) );
                        assertThat( value, is( defaultValues[0] ) );
                    }
                }
            }
        }
    }

    @Test
    public void shouldIncludeOptionsWithPropertyNames() throws Exception {
        final String custom = "blah";
        this.column.setStatementOption( custom, "sledge" );
        boolean customFound = false;

        final String standard = this.column.getStandardOptions().keySet().iterator().next();
        this.column.setStatementOption( standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.column.getPropertyNames( ) ) {
            if ( custom.equals( prop ) ) {
                if ( customFound ) {
                    fail( "Custom option included multiple times in property names" );
                }

                customFound = true;
            } else if ( standard.equals( prop ) ) {
                if ( standardFound ) {
                    fail( "Standard option included multiple times in property names" );
                }

                standardFound = true;
            }

            if ( customFound && standardFound ) {
                break;
            }
        }

        if ( !customFound ) {
            fail( "Custom option not included in property names" );
        }

        if ( !standardFound ) {
            fail( "Standard option not included in property names" );
        }
    }

    @Test
    public void shouldIncludeStandardOptionsWithPrimaryTypePropertyDescriptors() throws Exception {
        final Set< String > optionNames = this.column.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.column.getPrimaryType( ).getPropertyDescriptors( );

        for ( final String optionName : optionNames ) {
            boolean found = false;

            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( optionName.equals( descriptor.getName() ) ) {
                    found = true;
                    break;
                }
            }

            if ( !found ) {
                fail( "Option '" + optionName + "'was not included in the primary type property descriptors" );
            }
        }
    }

    @Test
    public void shouldIncludeStandardOptionsWithPropertyDescriptors() throws Exception {
        final Set< String > optionNames = this.column.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.column.getPropertyDescriptors( );

        for ( final String optionName : optionNames ) {
            boolean found = false;

            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( optionName.equals( descriptor.getName() ) ) {
                    found = true;
                    break;
                }
            }

            if ( !found ) {
                fail( "Option '" + optionName + "'was not included in the property descriptors" );
            }
        }
    }

    @Test
    public void shouldObtainCustomOptions() throws Exception {
        final String sledge = "sledge";
        this.column.setStatementOption( sledge, "hammer" );

        final String elvis = "elvis";
        this.column.setStatementOption( elvis, "presley" );

        assertThat( this.column.getCustomOptions( ).length, is( 2 ) );
        assertThat( Arrays.asList( this.column.getStatementOptionNames( ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.column.setStatementOption( custom, "sledge" );

        final String standard = this.column.getStandardOptions().keySet().iterator().next();
        this.column.setStatementOption( standard, "hammer" );

        assertThat( this.column.getStatementOptionNames( ).length, is( 2 ) );
        assertThat( Arrays.asList( this.column.getStatementOptionNames( ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.column.getStandardOptions().keySet().iterator().next();
        final String value = "newValue";
        this.column.setProperty( option, value ); // add
        this.column.setProperty( option, (Object)null ); // remove
        assertThat( this.column.hasProperty( option ), is( false ) );
        assertThat( this.column.hasChild( option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.column.setStatementOption( option, "initialValue" );

        final String value = "newValue";
        this.column.setProperty( option, value );

        assertThat( this.column.hasProperty( option ), is( true ) );
        assertThat( this.column.getProperty( option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.column.getStatementOptions( ).length, is( 1 ) );
        assertThat( this.column.isCustomOption( option ), is( true ) );

        final StatementOption statementOption = this.column.getStatementOptions( )[0];
        assertThat( statementOption.getName( ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.column.getStandardOptions().keySet().iterator().next();
        this.column.setStatementOption( option, "initialValue" );

        final String value = "newValue";
        this.column.setProperty( option, value );

        assertThat( this.column.hasProperty( option ), is( true ) );
        assertThat( this.column.getProperty( option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.column.isCustomOption( option ), is( false ) );
        assertThat( this.column.getStatementOptions( ).length, is( 1 ) );

        final StatementOption statementOption = this.column.getStatementOptions( )[0];
        assertThat( statementOption.getName( ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

}
