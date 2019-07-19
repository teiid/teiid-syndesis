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
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.PropertyDescriptor;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.Table.OnCommit;
import org.komodo.relational.model.Table.TemporaryType;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TableImplTest extends RelationalModelTest {

    private static final String NAME = "myTable";

    private ModelImpl model;
    private TableImpl table;

    @Before
    public void init() throws Exception {
        this.model = createModel();
        this.table = model.addTable( NAME );
        commit();
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final String name = "column";
        final ColumnImpl column = this.table.addColumn( name );
        assertThat( column, is( notNullValue() ) );
        assertThat( this.table.getColumns( ).length, is( 1 ) );
        assertThat( column.getName( ), is( name ) );
        assertThat( this.table.getChildren( ).length, is( 1 ) );
        assertThat( this.table.getChildren( )[0], is( instanceOf( Column.class ) ) );
    }

    @Test
    public void shouldAddForeignKey() throws Exception {
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        final ForeignKeyImpl foreignKey = this.table.addForeignKey( name, refTable );

        assertThat( foreignKey, is( notNullValue() ) );
        assertThat( foreignKey.getName( ), is( name ) );
        assertThat( this.table.getChildren( ).length, is( 1 ) );
        assertThat( this.table.getChildren( )[0], is( instanceOf( ForeignKey.class ) ) );
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.table.setStatementOption( name, value );
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( ), is( name ) );
        assertThat( statementOption.getOption( ), is( value ) );
    }

    @Test
    public void shouldAddUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        final UniqueConstraintImpl uniqueConstraint = this.table.addUniqueConstraint( name );

        assertThat( uniqueConstraint, is( notNullValue() ) );
        assertThat( uniqueConstraint.getName( ), is( name ) );
        assertThat( this.table.getChildren( ).length, is( 1 ) );
        assertThat( this.table.getChildren( )[0], is( instanceOf( UniqueConstraint.class ) ) );
    }

    @Test
    public void shouldAllowEmptyQueryExpression() throws Exception {
        this.table.setQueryExpression( StringConstants.EMPTY_STRING );
    }

    @Test
    public void shouldAllowNullOnCommitValue() throws Exception {
        this.table.setOnCommitValue( null );
    }

    @Test
    public void shouldAllowNullQueryExpression() throws Exception {
        this.table.setQueryExpression( null );
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.table.setSchemaElementType( null );
    }

    @Test
    public void shouldAllowNullTemporaryTypeValue() throws Exception {
        this.table.setTemporaryTableType( null );
    }

    @Test
    public void shouldCountChildren() throws Exception {
        this.table.addColumn( "column" );
        this.table.addUniqueConstraint( "uniqueConstraint" );
        this.table.setPrimaryKey( "primaryKey" );
        assertThat( this.table.getChildren( ).length, is( 3 ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyColumnName() throws Exception {
        this.table.addColumn( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyForeignKeyName() throws Exception {
        this.table.addForeignKey( StringConstants.EMPTY_STRING, mock( Table.class ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.table.setStatementOption( StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumnName() throws Exception {
        this.table.addColumn( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullForeignKeyName() throws Exception {
        this.table.addForeignKey( null, mock( Table.class ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.table.setStatementOption( null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint( null );
    }

    @Test
    public void shouldFailConstructionIfNotTable() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new TableImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingMissingPrimaryKey() throws Exception {
        this.table.removePrimaryKey( );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyDescriptionWhenNeverAdded() throws Exception {
        this.table.setDescription( StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyMaterializedTableWhenNeverAdded() throws Exception {
        this.table.setMaterializedTable( StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyNameInSourceWhenNeverAdded() throws Exception {
        this.table.setNameInSource( StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.table.setStatementOption( "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.table.setUuid( StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullDescriptionWhenNeverAdded() throws Exception {
        this.table.setDescription( null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullMaterializedTableWhenNeverAdded() throws Exception {
        this.table.setMaterializedTable( null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNameInSourceWhenNeverAdded() throws Exception {
        this.table.setNameInSource( null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullStatementOptionValueWhenNeverAdded() throws Exception {
        this.table.setStatementOption( "blah", null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.table.setUuid( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyColumnName() throws Exception {
        this.table.removeColumn( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyForeignKeyName() throws Exception {
        this.table.removeForeignKey( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.table.removeStatementOption( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint( StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveMissingPrimaryKey() throws Exception {
        this.table.removePrimaryKey( );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullColumnName() throws Exception {
        this.table.removeColumn( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullForeignKeyName() throws Exception {
        this.table.removeForeignKey( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.table.removeStatementOption( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint( null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownColumn() throws Exception {
        this.table.removeColumn( "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownForeignKey() throws Exception {
        this.table.removeForeignKey( "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.table.removeStatementOption( "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownUniqueConstraint() throws Exception {
        this.table.removeUniqueConstraint( "unknown" );
    }

    @Test
    public void shouldGetColumns() throws Exception {
        final int numColumns = 5;

        for ( int i = 0; i < numColumns; ++i ) {
            this.table.addColumn( "column" + i );
        }

        assertThat( this.table.getColumns( ).length, is( numColumns ) );
    }

    @Test
    public void shouldGetForeignKeys() throws Exception {
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final int numForeignKeys = 5;

        for ( int i = 0; i < numForeignKeys; ++i ) {
            this.table.addForeignKey( "foreignKey" + i, refTable );
        }

        assertThat( this.table.getForeignKeys( ).length, is( numForeignKeys ) );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for ( int i = 0; i < numStatementOptions; ++i ) {
            this.table.setStatementOption( "statementoption" + i, "statementvalue" + i );
        }

        assertThat( this.table.getStatementOptions( ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldGetUniqueConstraints() throws Exception {
        final int numUniqueConstraints = 5;

        for ( int i = 0; i < numUniqueConstraints; ++i ) {
            this.table.addUniqueConstraint( "foreignKey" + i );
        }

        assertThat( this.table.getUniqueConstraints( ).length, is( numUniqueConstraints ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.table.getTypeIdentifier( ), is(KomodoType.TABLE));
    }

    @Test
    public void shouldHaveDefaultCardinalityAfterConstruction() throws Exception {
        assertThat( this.table.getCardinality( ), is( Table.DEFAULT_CARDINALITY ) );
    }

    @Test
    public void shouldHaveDefaultMaterializedAfterConstruction() throws Exception {
        assertThat( this.table.isMaterialized( ), is( Table.DEFAULT_MATERIALIZED ) );
    }

    @Test
    public void shouldHaveDefaultUpdatableAfterConstruction() throws Exception {
        assertThat( this.table.isUpdatable( ), is( Table.DEFAULT_UPDATABLE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.table.getPropertyNames( );
        final String[] rawProps = this.table.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentModel() throws Exception {
        assertThat( this.table.getParent( ), is( instanceOf( ModelImpl.class ) ) );
        assertThat( this.table.getParent( ), is( ( KomodoObject )this.model ) );
    }

    @Test
    public void shouldHaveSchemaElementTypePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.table.getSchemaElementType( ), is( SchemaElementType.DEFAULT_VALUE ) );
        assertThat( this.table.hasProperty( StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.table.setStatementOption( customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.table.getPropertyDescriptors( );
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
    public void shouldIncludeOptionsWithPropertyNames() throws Exception {
        final String custom = "blah";
        this.table.setStatementOption( custom, "sledge" );
        boolean customFound = false;

        final String standard = this.table.getStandardOptions().keySet().iterator().next();
        this.table.setStatementOption( standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.table.getPropertyNames( ) ) {
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
    public void shouldIncludeStandardOptionDefaultValuesWithPropertyDescriptors() throws Exception {
        final Map< String, String > options = this.table.getStandardOptions();
        final PropertyDescriptor[] propDescriptors = this.table.getPropertyDescriptors( );

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
    public void shouldIncludeStandardOptionsWithPrimaryTypePropertyDescriptors() throws Exception {
        final Set< String > optionNames = this.table.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.table.getPrimaryType( ).getPropertyDescriptors( );

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
        final Set< String > optionNames = this.table.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.table.getPropertyDescriptors( );

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
        this.table.setStatementOption( sledge, "hammer" );

        final String elvis = "elvis";
        this.table.setStatementOption( elvis, "presley" );

        assertThat( this.table.getCustomOptions( ).length, is( 2 ) );
        assertThat( Arrays.asList( this.table.getStatementOptionNames( ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.table.setStatementOption( custom, "sledge" );

        final String standard = this.table.getStandardOptions().keySet().iterator().next();
        this.table.setStatementOption( standard, "hammer" );

        assertThat( this.table.getStatementOptionNames( ).length, is( 2 ) );
        assertThat( Arrays.asList( this.table.getStatementOptionNames( ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.table.getStandardOptions().keySet().iterator().next();
        final String value = "newValue";
        this.table.setProperty( option, value ); // add
        this.table.setProperty( option, (Object)null ); // remove
        assertThat( this.table.hasProperty( option ), is( false ) );
        assertThat( this.table.hasChild( option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.table.setStatementOption( option, "initialValue" );

        final String value = "newValue";
        this.table.setProperty( option, value );

        assertThat( this.table.hasProperty( option ), is( true ) );
        assertThat( this.table.getProperty( option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.table.getStatementOptions( ).length, is( 1 ) );
        assertThat( this.table.isCustomOption( option ), is( true ) );

        final StatementOption statementOption = this.table.getStatementOptions( )[0];
        assertThat( statementOption.getName( ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.table.getStandardOptions().keySet().iterator().next();
        this.table.setStatementOption( option, "initialValue" );

        final String value = "newValue";
        this.table.setProperty( option, value );

        assertThat( this.table.hasProperty( option ), is( true ) );
        assertThat( this.table.getProperty( option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.table.isCustomOption( option ), is( false ) );
        assertThat( this.table.getStatementOptions( ).length, is( 1 ) );

        final StatementOption statementOption = this.table.getStatementOptions( )[0];
        assertThat( statementOption.getName( ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.table.getPropertyNames( );
        final Filter[] filters = ((TableImpl)this.table).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.table.setCardinality( 5 );
        this.table.setStatementOption( "sledge", "hammer" );
        assertThat( this.table.getChildren( ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveOnCommitValueAfterConstruction() throws Exception {
        assertThat( this.table.hasProperty( StandardDdlLexicon.ON_COMMIT_VALUE ), is( false ) );
        assertThat( this.table.getOnCommitValue( ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHavePrimaryKeyAfterConstruction() throws Exception {
        assertThat( this.table.getPrimaryKey( ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveQueryExpressionAfterConstruction() throws Exception {
        assertThat( this.table.getQueryExpression( ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveTemporaryTypeValueAfterConstruction() throws Exception {
        assertThat( this.table.hasProperty( StandardDdlLexicon.TEMPORARY ), is( false ) );
        assertThat( this.table.getTemporaryTableType( ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final String name = "column";
        this.table.addColumn( name );
        this.table.removeColumn( name );
        assertThat( this.table.getColumns( ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveForeignKey() throws Exception {
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        this.table.addForeignKey( name, refTable );
        this.table.removeForeignKey( name );

        assertThat( this.table.getForeignKeys( ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePrimaryKey() throws Exception {
        this.table.setPrimaryKey( "primaryKey" );
        this.table.removePrimaryKey( );
        assertThat( this.table.getPrimaryKey( ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.table.setStatementOption( name, "blah" );
        this.table.removeStatementOption( name );
        assertThat( this.table.getStatementOptions( ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        this.table.addUniqueConstraint( name );
        this.table.removeUniqueConstraint( name );

        assertThat( this.table.getUniqueConstraints( ).length, is( 0 ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.table.rename( getTransaction(), newName );
        assertThat( this.table.getName( ), is( newName ) );
    }

    @Test
    public void shouldSetCardinality() throws Exception {
        final long value = 10;
        this.table.setCardinality( value );
        assertThat( this.table.getCardinality( ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.table.setDescription( value );
        assertThat( this.table.getDescription( ), is( value ) );
    }

    @Test
    public void shouldSetMaterialized() throws Exception {
        final boolean value = !Table.DEFAULT_MATERIALIZED;
        this.table.setMaterialized( value );
        assertThat( this.table.isMaterialized( ), is( value ) );
    }

    @Test
    public void shouldSetMaterializedTable() throws Exception {
        final String value = "materializedTable";
        this.table.setMaterializedTable( value );
        assertThat( this.table.getMaterializedTable( ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.table.setNameInSource( value );
        assertThat( this.table.getNameInSource( ), is( value ) );
    }

    @Test
    public void shouldSetOnCommitProperty() throws Exception {
        final OnCommit value = OnCommit.DELETE_ROWS;
        this.table.setOnCommitValue( value );
        assertThat( this.table.getOnCommitValue( ), is( value ) );
        assertThat( this.table.getProperty( StandardDdlLexicon.ON_COMMIT_VALUE ).getStringValue( getTransaction() ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetPrimaryKey() throws Exception {
        final String name = "primaryKey";
        final PrimaryKeyImpl pk = this.table.setPrimaryKey( name );
        assertThat( pk, is( notNullValue() ) );
        assertThat( pk.getName( ), is( name ) );
        assertThat( this.table.hasChild( name ), is( true ) );
    }

    @Test
    public void shouldSetQueryExpression() throws Exception {
        final String value = ( "select * from " + NAME );
        this.table.setQueryExpression( value );
        assertThat( this.table.getQueryExpression( ), is( value ) );
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.table.setSchemaElementType( value );
        assertThat( this.table.getSchemaElementType( ), is( value ) );
        assertThat( this.table.getProperty( TeiidDdlLexicon.SchemaElement.TYPE ).getStringValue( getTransaction() ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetTemporaryTableTypeProperty() throws Exception {
        final TemporaryType value = TemporaryType.GLOBAL;
        this.table.setTemporaryTableType( value );
        assertThat( this.table.getTemporaryTableType( ), is( value ) );
        assertThat( this.table.getProperty( StandardDdlLexicon.TEMPORARY ).getStringValue( getTransaction() ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetUpdatable() throws Exception {
        final boolean value = !Table.DEFAULT_UPDATABLE;
        this.table.setUpdatable( value );
        assertThat( this.table.isUpdatable( ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.table.setUuid( value );
        assertThat( this.table.getUuid( ), is( value ) );
    }
    
    @Test
    public void shouldExportDdl() throws Exception {
        // Add columns
        final ColumnImpl column1 = this.table.addColumn( "column1" );
        column1.setDescription( "Col1 Description" );
        column1.setDatatypeName( "string" );
        column1.setNameInSource( "Col1_NIS" );
        final ColumnImpl column2 = this.table.addColumn( "column2" );
        column2.setDescription( "Col2 Description" );
        column2.setDatatypeName( "string" );
        column2.setNameInSource( "Col2_NIS" );

        // Add a FK
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        this.table.addForeignKey( name, refTable );
        commit();

        // Export the table
        byte[] bytes = this.table.export(new Properties());
        String exportedDdl = new String(bytes);
        
        // Check exported DDL
        assertThat( exportedDdl.contains("CREATE FOREIGN TABLE"), is( true ) );
        assertThat( exportedDdl.contains("myTable"), is( true ) );
        assertThat( exportedDdl.contains("column1"), is( true ) );
        assertThat( exportedDdl.contains("column2"), is( true ) );
        assertThat( exportedDdl.contains("FOREIGN KEY"), is( true ) );
        assertThat( exportedDdl.contains("undefined"), is(false));
    }
    
    @Test
    public void shouldExportDdlExcludeConstraints() throws Exception {
        // Add columns
        final ColumnImpl column1 = this.table.addColumn( "column1" );
        column1.setDescription( "Col1 Description" );
        column1.setDatatypeName( "string" );
        column1.setNameInSource( "Col1_NIS" );
        final ColumnImpl column2 = this.table.addColumn( "column2" );
        column2.setDescription( "Col2 Description" );
        column2.setDatatypeName( "string" );
        column2.setNameInSource( "Col2_NIS" );

        // Add a FK
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        this.table.addForeignKey( name, refTable );

        // Export the table
        Properties exportProps = new Properties();
        exportProps.put( Exportable.EXCLUDE_TABLE_CONSTRAINTS_KEY, true );
        byte[] bytes = this.table.export(exportProps);
        String exportedDdl = new String(bytes);
        
        // Check exported DDL
        assertThat( exportedDdl.contains("CREATE FOREIGN TABLE"), is( true ) );
        assertThat( exportedDdl.contains("myTable"), is( true ) );
        assertThat( exportedDdl.contains("column1"), is( true ) );
        assertThat( exportedDdl.contains("column2"), is( true ) );
        assertThat( exportedDdl.contains("FOREIGN KEY"), is( false ) );
    }

}
