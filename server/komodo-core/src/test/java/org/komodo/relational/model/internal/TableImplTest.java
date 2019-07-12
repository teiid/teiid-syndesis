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
        this.table = model.addTable( getTransaction(), NAME );
        commit();
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final String name = "column";
        final ColumnImpl column = this.table.addColumn( getTransaction(), name );
        assertThat( column, is( notNullValue() ) );
        assertThat( this.table.getColumns( getTransaction() ).length, is( 1 ) );
        assertThat( column.getName( getTransaction() ), is( name ) );
        assertThat( this.table.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.table.getChildren( getTransaction() )[0], is( instanceOf( Column.class ) ) );
    }

    @Test
    public void shouldAddForeignKey() throws Exception {
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        final ForeignKeyImpl foreignKey = this.table.addForeignKey( getTransaction(), name, refTable );

        assertThat( foreignKey, is( notNullValue() ) );
        assertThat( foreignKey.getName( getTransaction() ), is( name ) );
        assertThat( this.table.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.table.getChildren( getTransaction() )[0], is( instanceOf( ForeignKey.class ) ) );
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.table.setStatementOption( getTransaction(), name, value );
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( getTransaction() ), is( name ) );
        assertThat( statementOption.getOption( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldAddUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        final UniqueConstraintImpl uniqueConstraint = this.table.addUniqueConstraint( getTransaction(), name );

        assertThat( uniqueConstraint, is( notNullValue() ) );
        assertThat( uniqueConstraint.getName( getTransaction() ), is( name ) );
        assertThat( this.table.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.table.getChildren( getTransaction() )[0], is( instanceOf( UniqueConstraint.class ) ) );
    }

    @Test
    public void shouldAllowEmptyQueryExpression() throws Exception {
        this.table.setQueryExpression( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test
    public void shouldAllowNullOnCommitValue() throws Exception {
        this.table.setOnCommitValue( getTransaction(), null );
    }

    @Test
    public void shouldAllowNullQueryExpression() throws Exception {
        this.table.setQueryExpression( getTransaction(), null );
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.table.setSchemaElementType( getTransaction(), null );
    }

    @Test
    public void shouldAllowNullTemporaryTypeValue() throws Exception {
        this.table.setTemporaryTableType( getTransaction(), null );
    }

    @Test
    public void shouldCountChildren() throws Exception {
        this.table.addColumn( getTransaction(), "column" );
        this.table.addUniqueConstraint( getTransaction(), "uniqueConstraint" );
        this.table.setPrimaryKey( getTransaction(), "primaryKey" );
        assertThat( this.table.getChildren( getTransaction() ).length, is( 3 ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyColumnName() throws Exception {
        this.table.addColumn( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyForeignKeyName() throws Exception {
        this.table.addForeignKey( getTransaction(), StringConstants.EMPTY_STRING, mock( Table.class ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.table.setStatementOption( getTransaction(), StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumnName() throws Exception {
        this.table.addColumn( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullForeignKeyName() throws Exception {
        this.table.addForeignKey( getTransaction(), null, mock( Table.class ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.table.setStatementOption( getTransaction(), null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullUniqueConstraintName() throws Exception {
        this.table.addUniqueConstraint( getTransaction(), null );
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
        this.table.removePrimaryKey( getTransaction() );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyDescriptionWhenNeverAdded() throws Exception {
        this.table.setDescription( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyMaterializedTableWhenNeverAdded() throws Exception {
        this.table.setMaterializedTable( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyNameInSourceWhenNeverAdded() throws Exception {
        this.table.setNameInSource( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.table.setStatementOption( getTransaction(), "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.table.setUuid( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullDescriptionWhenNeverAdded() throws Exception {
        this.table.setDescription( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullMaterializedTableWhenNeverAdded() throws Exception {
        this.table.setMaterializedTable( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNameInSourceWhenNeverAdded() throws Exception {
        this.table.setNameInSource( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullStatementOptionValueWhenNeverAdded() throws Exception {
        this.table.setStatementOption( getTransaction(), "blah", null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.table.setUuid( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyColumnName() throws Exception {
        this.table.removeColumn( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyForeignKeyName() throws Exception {
        this.table.removeForeignKey( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.table.removeStatementOption( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveMissingPrimaryKey() throws Exception {
        this.table.removePrimaryKey( getTransaction() );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullColumnName() throws Exception {
        this.table.removeColumn( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullForeignKeyName() throws Exception {
        this.table.removeForeignKey( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.table.removeStatementOption( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullUniqueConstraintName() throws Exception {
        this.table.removeUniqueConstraint( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownColumn() throws Exception {
        this.table.removeColumn( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownForeignKey() throws Exception {
        this.table.removeForeignKey( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.table.removeStatementOption( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownUniqueConstraint() throws Exception {
        this.table.removeUniqueConstraint( getTransaction(), "unknown" );
    }

    @Test
    public void shouldGetColumns() throws Exception {
        final int numColumns = 5;

        for ( int i = 0; i < numColumns; ++i ) {
            this.table.addColumn( getTransaction(), "column" + i );
        }

        assertThat( this.table.getColumns( getTransaction() ).length, is( numColumns ) );
    }

    @Test
    public void shouldGetForeignKeys() throws Exception {
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final int numForeignKeys = 5;

        for ( int i = 0; i < numForeignKeys; ++i ) {
            this.table.addForeignKey( getTransaction(), "foreignKey" + i, refTable );
        }

        assertThat( this.table.getForeignKeys( getTransaction() ).length, is( numForeignKeys ) );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for ( int i = 0; i < numStatementOptions; ++i ) {
            this.table.setStatementOption( getTransaction(), "statementoption" + i, "statementvalue" + i );
        }

        assertThat( this.table.getStatementOptions( getTransaction() ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldGetUniqueConstraints() throws Exception {
        final int numUniqueConstraints = 5;

        for ( int i = 0; i < numUniqueConstraints; ++i ) {
            this.table.addUniqueConstraint( getTransaction(), "foreignKey" + i );
        }

        assertThat( this.table.getUniqueConstraints( getTransaction() ).length, is( numUniqueConstraints ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.table.getTypeIdentifier( getTransaction() ), is(KomodoType.TABLE));
    }

    @Test
    public void shouldHaveDefaultCardinalityAfterConstruction() throws Exception {
        assertThat( this.table.getCardinality( getTransaction() ), is( Table.DEFAULT_CARDINALITY ) );
    }

    @Test
    public void shouldHaveDefaultMaterializedAfterConstruction() throws Exception {
        assertThat( this.table.isMaterialized( getTransaction() ), is( Table.DEFAULT_MATERIALIZED ) );
    }

    @Test
    public void shouldHaveDefaultUpdatableAfterConstruction() throws Exception {
        assertThat( this.table.isUpdatable( getTransaction() ), is( Table.DEFAULT_UPDATABLE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.table.getPropertyNames( getTransaction() );
        final String[] rawProps = this.table.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentModel() throws Exception {
        assertThat( this.table.getParent( getTransaction() ), is( instanceOf( ModelImpl.class ) ) );
        assertThat( this.table.getParent( getTransaction() ), is( ( KomodoObject )this.model ) );
    }

    @Test
    public void shouldHaveSchemaElementTypePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.table.getSchemaElementType( getTransaction() ), is( SchemaElementType.DEFAULT_VALUE ) );
        assertThat( this.table.hasProperty( getTransaction(), StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.table.setStatementOption( getTransaction(), customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.table.getPropertyDescriptors( getTransaction() );
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
        this.table.setStatementOption( getTransaction(), custom, "sledge" );
        boolean customFound = false;

        final String standard = this.table.getStandardOptions().keySet().iterator().next();
        this.table.setStatementOption( getTransaction(), standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.table.getPropertyNames( getTransaction() ) ) {
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
        final PropertyDescriptor[] propDescriptors = this.table.getPropertyDescriptors( getTransaction() );

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
        final PropertyDescriptor[] propDescriptors = this.table.getPrimaryType( getTransaction() ).getPropertyDescriptors( getTransaction() );

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
        final PropertyDescriptor[] propDescriptors = this.table.getPropertyDescriptors( getTransaction() );

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
        this.table.setStatementOption( getTransaction(), sledge, "hammer" );

        final String elvis = "elvis";
        this.table.setStatementOption( getTransaction(), elvis, "presley" );

        assertThat( this.table.getCustomOptions( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.table.getStatementOptionNames( getTransaction() ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.table.setStatementOption( getTransaction(), custom, "sledge" );

        final String standard = this.table.getStandardOptions().keySet().iterator().next();
        this.table.setStatementOption( getTransaction(), standard, "hammer" );

        assertThat( this.table.getStatementOptionNames( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.table.getStatementOptionNames( getTransaction() ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.table.getStandardOptions().keySet().iterator().next();
        final String value = "newValue";
        this.table.setProperty( getTransaction(), option, value ); // add
        this.table.setProperty( getTransaction(), option, (Object)null ); // remove
        assertThat( this.table.hasProperty( getTransaction(), option ), is( false ) );
        assertThat( this.table.hasChild( getTransaction(), option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.table.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.table.setProperty( getTransaction(), option, value );

        assertThat( this.table.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.table.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.table.getStatementOptions( getTransaction() ).length, is( 1 ) );
        assertThat( this.table.isCustomOption( getTransaction(), option ), is( true ) );

        final StatementOption statementOption = this.table.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.table.getStandardOptions().keySet().iterator().next();
        this.table.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.table.setProperty( getTransaction(), option, value );

        assertThat( this.table.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.table.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.table.isCustomOption( getTransaction(), option ), is( false ) );
        assertThat( this.table.getStatementOptions( getTransaction() ).length, is( 1 ) );

        final StatementOption statementOption = this.table.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.table.getPropertyNames( getTransaction() );
        final Filter[] filters = ((TableImpl)this.table).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.table.setCardinality( getTransaction(), 5 );
        this.table.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.table.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveOnCommitValueAfterConstruction() throws Exception {
        assertThat( this.table.hasProperty( getTransaction(), StandardDdlLexicon.ON_COMMIT_VALUE ), is( false ) );
        assertThat( this.table.getOnCommitValue( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHavePrimaryKeyAfterConstruction() throws Exception {
        assertThat( this.table.getPrimaryKey( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveQueryExpressionAfterConstruction() throws Exception {
        assertThat( this.table.getQueryExpression( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveTemporaryTypeValueAfterConstruction() throws Exception {
        assertThat( this.table.hasProperty( getTransaction(), StandardDdlLexicon.TEMPORARY ), is( false ) );
        assertThat( this.table.getTemporaryTableType( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final String name = "column";
        this.table.addColumn( getTransaction(), name );
        this.table.removeColumn( getTransaction(), name );
        assertThat( this.table.getColumns( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveForeignKey() throws Exception {
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        this.table.addForeignKey( getTransaction(), name, refTable );
        this.table.removeForeignKey( getTransaction(), name );

        assertThat( this.table.getForeignKeys( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePrimaryKey() throws Exception {
        this.table.setPrimaryKey( getTransaction(), "primaryKey" );
        this.table.removePrimaryKey( getTransaction() );
        assertThat( this.table.getPrimaryKey( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.table.setStatementOption( getTransaction(), name, "blah" );
        this.table.removeStatementOption( getTransaction(), name );
        assertThat( this.table.getStatementOptions( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveUniqueConstraint() throws Exception {
        final String name = "uniqueConstraint";
        this.table.addUniqueConstraint( getTransaction(), name );
        this.table.removeUniqueConstraint( getTransaction(), name );

        assertThat( this.table.getUniqueConstraints( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.table.rename( getTransaction(), newName );
        assertThat( this.table.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetCardinality() throws Exception {
        final long value = 10;
        this.table.setCardinality( getTransaction(), value );
        assertThat( this.table.getCardinality( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.table.setDescription( getTransaction(), value );
        assertThat( this.table.getDescription( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetMaterialized() throws Exception {
        final boolean value = !Table.DEFAULT_MATERIALIZED;
        this.table.setMaterialized( getTransaction(), value );
        assertThat( this.table.isMaterialized( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetMaterializedTable() throws Exception {
        final String value = "materializedTable";
        this.table.setMaterializedTable( getTransaction(), value );
        assertThat( this.table.getMaterializedTable( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.table.setNameInSource( getTransaction(), value );
        assertThat( this.table.getNameInSource( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetOnCommitProperty() throws Exception {
        final OnCommit value = OnCommit.DELETE_ROWS;
        this.table.setOnCommitValue( getTransaction(), value );
        assertThat( this.table.getOnCommitValue( getTransaction() ), is( value ) );
        assertThat( this.table.getProperty( getTransaction(), StandardDdlLexicon.ON_COMMIT_VALUE ).getStringValue( getTransaction() ),
                    is( value.toValue() ) );
    }

    @Test
    public void shouldSetPrimaryKey() throws Exception {
        final String name = "primaryKey";
        final PrimaryKeyImpl pk = this.table.setPrimaryKey( getTransaction(), name );
        assertThat( pk, is( notNullValue() ) );
        assertThat( pk.getName( getTransaction() ), is( name ) );
        assertThat( this.table.hasChild( getTransaction(), name ), is( true ) );
    }

    @Test
    public void shouldSetQueryExpression() throws Exception {
        final String value = ( "select * from " + NAME );
        this.table.setQueryExpression( getTransaction(), value );
        assertThat( this.table.getQueryExpression( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.table.setSchemaElementType( getTransaction(), value );
        assertThat( this.table.getSchemaElementType( getTransaction() ), is( value ) );
        assertThat( this.table.getProperty( getTransaction(), TeiidDdlLexicon.SchemaElement.TYPE ).getStringValue( getTransaction() ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetTemporaryTableTypeProperty() throws Exception {
        final TemporaryType value = TemporaryType.GLOBAL;
        this.table.setTemporaryTableType( getTransaction(), value );
        assertThat( this.table.getTemporaryTableType( getTransaction() ), is( value ) );
        assertThat( this.table.getProperty( getTransaction(), StandardDdlLexicon.TEMPORARY ).getStringValue( getTransaction() ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetUpdatable() throws Exception {
        final boolean value = !Table.DEFAULT_UPDATABLE;
        this.table.setUpdatable( getTransaction(), value );
        assertThat( this.table.isUpdatable( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.table.setUuid( getTransaction(), value );
        assertThat( this.table.getUuid( getTransaction() ), is( value ) );
    }
    
    @Test
    public void shouldExportDdl() throws Exception {
        // Add columns
        final ColumnImpl column1 = this.table.addColumn( getTransaction(), "column1" );
        column1.setDescription( getTransaction(), "Col1 Description" );
        column1.setDatatypeName( getTransaction(), "string" );
        column1.setNameInSource( getTransaction(), "Col1_NIS" );
        final ColumnImpl column2 = this.table.addColumn( getTransaction(), "column2" );
        column2.setDescription( getTransaction(), "Col2 Description" );
        column2.setDatatypeName( getTransaction(), "string" );
        column2.setNameInSource( getTransaction(), "Col2_NIS" );

        // Add a FK
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        this.table.addForeignKey( getTransaction(), name, refTable );
        commit();

        // Export the table
        byte[] bytes = this.table.export(getTransaction(), new Properties());
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
        final ColumnImpl column1 = this.table.addColumn( getTransaction(), "column1" );
        column1.setDescription( getTransaction(), "Col1 Description" );
        column1.setDatatypeName( getTransaction(), "string" );
        column1.setNameInSource( getTransaction(), "Col1_NIS" );
        final ColumnImpl column2 = this.table.addColumn( getTransaction(), "column2" );
        column2.setDescription( getTransaction(), "Col2 Description" );
        column2.setDatatypeName( getTransaction(), "string" );
        column2.setNameInSource( getTransaction(), "Col2_NIS" );

        // Add a FK
        final TableImpl refTable = RelationalModelFactory.createTable( getTransaction(), _repo, mock( ModelImpl.class ), "refTable" );
        final String name = "foreignKey";
        this.table.addForeignKey( getTransaction(), name, refTable );

        // Export the table
        Properties exportProps = new Properties();
        exportProps.put( Exportable.EXCLUDE_TABLE_CONSTRAINTS_KEY, true );
        byte[] bytes = this.table.export(getTransaction(), exportProps);
        String exportedDdl = new String(bytes);
        
        // Check exported DDL
        assertThat( exportedDdl.contains("CREATE FOREIGN TABLE"), is( true ) );
        assertThat( exportedDdl.contains("myTable"), is( true ) );
        assertThat( exportedDdl.contains("column1"), is( true ) );
        assertThat( exportedDdl.contains("column2"), is( true ) );
        assertThat( exportedDdl.contains("FOREIGN KEY"), is( false ) );
    }

}
