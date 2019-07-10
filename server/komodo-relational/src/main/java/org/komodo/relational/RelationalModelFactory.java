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
package org.komodo.relational;

import java.util.Map;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.repository.RepositoryTools;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.ServiceVdbEntry;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.dataservice.internal.ServiceVdbEntryImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
import org.komodo.relational.model.internal.ColumnImpl;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.relational.model.internal.StatementOptionImpl;
import org.komodo.relational.model.internal.TableImpl;
import org.komodo.relational.model.internal.UniqueConstraintImpl;
import org.komodo.relational.model.internal.ViewImpl;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.relational.profile.StateCommand;
import org.komodo.relational.profile.StateCommandAggregate;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.profile.internal.SqlCompositionImpl;
import org.komodo.relational.profile.internal.SqlProjectedColumnImpl;
import org.komodo.relational.profile.internal.StateCommandAggregateImpl;
import org.komodo.relational.profile.internal.ViewDefinitionImpl;
import org.komodo.relational.profile.internal.ViewEditorStateImpl;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.vdb.internal.VdbImportImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A factory for {@link RelationalObject relational model objects}.
 */
public final class RelationalModelFactory {

    /**
     * Wraps the given exception in a {@link KException}
     *
     * @param e
     *        the exception
     * @return return a {@link KException} from the given {@link Exception}
     */
    public static KException handleError( final Exception e ) {
        assert ( e != null );

        if ( e instanceof KException ) {
            return ( KException )e;
        }

        return new KException( e );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param table
     *        the table where the column is being created (cannot be <code>null</code>)
     * @param columnName
     *        the name of the column to create (cannot be empty)
     * @return the column model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Column createColumn( final UnitOfWork transaction,
                                       final Repository repository,
                                       final Table table,
                                       final String columnName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( table, "table" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( columnName, "columnName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, table.getAbsolutePath(), columnName, null );
        kobject.addDescriptor( transaction, CreateTable.TABLE_ELEMENT );

        final Column result = new ColumnImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentWorkspacePath
     *        the parent path (can be empty)
     * @param serviceName
     *        the name of the dataservice fragment to create (cannot be empty)
     * @return the Dataservice model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Dataservice createDataservice( final UnitOfWork transaction,
                                                 final Repository repository,
                                                 final String parentWorkspacePath,
                                                 final String serviceName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( serviceName, "serviceName" ); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = parentWorkspacePath;
        final String workspacePath = repository.komodoWorkspace( transaction ).getAbsolutePath();

        if ( StringUtils.isBlank( parentWorkspacePath ) ) {
            parentPath = workspacePath;
        } else if ( !parentPath.startsWith( workspacePath ) ) {
            parentPath = ( workspacePath + parentPath );
        }

        final KomodoObject kobject = repository.add( transaction, parentPath, serviceName, DataVirtLexicon.DataService.NODE_TYPE );
        final Dataservice result = new DataserviceImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the table model object where the foreign key is being created (cannot be <code>null</code>)
     * @param foreignKeyName
     *        the name of the foreign key to create (cannot be empty)
     * @param tableReference
     *        the table referenced by this constraint (cannot be <code>null</code>)
     * @return the foreign key model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static ForeignKey createForeignKey( final UnitOfWork transaction,
                                               final Repository repository,
                                               final Table parentTable,
                                               final String foreignKeyName,
                                               final Table tableReference ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( foreignKeyName, "foreignKeyName" ); //$NON-NLS-1$
        ArgCheck.isNotNull( tableReference, "tableReference" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), foreignKeyName, null );
        kobject.addDescriptor( transaction, Constraint.FOREIGN_KEY_CONSTRAINT );
        kobject.setProperty( transaction, Constraint.TYPE, ForeignKey.CONSTRAINT_TYPE.toValue() );

        final ForeignKey fk = new ForeignKeyImpl( transaction, repository, kobject.getAbsolutePath() );
        fk.setReferencesTable( transaction, tableReference );
        return fk;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param vdb
     *        the VDB where the model is being created (cannot be <code>null</code>)
     * @param modelName
     *        the name of the model to create (cannot be empty)
     * @return the VDB import model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Model createModel( final UnitOfWork transaction,
                                     final Repository repository,
                                     final Vdb vdb,
                                     final String modelName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( vdb, "vdb" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( modelName, "modelName" ); //$NON-NLS-1$

        final KomodoObject kobject = vdb.addChild( transaction, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL );
        final Model result = new ModelImpl( transaction, repository, kobject.getAbsolutePath() );

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentModel
     *        the model where the model source is being created (cannot be <code>null</code>)
     * @param sourceName
     *        the name of the model source to create (cannot be empty)
     * @return the model source object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static ModelSource createModelSource( final UnitOfWork transaction,
                                                 final Repository repository,
                                                 final Model parentModel,
                                                 final String sourceName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( sourceName, "sourceName" ); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction,
                                                                             parentModel,
                                                                             VdbLexicon.Vdb.SOURCES,
                                                                             VdbLexicon.Vdb.SOURCES );
            final KomodoObject kobject = grouping.addChild( transaction, sourceName, VdbLexicon.Source.SOURCE );
            final ModelSource result = new ModelSourceImpl( transaction, repository, kobject.getAbsolutePath() );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the parent of the model object being created (cannot be <code>null</code>)
     * @param primaryKeyName
     *        the name of the primary key to create (cannot be empty)
     * @return the primary key model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static PrimaryKey createPrimaryKey( final UnitOfWork transaction,
                                               final Repository repository,
                                               final Table parentTable,
                                               final String primaryKeyName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( primaryKeyName, "primaryKeyName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), primaryKeyName, null );
        kobject.addDescriptor( transaction, Constraint.TABLE_ELEMENT );
        kobject.setProperty( transaction, Constraint.TYPE, PrimaryKey.CONSTRAINT_TYPE.toValue() );

        final PrimaryKey result = new PrimaryKeyImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param dataService
     *        the data service where the VDB file entry is being created (cannot be <code>null</code>)
     * @param serviceVdbEntryName
     *        the name of the service VDB entry to create (cannot be empty)
     * @return the service VDB entry model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static ServiceVdbEntry createServiceVdbEntry( final UnitOfWork transaction,
                                                         final Repository repository,
                                                         final Dataservice dataService,
                                                         final String serviceVdbEntryName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( dataService, "dataService" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( serviceVdbEntryName, "serviceVdbEntryName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction,
                                                     dataService.getAbsolutePath(),
                                                     serviceVdbEntryName,
                                                     DataVirtLexicon.ServiceVdbEntry.NODE_TYPE );
        final ServiceVdbEntry result = new ServiceVdbEntryImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param optionContainer
     *        the parent where the option is being created (can be empty if created at the root of the workspace)
     * @param optionName
     *        the name of the statement option to create (cannot be empty)
     * @param optionValue
     *        the option value (cannot be empty)
     * @return the statement option model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static StatementOption createStatementOption( final UnitOfWork transaction,
                                                         final Repository repository,
                                                         final OptionContainer optionContainer,
                                                         final String optionName,
                                                         final String optionValue ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( optionContainer, "optionContainer" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionName, "optionName" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( optionValue, "optionValue" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, optionContainer.getAbsolutePath(), optionName, null );
        kobject.addDescriptor( transaction, StandardDdlLexicon.TYPE_STATEMENT_OPTION );

        final StatementOption result = new StatementOptionImpl( transaction, repository, kobject.getAbsolutePath() );
        result.setOption( transaction, optionValue );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentModel
     *        the model where the table is being created (cannot be <code>null</code>)
     * @param tableName
     *        the name of the table to create (cannot be empty)
     * @return the table model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Table createTable( final UnitOfWork transaction,
                                     final Repository repository,
                                     final Model parentModel,
                                     final String tableName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( tableName, "tableName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), tableName, null );
        kobject.addDescriptor( transaction, CreateTable.TABLE_STATEMENT );
        setCreateStatementProperties( transaction, kobject );

        final Table result = new TableImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentVdb
     *        the VDB where the VDB import model object is being created (cannot be <code>null</code>)
     * @param translatorName
     *        the name of the VDB translator to create (cannot be empty)
     * @param translatorType
     *        the type of translator (cannot be empty)
     * @return the VDB translator model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Translator createTranslator( final UnitOfWork transaction,
                                               final Repository repository,
                                               final Vdb parentVdb,
                                               final String translatorName,
                                               final String translatorType ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentVdb, "parentVdb" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( translatorName, "translatorName" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( translatorType, "translatorType" ); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction,
                                                                             parentVdb,
                                                                             VdbLexicon.Vdb.TRANSLATORS,
                                                                             VdbLexicon.Vdb.TRANSLATORS );
            final KomodoObject kobject = grouping.addChild( transaction, translatorName, VdbLexicon.Translator.TRANSLATOR );
            final Translator result = new TranslatorImpl( transaction, grouping.getRepository(), kobject.getAbsolutePath() );
            result.setType( transaction, translatorType );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentTable
     *        the parent of the model object being created (cannot be <code>null</code>)
     * @param uniqueConstraintName
     *        the name of the unique constraint to create (cannot be empty)
     * @return the unique constraint model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static UniqueConstraint createUniqueConstraint( final UnitOfWork transaction,
                                                           final Repository repository,
                                                           final Table parentTable,
                                                           final String uniqueConstraintName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentTable, "parentTable" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( uniqueConstraintName, "uniqueConstraintName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentTable.getAbsolutePath(), uniqueConstraintName, null );
        kobject.addDescriptor( transaction, Constraint.TABLE_ELEMENT );
        kobject.setProperty( transaction, Constraint.TYPE, UniqueConstraint.CONSTRAINT_TYPE.toValue() );

        final UniqueConstraint result = new UniqueConstraintImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentWorkspacePath
     *        the parent path (can be empty)
     * @param vdbName
     *        the name of the VDB to create (cannot be empty)
     * @param externalFilePath
     *        the VDB file path on the local file system (cannot be empty)
     * @return the VDB model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Vdb createVdb( final UnitOfWork transaction,
                                 final Repository repository,
                                 final String parentWorkspacePath,
                                 final String vdbName,
                                 final String externalFilePath ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( externalFilePath, "externalFilePath" ); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = parentWorkspacePath;
        final String workspacePath = repository.komodoWorkspace( transaction ).getAbsolutePath();

        if ( StringUtils.isBlank( parentWorkspacePath ) ) {
            parentPath = workspacePath;
        } else if ( !parentPath.startsWith( workspacePath ) ) {
            parentPath = ( workspacePath + parentPath );
        }

        final KomodoObject kobject = repository.add( transaction, parentPath, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        final Vdb result = new VdbImpl( transaction, repository, kobject.getAbsolutePath() );
        result.setOriginalFilePath( transaction, externalFilePath );
        result.setVdbName( transaction, vdbName );
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentVdb
     *        the VDB where the VDB import model object is being created (cannot be <code>null</code>)
     * @param vdbName
     *        the name of the VDB import to create (cannot be empty)
     * @return the VDB import model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static VdbImport createVdbImport( final UnitOfWork transaction,
                                             final Repository repository,
                                             final Vdb parentVdb,
                                             final String vdbName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentVdb, "parentVdb" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction,
                                                                             parentVdb,
                                                                             VdbLexicon.Vdb.IMPORT_VDBS,
                                                                             VdbLexicon.Vdb.IMPORT_VDBS );
            final KomodoObject kobject = grouping.addChild( transaction, vdbName, VdbLexicon.ImportVdb.IMPORT_VDB );
            final VdbImport result = new VdbImportImpl( transaction, repository, kobject.getAbsolutePath() );
            result.setVersion( transaction, Vdb.DEFAULT_VERSION );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentModel
     *        the model where the view is being created (cannot be <code>null</code>)
     * @param viewName
     *        the name of the view to create (cannot be empty)
     * @return the view model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static View createView( final UnitOfWork transaction,
                                   final Repository repository,
                                   final Model parentModel,
                                   final String viewName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parentModel, "parentModel" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( viewName, "viewName" ); //$NON-NLS-1$

        final KomodoObject kobject = repository.add( transaction, parentModel.getAbsolutePath(), viewName, null );
        kobject.addDescriptor( transaction, CreateTable.VIEW_STATEMENT );
        setCreateStatementProperties( transaction, kobject );

        final View result = new ViewImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param profile
     *        the parent profile object
     * @param stateId
     *        the id of the view editor state
     * @return the view editor state object
     * @throws KException
     *        if an error occurs
     */
    public static ViewEditorState createViewEditorState(UnitOfWork transaction, Repository repository, Profile profile,
                                                                                                   String stateId) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( stateId, "stateId" ); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction,
                                                                             profile,
                                                                             KomodoLexicon.Profile.VIEW_EDITOR_STATES,
                                                                             KomodoLexicon.Profile.VIEW_EDITOR_STATES );
            final KomodoObject kobject = grouping.addChild( transaction, stateId, KomodoLexicon.ViewEditorState.NODE_TYPE );
            final ViewEditorState result = new ViewEditorStateImpl( transaction, repository, kobject.getAbsolutePath() );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }
    
    /**
    *
    * @param transaction
    *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
    * @param repository
    *        the repository where the model object will be created (cannot be <code>null</code>)
    * @param profile
    *        the parent profile object
    * @param stateId
    *        the id of the view definition
    * @return the view definition object
    * @throws KException
    *        if an error occurs
    */
   public static ViewDefinition createViewDefinition(UnitOfWork transaction, Repository repository, ViewEditorState viewEditorState) throws KException {
       ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
       ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
       ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$

       try {
   		   // If a ViewDefinition already exists, remove it first
           final ViewDefinition viewDefn = viewEditorState.getViewDefinition(transaction);
           if (viewDefn != null) {
           	viewDefn.remove(transaction);
           }

    	   final KomodoObject kobject = viewEditorState.addChild(transaction, KomodoLexicon.ViewEditorState.VIEW_DEFINITION, KomodoLexicon.ViewEditorState.VIEW_DEFINITION);
           final ViewDefinition result = new ViewDefinitionImpl( transaction, repository, kobject.getAbsolutePath() );
           return result;
       } catch ( final Exception e ) {
           throw handleError( e );
       }
   }
   
   /**
   *
   * @param transaction
   *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
   * @param repository
   *        the repository where the model object will be created (cannot be <code>null</code>)
   * @param viewDefinition
   *        the parent view definition
   * @param compositionName
   *        the sql composition name
   * @return the sql composition object
   * @throws KException
   *        if an error occurs
   */
	public static SqlComposition createSqlComposition(UnitOfWork transaction, 
			                                          Repository repository, 
			                                          ViewDefinition viewDefinition, 
			                                          String compositionName) throws KException {
		
	       ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
	       ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
	       ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
	       ArgCheck.isNotNull( compositionName, "compositionName" ); //$NON-NLS-1$
	       
	       try {
	           final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction, viewDefinition, KomodoLexicon.ViewDefinition.SQL_COMPOSITIONS,
	                                                                                                         KomodoLexicon.ViewDefinition.SQL_COMPOSITIONS );
	           final KomodoObject kobject = grouping.addChild( transaction, compositionName, KomodoLexicon.SqlComposition.NODE_TYPE );
	           final SqlComposition result = new SqlCompositionImpl( transaction, repository, kobject.getAbsolutePath() );
	           return result;
	       } catch ( final Exception e ) {
	           throw handleError( e );
	       }
	}

	/**
	 *
	 * @param transaction
	 *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
	 * @param repository
	 *        the repository where the model object will be created (cannot be <code>null</code>)
	 * @param viewDefinition
	 *        the parent view definition
	 * @param columnName
	 *        the sql projected column name
	 * @return the sql projected column object
	 * @throws KException
	 *        if an error occurs
	 */
	public static SqlProjectedColumn createSqlProjectedColumn(UnitOfWork transaction, 
	                                                          Repository repository, 
	                                                          ViewDefinition viewDefinition, 
	                                                          String columnName) throws KException {

	    ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
	    ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
	    ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
	    ArgCheck.isNotNull( columnName, "columnName" ); //$NON-NLS-1$

	    try {
	        final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction, viewDefinition, KomodoLexicon.ViewDefinition.SQL_PROJECTED_COLUMNS,
	                                                                         KomodoLexicon.ViewDefinition.SQL_PROJECTED_COLUMNS );
	        final KomodoObject kobject = grouping.addChild( transaction, columnName, KomodoLexicon.SqlProjectedColumn.NODE_TYPE );
	        final SqlProjectedColumn result = new SqlProjectedColumnImpl( transaction, repository, kobject.getAbsolutePath() );
	        return result;
	    } catch ( final Exception e ) {
	        throw handleError( e );
	    }
	}

    /**
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param viewEditorState
     *        the parent view editor state object
     * @return the view editor state command object
     * @throws KException
     *        if an error occurs
     */
    public static StateCommandAggregate createStateCommandAggregate (UnitOfWork transaction, Repository repository,
                                                                     ViewEditorState viewEditorState) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$

        try {
            int cmdSize = viewEditorState.getCommands(transaction).length;
            int index = cmdSize > 0 ? cmdSize - 1 : 0;
            String aggName = KomodoLexicon.StateCommandAggregate.NAME_PREFIX + index;
            final KomodoObject kobject = viewEditorState.addChild(transaction, aggName,
                                                                  KomodoLexicon.StateCommandAggregate.NODE_TYPE);
            final StateCommandAggregate result = new StateCommandAggregateImpl(transaction, repository, kobject.getAbsolutePath());
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
    *
    * @param transaction
    *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
    * @param repository
    *        the repository where the model object will be created (cannot be <code>null</code>)
    * @param stateCommandAgg
    *        the parent state command aggregate
    * @param stateCommandType
    *        the state command type (either 'undo' or 'redo')
    * @param commandId
    *        the id of the command
    * @param arguments
    *        the command arguments
    * @return the view editor state command object
    * @throws KException
    *        if an error occurs
    */
   public static StateCommand createStateCommand(UnitOfWork transaction, Repository repository,
                                                 StateCommandAggregate stateCommandAgg,
                                                 String stateCommandType,
                                                 String commandId,
                                                 Map<String, String> arguments) throws KException {
       ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
       ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
       ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
       ArgCheck.isNotNull(commandId, "commandId"); //$NON-NLS-1$
       ArgCheck.isNotNull(arguments, "arguments"); //$NON-NLS-1$

       
       try {
           if (stateCommandAgg.hasChild(transaction, stateCommandType))
               stateCommandAgg.removeChild(transaction, stateCommandType);

           KomodoObject stateCmdObject = stateCommandAgg.addChild(transaction, stateCommandType,
                                                                              KomodoLexicon.StateCommand.NODE_TYPE);
           StateCommand stateCmd = StateCommand.RESOLVER.resolve(transaction, stateCmdObject);
           stateCmd.setId(transaction, commandId);
           stateCmd.setArguments(transaction, arguments);
           return stateCmd;
       } catch ( final Exception e ) {
           throw handleError( e );
       }
   }

    private static void setCreateStatementProperties( final UnitOfWork transaction,
                                                      final KomodoObject kobject ) throws KException {
        assert ( transaction != null );

        /*

        - ddl:expression (string) mandatory             // The string fragment encompassing the statement expression.
        - ddl:originalExpression (string)               // The string fragment encompassing the original statement expression.
        - ddl:startLineNumber (long) mandatory          // The starting line number for the statement
        - ddl:startColumnNumber (long) mandatory        // The starting column number for the statement
        - ddl:startCharIndex (long) mandatory           // The starting content character index for the statement
        - ddl:length (long)  mandatory                  // The string length

         */

        kobject.setProperty( transaction, StandardDdlLexicon.DDL_EXPRESSION, "komodo created model" ); //$NON-NLS-1$
        kobject.setProperty( transaction, StandardDdlLexicon.DDL_ORIGINAL_EXPRESSION, "komodo created model" ); //$NON-NLS-1$
        kobject.setProperty( transaction, StandardDdlLexicon.DDL_START_LINE_NUMBER, 0L );
        kobject.setProperty( transaction, StandardDdlLexicon.DDL_START_COLUMN_NUMBER, 0L );
        kobject.setProperty( transaction, StandardDdlLexicon.DDL_START_CHAR_INDEX, 0L );
        kobject.setProperty( transaction, StandardDdlLexicon.DDL_LENGTH, 0L );
    }

    private RelationalModelFactory() {
        // nothing to do
    }
}
