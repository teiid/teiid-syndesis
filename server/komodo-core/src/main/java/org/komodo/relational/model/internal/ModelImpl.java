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

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.komodo.core.visitor.DdlNodeVisitor;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.LexiconConstants.CoreLexicon;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a relational model.
 */
public final class ModelImpl extends RelationalObjectImpl implements Model {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { ModelSource.IDENTIFIER,
                                                                      Table.IDENTIFIER,
                                                                      View.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a model
     */
    public ModelImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String workspacePath ) throws KException {
        super( uow, repository, workspacePath );
    }

    @Override
    public KomodoType getTypeIdentifier( UnitOfWork uow ) {
        return Model.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public ModelSource addSource( final UnitOfWork transaction,
                                  final String sourceName ) throws KException {
        return RelationalModelFactory.createModelSource( transaction, getRepository(), this, sourceName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Table addTable( final UnitOfWork transaction,
                           final String tableName ) throws KException {
        return RelationalModelFactory.createTable( transaction, getRepository(), this, tableName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public View addView( final UnitOfWork transaction,
                         final String viewName ) throws KException {
        return RelationalModelFactory.createView( transaction, getRepository(), this, viewName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getChild( UnitOfWork transaction,
                                  String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        // check sources
        final KomodoObject[] matches = getSources( transaction, name );

        if ( matches.length != 0 ) {
            return matches[ 0 ];
        }

        // other child types do not have grouping nodes
        return super.getChild( transaction, name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if (VdbLexicon.Source.SOURCE.equals( typeName )) {
            final KomodoObject[] sources = getSources( transaction, name );

            if ( sources.length != 0 ) {
                return sources[ 0 ];
            }
        } else if (CreateTable.TABLE_STATEMENT.equals( typeName )) {
            final KomodoObject[] tables = getTables( transaction, name );

            if ( tables.length != 0 ) {
                return tables[ 0 ];
            }
        } else if (CreateTable.VIEW_STATEMENT.equals( typeName )) {
            final KomodoObject[] views = getViews( transaction, name );

            if ( views.length != 0 ) {
                return views[ 0 ];
            }
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.core.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork transaction,
                                       final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject[] sources = getSources( transaction, namePatterns );
        final KomodoObject[] tables = getTables( transaction, namePatterns );
        final KomodoObject[] views = getViews( transaction, namePatterns );

        final KomodoObject[] result = new KomodoObject[ sources.length
                                                        + tables.length
                                                        + views.length ];
        System.arraycopy( sources, 0, result, 0, sources.length );
        System.arraycopy( tables, 0, result, sources.length, tables.length );
        System.arraycopy( views, 0, result, sources.length + tables.length, views.length );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction, PropertyValueType.STRING, "getDescription", VdbLexicon.Vdb.DESCRIPTION ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getMetadataType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getMetadataType( final UnitOfWork transaction ) throws KException {
        final String result = getObjectProperty( transaction, PropertyValueType.STRING, "getMetadataType", //$NON-NLS-1$
                                                 VdbLexicon.Model.METADATA_TYPE );
        // if no metadata type return default value if there is a model definition
        if ( StringUtils.isBlank( result ) ) {
            return StringUtils.isBlank( getModelDefinition( transaction ) ) ? EMPTY_STRING : DEFAULT_METADATA_TYPE;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getModelDefinition(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getModelDefinition( final UnitOfWork uow ) throws KException {
        final String modelDefn = getObjectProperty( uow, PropertyValueType.STRING, "getModelDefinition", //$NON-NLS-1$
                                                    VdbLexicon.Model.MODEL_DEFINITION );
        return modelDefn == null ? EMPTY_STRING : modelDefn;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getModelType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Type getModelType( final UnitOfWork uow ) throws KException {
        final String value = getObjectProperty( uow, PropertyValueType.STRING, "getModelType", //$NON-NLS-1$
                                                CoreLexicon.MODEL_TYPE );
        final Type modelType = ( ( value == null ) ? null : Type.valueOf( value ) );
        return ( ( modelType == null ) ? Type.DEFAULT_VALUE : modelType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getSources(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public ModelSource[] getSources( final UnitOfWork transaction,
                                     final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getSourcesGroupingNode( transaction );

        if ( grouping != null ) {
            final List< ModelSource > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final ModelSource source = new ModelSourceImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( source );
            }

            return temp.toArray( new ModelSource[ temp.size() ] );
        }

        return ModelSource.NO_SOURCES;
    }

    private KomodoObject getSourcesGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.Vdb.SOURCES );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getTables(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Table[] getTables( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Table > result = new ArrayList< Table >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, CreateTable.TABLE_STATEMENT, namePatterns ) ) {
            final Table table = new TableImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( table );
        }

        if ( result.isEmpty() ) {
            return Table.NO_TABLES;
        }

        return result.toArray( new Table[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getViews(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public View[] getViews( final UnitOfWork transaction,
                            final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< View > result = new ArrayList< View >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, CreateTable.VIEW_STATEMENT, namePatterns ) ) {
            final View view = new ViewImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( view );
        }

        if ( result.isEmpty() ) {
            return View.NO_VIEWS;
        }

        return result.toArray( new View[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Vdb getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( transaction );
        final Vdb result = Vdb.RESOLVER.resolve( transaction, parent );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        if ( VdbLexicon.Vdb.SOURCES.equals( name ) ) {
            return false; // use hasRawChild
        }

        return ( super.hasChild( transaction, name ) || ( getSources( transaction, name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) throws KException {
        if ( VdbLexicon.Source.SOURCE.equals( typeName ) ) {
            return ( getSources( transaction, name ).length != 0 );
        }

        return super.hasChild( transaction, name, typeName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork transaction ) throws KException {
        // short-circuit with call to super (will also return the sources grouping node)
        // call to getChildren does not return source grouping node
        return ( super.hasChildren( transaction ) && ( getChildren( transaction ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#isVisible(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isVisible( final UnitOfWork transaction ) throws KException {
        final Boolean value = getObjectProperty( transaction, PropertyValueType.BOOLEAN, "isVisible", VdbLexicon.Model.VISIBLE ); //$NON-NLS-1$

        if ( value == null ) {
            return DEFAULT_VISIBLE;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeSource( final UnitOfWork transaction,
                              final String sourceToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( sourceToRemove, "sourceToRemove" ); //$NON-NLS-1$

        final ModelSource[] sources = getSources( transaction, sourceToRemove );

        if ( sources.length == 0 ) {
            throw new KException( Messages.getString( Relational.MODEL_SOURCE_NOT_FOUND_TO_REMOVE, sourceToRemove ) );
        }

        // remove first occurrence
        sources[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTable( final UnitOfWork transaction,
                             final String tableName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( tableName, "tableName" ); //$NON-NLS-1$

        final Table[] tables = getTables( transaction, tableName );

        if ( tables.length == 0 ) {
            throw new KException( Messages.getString( Relational.TABLE_NOT_FOUND_TO_REMOVE, tableName ) );
        }

        // remove first occurrence
        tables[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeView( final UnitOfWork transaction,
                            final String viewName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( viewName, "viewName" ); //$NON-NLS-1$

        final View[] views = getViews( transaction, viewName );

        if ( views.length == 0 ) {
            throw new KException( Messages.getString( Relational.VIEW_NOT_FOUND_TO_REMOVE, viewName ) );
        }

        // remove first occurrence
        views[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setObjectProperty( transaction, "setDescription", VdbLexicon.Vdb.DESCRIPTION, newDescription ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setMetadataType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setMetadataType( final UnitOfWork transaction,
                                 final String newMetadataType ) throws KException {
        String value = newMetadataType;

        if ( StringUtils.isBlank( newMetadataType ) ) {
            value = ( StringUtils.isBlank( getModelDefinition( transaction ) ) ? value : DEFAULT_METADATA_TYPE );
        }

        setObjectProperty( transaction, "setMetadataType", VdbLexicon.Model.METADATA_TYPE, value ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setModelDefinition(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setModelDefinition( final UnitOfWork uow,
                                    final String modelDefinition ) throws KException {
        setObjectProperty( uow, "setModelDefinition", VdbLexicon.Model.MODEL_DEFINITION, modelDefinition ); //$NON-NLS-1$

        if ( StringUtils.isBlank( modelDefinition ) ) {
            // if no model definition make sure there isn't a metadata type
            if ( !StringUtils.isBlank( getMetadataType( uow ) ) ) {
                setMetadataType( uow, EMPTY_STRING );
            }
        } else {
            // if there is a model definition make sure there is a metadata type
            if ( StringUtils.isBlank( getMetadataType( uow ) ) ) {
                setMetadataType( uow, DEFAULT_METADATA_TYPE );
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setModelType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Model.Type)
     */
    @Override
    public void setModelType( final UnitOfWork uow,
                              final Type newModelType ) throws KException {
        final Type modelType = ( ( newModelType == null ) ? Type.DEFAULT_VALUE : newModelType );
        setObjectProperty( uow, "setModelType", CoreLexicon.MODEL_TYPE, modelType.name() ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setVisible(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setVisible( final UnitOfWork transaction,
                            final boolean newVisible ) throws KException {
        setObjectProperty( transaction, "setVisible", VdbLexicon.Model.VISIBLE, newVisible ); //$NON-NLS-1$
    }

    private String exportDdl(UnitOfWork transaction, Properties exportProperties) throws Exception {
    	MetadataInstance metadata = getRepository().getMetadataInstance();
        DdlNodeVisitor visitor = new DdlNodeVisitor(metadata.getVersion(), metadata.getDataTypeService(), false);
        visitor.visit(transaction, this);

        String result = visitor.getDdl();
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork transaction , Properties exportProperties) throws KException {
        ArgCheck.isNotNull(transaction);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("modelimpl-export: transaction = {0}", transaction.getName()); //$NON-NLS-1$
        }

        try {
            String result = exportDdl(transaction, exportProperties);

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug("ModelImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                             transaction.getName(),
                             result);
            }

            return result.getBytes();

        } catch (final Exception e) {
            throw handleError(e);
        }
    }

    @Override
    public DocumentType getDocumentType(UnitOfWork transaction) throws KException {
        return DocumentType.DDL;
    }
}
