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

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.core.visitor.DdlNodeVisitor;
import org.komodo.metadata.MetadataInstance;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.internal.ModelSourceImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;
import org.teiid.modeshape.sequencer.vdb.lexicon.CoreLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * An implementation of a relational model.
 */
public class ModelImpl extends RelationalObjectImpl implements Model {
    /**
     * An empty array of tables.
     */
    static final TableImpl[] NO_TABLES = new TableImpl[0];
    
    /**
     * An empty array of views.
     */
    static final ViewImpl[] NO_VIEWS = new ViewImpl[0];
	
    /**
     * An empty array of model sources.
     */
    static final ModelSourceImpl[] NO_SOURCES = new ModelSourceImpl[0];
	
    /**
     * The resolver of a {@link Model}.
     */
    public static final TypeResolver< ModelImpl > RESOLVER = new TypeResolver< ModelImpl >() {

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
        public Class< ModelImpl > owningClass() {
            return ModelImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( kobject, VdbLexicon.Vdb.DECLARATIVE_MODEL );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public ModelImpl resolve( final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Model.TYPE_ID ) {
                return ( ModelImpl )kobject;
            }

            return new ModelImpl( kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };


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
    public KomodoType getTypeIdentifier( ) {
        return Model.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public ModelSourceImpl addSource(
                                  final String sourceName ) throws KException {
        return RelationalModelFactory.createModelSource( getTransaction(), getRepository(), this, sourceName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public TableImpl addTable(
                           final String tableName ) throws KException {
        return RelationalModelFactory.createTable( getTransaction(), getRepository(), this, tableName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#addView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public ViewImpl addView(
                         final String viewName ) throws KException {
        return RelationalModelFactory.createView( getTransaction(), getRepository(), this, viewName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(java.lang.String)
     */
    @Override
    public KomodoObject getChild( String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        // check sources
        final KomodoObject[] matches = getSources( name );

        if ( matches.length != 0 ) {
            return matches[ 0 ];
        }

        // other child types do not have grouping nodes
        return super.getChild( name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public RelationalObjectImpl getChild( final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if (VdbLexicon.Source.SOURCE.equals( typeName )) {
            final ModelSourceImpl[] sources = getSources( name );

            if ( sources.length != 0 ) {
                return sources[ 0 ];
            }
        } else if (CreateTable.TABLE_STATEMENT.equals( typeName )) {
            final TableImpl[] tables = getTables( name );

            if ( tables.length != 0 ) {
                return tables[ 0 ];
            }
        } else if (CreateTable.VIEW_STATEMENT.equals( typeName )) {
            final ViewImpl[] views = getViews( name );

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
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject[] sources = getSources( namePatterns );
        final KomodoObject[] tables = getTables( namePatterns );
        final KomodoObject[] views = getViews( namePatterns );

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
     * @see org.komodo.relational.model.Model#getDescription()
     */
    @Override
    public String getDescription() throws KException {
        return getObjectProperty( getTransaction(), PropertyValueType.STRING, "getDescription", VdbLexicon.Vdb.DESCRIPTION ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getMetadataType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    public String getMetadataType() throws KException {
        final String result = getObjectProperty( getTransaction(), PropertyValueType.STRING, "getMetadataType", //$NON-NLS-1$
                                                 VdbLexicon.Model.METADATA_TYPE );
        // if no metadata type return default value if there is a model definition
        if ( StringUtils.isBlank( result ) ) {
            return StringUtils.isBlank( getModelDefinition( ) ) ? EMPTY_STRING : DEFAULT_METADATA_TYPE;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getModelDefinition(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getModelDefinition() throws KException {
        final String modelDefn = getObjectProperty( getTransaction(), PropertyValueType.STRING, "getModelDefinition", //$NON-NLS-1$
                                                    VdbLexicon.Model.MODEL_DEFINITION );
        return modelDefn == null ? EMPTY_STRING : modelDefn;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getModelType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Type getModelType() throws KException {
        final String value = getObjectProperty( getTransaction(), PropertyValueType.STRING, "getModelType", //$NON-NLS-1$
                                                CoreLexicon.JcrId.MODEL_TYPE );
        final Type modelType = ( ( value == null ) ? null : Type.valueOf( value ) );
        return ( ( modelType == null ) ? Type.DEFAULT_VALUE : modelType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#getSources(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public ModelSourceImpl[] getSources(
                                     final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getSourcesGroupingNode( );

        if ( grouping != null ) {
            final List< ModelSourceImpl > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( namePatterns ) ) {
                final ModelSourceImpl source = new ModelSourceImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
                temp.add( source );
            }

            return temp.toArray( new ModelSourceImpl[ temp.size() ] );
        }

        return NO_SOURCES;
    }

    private KomodoObject getSourcesGroupingNode() {
        try {
            final KomodoObject[] groupings = getRawChildren( getTransaction(), VdbLexicon.Vdb.SOURCES );

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
    public TableImpl[] getTables(
                              final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< TableImpl > result = new ArrayList< TableImpl >();

        for ( final KomodoObject kobject : super.getChildrenOfType( CreateTable.TABLE_STATEMENT, namePatterns ) ) {
            final TableImpl table = new TableImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
            result.add( table );
        }

        if ( result.isEmpty() ) {
            return NO_TABLES;
        }

        return result.toArray( new TableImpl[ result.size() ] );
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
     * @see org.komodo.relational.model.Model#getViews(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public ViewImpl[] getViews(
                            final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< ViewImpl > result = new ArrayList< ViewImpl >();

        for ( final KomodoObject kobject : super.getChildrenOfType( CreateTable.VIEW_STATEMENT, namePatterns ) ) {
            final ViewImpl view = new ViewImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
            result.add( view );
        }

        if ( result.isEmpty() ) {
            return NO_VIEWS;
        }

        return result.toArray( new ViewImpl[ result.size() ] );
    }
    
    @Override
    public ViewImpl getView( final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        
        if (super.hasChild( name, CreateTable.VIEW_STATEMENT)) {
            KomodoObject child = super.getChild( name, CreateTable.VIEW_STATEMENT);
            return ViewImpl.RESOLVER.resolve( child );
        }
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent()
     */
    @Override
    public VdbImpl getParent() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( );
        final VdbImpl result = VdbImpl.RESOLVER.resolve( parent );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(java.lang.String)
     */
    @Override
    public boolean hasChild( final String name ) throws KException {
        if ( VdbLexicon.Vdb.SOURCES.equals( name ) ) {
            return false; // use hasRawChild
        }

        return ( super.hasChild( name ) || ( getSources( name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final String name,
                             final String typeName ) throws KException {
        if ( VdbLexicon.Source.SOURCE.equals( typeName ) ) {
            return ( getSources( name ).length != 0 );
        }

        return super.hasChild( name, typeName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren()
     */
    @Override
    public boolean hasChildren( ) throws KException {
        // short-circuit with call to super (will also return the sources grouping node)
        // call to getChildren does not return source grouping node
        return ( super.hasChildren( ) && ( getChildren( ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#isVisible(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isVisible() throws KException {
        final Boolean value = getObjectProperty( getTransaction(), PropertyValueType.BOOLEAN, "isVisible", VdbLexicon.Model.VISIBLE ); //$NON-NLS-1$

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
    public void removeSource(
                              final String sourceToRemove ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( sourceToRemove, "sourceToRemove" ); //$NON-NLS-1$

        final ModelSourceImpl[] sources = getSources( sourceToRemove );

        if ( sources.length == 0 ) {
            throw new KException( Messages.getString( Relational.MODEL_SOURCE_NOT_FOUND_TO_REMOVE, sourceToRemove ) );
        }

        // remove first occurrence
        sources[ 0 ].remove( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeTable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTable(
                             final String tableName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( tableName, "tableName" ); //$NON-NLS-1$

        final TableImpl[] tables = getTables( tableName );

        if ( tables.length == 0 ) {
            throw new KException( Messages.getString( Relational.TABLE_NOT_FOUND_TO_REMOVE, tableName ) );
        }

        // remove first occurrence
        tables[ 0 ].remove( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#removeView(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeView(
                            final String viewName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( viewName, "viewName" ); //$NON-NLS-1$

        final ViewImpl[] views = getViews( viewName );

        if ( views.length == 0 ) {
            throw new KException( Messages.getString( Relational.VIEW_NOT_FOUND_TO_REMOVE, viewName ) );
        }

        // remove first occurrence
        views[ 0 ].remove( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setDescription(java.lang.String)
     */
    @Override
    public void setDescription(
                                final String newDescription ) throws KException {
        setObjectProperty( getTransaction(), "setDescription", VdbLexicon.Vdb.DESCRIPTION, newDescription ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setMetadataType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    public void setMetadataType(
                                 final String newMetadataType ) throws KException {
        String value = newMetadataType;

        if ( StringUtils.isBlank( newMetadataType ) ) {
            value = ( StringUtils.isBlank( getModelDefinition( ) ) ? value : DEFAULT_METADATA_TYPE );
        }

        setObjectProperty( getTransaction(), "setMetadataType", VdbLexicon.Model.METADATA_TYPE, value ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setModelDefinition(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setModelDefinition(
                                    final String modelDefinition ) throws KException {
        setObjectProperty( getTransaction(), "setModelDefinition", VdbLexicon.Model.MODEL_DEFINITION, modelDefinition ); //$NON-NLS-1$

        if ( StringUtils.isBlank( modelDefinition ) ) {
            // if no model definition make sure there isn't a metadata type
            if ( !StringUtils.isBlank( getMetadataType( ) ) ) {
                setMetadataType( EMPTY_STRING );
            }
        } else {
            // if there is a model definition make sure there is a metadata type
            if ( StringUtils.isBlank( getMetadataType( ) ) ) {
                setMetadataType( DEFAULT_METADATA_TYPE );
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
    public void setModelType(
                              final Type newModelType ) throws KException {
        final Type modelType = ( ( newModelType == null ) ? Type.DEFAULT_VALUE : newModelType );
        setObjectProperty( getTransaction(), "setModelType", CoreLexicon.JcrId.MODEL_TYPE, modelType.name() ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Model#setVisible(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setVisible(
                            final boolean newVisible ) throws KException {
        setObjectProperty( getTransaction(), "setVisible", VdbLexicon.Model.VISIBLE, newVisible ); //$NON-NLS-1$
    }

    private String exportDdl(UnitOfWork transaction, Properties exportProperties) throws Exception {
    	MetadataInstance metadata = getRepository().getMetadataInstance();
        DdlNodeVisitor visitor = new DdlNodeVisitor(metadata.getDataTypeService(), false);
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
    public byte[] export(Properties exportProperties) throws KException {
        ArgCheck.isNotNull(getTransaction());

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("modelimpl-export: transaction = {0}", getTransaction().getName()); //$NON-NLS-1$
        }

        try {
            String result = exportDdl(getTransaction(), exportProperties);

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug("ModelImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                             getTransaction().getName(),
                             result);
            }

            return result.getBytes();

        } catch (final Exception e) {
            throw handleError(e);
        }
    }
    
    @Override
    public VdbImpl getRelationalParent() throws KException {
    	return this.getParent();
    }

}
