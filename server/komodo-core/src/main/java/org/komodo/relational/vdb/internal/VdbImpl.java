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
package org.komodo.relational.vdb.internal;

import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.DescriptorImpl;
import org.komodo.core.repository.KPropertyFactory;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyDescriptorImpl;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.core.visitor.VdbNodeVisitor;
import org.komodo.metadata.MetadataInstance;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;

/**
 * An implementation of a virtual database manifest.
 */
public class VdbImpl extends RelationalObjectImpl implements Vdb {
	
    /**
     * The resolver of a {@link Vdb}.
     */
    public static final TypeResolver< Vdb > RESOLVER = new TypeResolver< Vdb >() {

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
        public Class< VdbImpl > owningClass() {
            return VdbImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Vdb resolve( final UnitOfWork transaction,
                            final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Vdb.TYPE_ID ) {
                return ( Vdb )kobject;
            }

            return new VdbImpl( transaction, RepositoryImpl.getRepository(transaction), kobject.getAbsolutePath() );
        }

    };

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Model.IDENTIFIER,
                                                                         Translator.IDENTIFIER, VdbImport.IDENTIFIER };

	/**
	 * Include the special properties into the primary type descriptor.
	 *
	 * @see SpecialProperty
	 */
    class PrimaryTypeDescriptor extends DescriptorImpl {

        private final Descriptor delegate;

        PrimaryTypeDescriptor( final Repository repository,
                               final Descriptor delegate ) {
            super( repository, delegate.getName() );
            this.delegate = delegate;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.core.repository.DescriptorImpl#getPropertyDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
         */
        @Override
        public PropertyDescriptor[] getPropertyDescriptors( final UnitOfWork transaction ) throws KException {
            final PropertyDescriptor[] propDescriptors = this.delegate.getPropertyDescriptors( transaction );

            final PropertyDescriptor[] result = new PropertyDescriptor[ propDescriptors.length + specialProperties.length ];
            System.arraycopy( propDescriptors, 0, result, 0, propDescriptors.length );

            int i = propDescriptors.length;

            for ( final SpecialProperty prop : specialProperties ) {
                result[i++] = prop.getDescriptor();
            }

            return result;
        }

    }

    private class SpecialProperty {

        private final String teiidName;

        private SpecialProperty( final String teiidName ) {
            this.teiidName = teiidName;
        }

        PropertyDescriptor getDescriptor() throws KException {
            final PropertyValueType type = ( this == specialProperties[QUERY_TIMEOUT] ) ? PropertyValueType.LONG : PropertyValueType.STRING;
            KPropertyFactory factory = VdbImpl.this.getPropertyFactory();
            return new PropertyDescriptorImpl( false, true, false, toTeiidName(), type, null, factory );
        }

        String toTeiidName() {
            return this.teiidName;
        }

    }

    private static final int ALLOWED_LANGUAGES = 0;
    private static final int AUTHENTICATION_TYPE = 1;
    private static final int GSS_PATTERN = 2;
    private static final int PASSWORD_PATTERN = 3;
    private static final int QUERY_TIMEOUT = 4;

    private final SpecialProperty[] specialProperties = new SpecialProperty[] {
        new SpecialProperty(ALLOWED_LANGUAGES_TEIIDNAME), //$NON-NLS-1$
        new SpecialProperty(AUTHENTICATION_TYPE_TEIIDNAME), //$NON-NLS-1$
        new SpecialProperty(GSS_PATTERN_TEIIDNAME), //$NON-NLS-1$
        new SpecialProperty(PASSWORD_PATTERN_TEIIDNAME), //$NON-NLS-1$
        new SpecialProperty(QUERY_TIMEOUT_TEIIDNAME), //$NON-NLS-1$
        new SpecialProperty(SECURITY_DOMAIN_TEIIDNAME), //$NON-NLS-1$
    };

    private class VdbManifestImpl implements VdbManifest {

        private final String xml;

        VdbManifestImpl( final UnitOfWork transaction,
                         final VdbImpl vdb, final Properties exportProperties ) throws KException {
            final StringWriter writer = new StringWriter();

            try {
                final XMLOutputFactory xof = XMLOutputFactory.newInstance();
                final XMLStreamWriter xsw = xof.createXMLStreamWriter(writer);
                MetadataInstance metadata = getRepository().getMetadataInstance();
                final VdbNodeVisitor visitor = new VdbNodeVisitor(metadata.getDataTypeService(), xsw);
                if( exportProperties != null && !exportProperties.isEmpty() ) {
                	boolean useTabs = exportProperties.containsKey(Exportable.USE_TABS_PROP_KEY);
                	visitor.setShowTabs(useTabs);
                }
                visitor.visit(transaction, vdb);
            } catch (final Exception e) {
                throw new KException(e);
            }

            // Create an XML Document from the filled writer
            this.xml = writer.toString().trim();

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug("VdbImpl#VdbManifestImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                             transaction.getName(),
                             this.xml);
            }
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.vdb.Vdb.VdbManifest#asDocument()
         */
        @Override
        public Document asDocument() throws KException {
            return FileUtils.createDocument(this.xml);
        }

        @Override
        public String getName(UnitOfWork transaction) throws KException {
            return getVdbName(transaction);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
         */
        @Override
        public byte[] export( final UnitOfWork transaction, Properties properties) {
            return this.xml == null ? new byte[0] : this.xml.getBytes();
        }

    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public VdbImpl( final UnitOfWork uow,
                    final Repository repository,
                    final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Vdb.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public VdbImport addImport( final UnitOfWork transaction,
                                final String vdbName ) throws KException {
        return RelationalModelFactory.createVdbImport( transaction, getRepository(), this, vdbName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Model addModel( final UnitOfWork transaction,
                           final String modelName ) throws KException {
        return RelationalModelFactory.createModel( transaction, getRepository(), this, modelName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public Translator addTranslator( final UnitOfWork transaction,
                                     final String translatorName,
                                     final String translatorType ) throws KException {
        return RelationalModelFactory.createTranslator( transaction, getRepository(), this, translatorName, translatorType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#createManifest(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public VdbManifest createManifest( final UnitOfWork transaction,
                                       final Properties properties ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final VdbManifest result = new VdbManifestImpl( transaction, this, properties );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork transaction,
                          final Properties properties ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final byte[] result = createManifest( transaction, properties ).export( transaction, properties );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getAllowedLanguages(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getAllowedLanguages( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getAllowedLanguages", //$NON-NLS-1$
                                  specialProperties[ALLOWED_LANGUAGES].toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getAuthenticationType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getAuthenticationType( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getAuthenticationType", //$NON-NLS-1$
                                  specialProperties[AUTHENTICATION_TYPE].toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name ) throws KException {
        // check models
    	KomodoObject[] kids = getModels( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check translators
        kids = getTranslators( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check imports
        kids = getImports( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.core.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork transaction,
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( typeName ) ) {
            final KomodoObject[] imports = getImports( transaction, name );

            if ( imports.length != 0 ) {
                return imports[ 0 ];
            }
        } else if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( typeName ) ) {
            final KomodoObject[] models = getModels( transaction, name );

            if ( models.length != 0 ) {
                return models[ 0 ];
            }
        } else if ( VdbLexicon.Translator.TRANSLATOR.equals( typeName ) ) {
            final KomodoObject[] translators = getTranslators( transaction, name );

            if ( translators.length != 0 ) {
                return translators[ 0 ];
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

        final VdbImport[] imports = getImports( transaction, namePatterns );
        final Model[] models = getModels( transaction, namePatterns );
        final Translator[] translators = getTranslators( transaction, namePatterns );

        final KomodoObject[] result = new KomodoObject[ imports.length + models.length
                                                        + translators.length ];
        System.arraycopy( imports, 0, result, 0, imports.length );
        System.arraycopy( models, 0, result, imports.length, models.length );
        System.arraycopy( translators,
                          0,
                          result,
                          imports.length + models.length,
                          translators.length );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( type ) ) {
            result = getImports( transaction, namePatterns );
        } else if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( type ) ) {
            result = getModels( transaction, namePatterns );
        } else if ( VdbLexicon.Translator.TRANSLATOR.equals( type ) ) {
            result = getTranslators( transaction, namePatterns );
        } else {
            result = super.getChildrenOfType(transaction, type, namePatterns);
        }

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
     * @see org.komodo.relational.vdb.Vdb#getConnectionType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getConnectionType( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getConnectionType", VdbLexicon.Vdb.CONNECTION_TYPE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getDescription", VdbLexicon.Vdb.DESCRIPTION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getGssPattern(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getGssPattern( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getGssPattern", //$NON-NLS-1$
                                  specialProperties[GSS_PATTERN].toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getImports(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public VdbImport[] getImports( final UnitOfWork transaction,
                                   final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getImportsGroupingNode( transaction);

        if ( grouping != null ) {
            final List< VdbImport > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final VdbImport vdbImport = new VdbImportImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( vdbImport );
            }

            return temp.toArray( new VdbImport[ temp.size() ] );
        }

        return VdbImport.NO_IMPORTS;
    }

    private KomodoObject getImportsGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.Vdb.IMPORT_VDBS );

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
     * @see org.komodo.relational.vdb.Vdb#getModels(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Model[] getModels( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Model > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction,
                                                                    VdbLexicon.Vdb.DECLARATIVE_MODEL,
                                                                    namePatterns ) ) {
            final Model model = new ModelImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( model );
        }

        if ( result.isEmpty() ) {
            return Model.NO_MODELS;
        }

        return result.toArray( new Model[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOriginalFilePath( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getOriginalFilePath", VdbLexicon.Vdb.ORIGINAL_FILE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getPasswordPattern(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getPasswordPattern( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getPasswordPattern", //$NON-NLS-1$
                                  specialProperties[PASSWORD_PATTERN].toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getPropertyDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public PropertyDescriptor getPropertyDescriptor( final UnitOfWork transaction,
                                                     final String propName ) throws KException {
        for (SpecialProperty prop : specialProperties) {
            if (prop.toTeiidName().equals(propName))
                return prop.getDescriptor();
        }

        return super.getPropertyDescriptor( transaction, propName );        
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Descriptor getPrimaryType( final UnitOfWork transaction ) throws KException {
        return new PrimaryTypeDescriptor( getRepository(), super.getPrimaryType( transaction ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getQueryTimeout(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getQueryTimeout( final UnitOfWork transaction ) throws KException {
        final Integer value = getObjectProperty( transaction,
                                                 PropertyValueType.INTEGER,
                                                 "getQueryTimeout", //$NON-NLS-1$
                                                 specialProperties[QUERY_TIMEOUT].toTeiidName() );
        return ( value == null ) ? -1 : value;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getTranslators(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public Translator[] getTranslators( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getTranslatorsGroupingNode( transaction );

        if ( grouping != null ) {
            final List< Translator > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( transaction, namePatterns ) ) {
                final Translator translator = new TranslatorImpl( transaction, getRepository(), kobject.getAbsolutePath() );
                temp.add( translator );
            }

            return temp.toArray( new Translator[ temp.size() ] );
        }

        return Translator.NO_TRANSLATORS;
    }

    private KomodoObject getTranslatorsGroupingNode( final UnitOfWork transaction ) {
        try {
            final KomodoObject[] groupings = getRawChildren( transaction, VdbLexicon.Vdb.TRANSLATORS );

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
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getVdbName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getVdbName( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getVdbName", VdbLexicon.Vdb.NAME); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.INTEGER, "getVersion", VdbLexicon.Vdb.VERSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        return ( ( getImports( transaction, name ).length != 0 )
                 || ( getModels( transaction, name ).length != 0 )
                 || ( getTranslators( transaction, name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( typeName ) ) {
            return ( getImports( transaction, name ).length != 0 );
        }

        if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( typeName ) ) {
            return ( getModels( transaction, name ).length != 0 );
        }

        if ( VdbLexicon.Translator.TRANSLATOR.equals( typeName ) ) {
            return ( getTranslators( transaction, name ).length != 0 );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork transaction ) throws KException {
        if ( super.hasChildren( transaction ) ) {
            return ( ( getImports( transaction ).length != 0 )
            || ( getModels( transaction ).length != 0 )
            || ( getTranslators( transaction ).length != 0 ) );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#isPreview(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isPreview( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.BOOLEAN, "isPreview", VdbLexicon.Vdb.PREVIEW); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeImport( final UnitOfWork transaction,
                              final String importToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( importToRemove, "importToRemove" ); //$NON-NLS-1$

        final VdbImport[] vdbImports = getImports( transaction, importToRemove );

        if ( vdbImports.length == 0 ) {
            throw new KException( Messages.getString( Relational.VDB_IMPORT_NOT_FOUND_TO_REMOVE, importToRemove ) );
        }

        // remove first occurrence
        vdbImports[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeModel( final UnitOfWork transaction,
                             final String modelToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( modelToRemove, "modelToRemove" ); //$NON-NLS-1$

        final Model[] models = getModels( transaction, modelToRemove );

        if ( models.length == 0 ) {
            throw new KException( Messages.getString( Relational.MODEL_NOT_FOUND_TO_REMOVE, modelToRemove ) );
        }

        // remove first occurrence
        models[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTranslator( final UnitOfWork transaction,
                                  final String translatorToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( translatorToRemove, "translatorToRemove" ); //$NON-NLS-1$

        final Translator[] translators = getTranslators( transaction, translatorToRemove );

        if ( translators.length == 0 ) {
            throw new KException( Messages.getString( Relational.TRANSLATOR_NOT_FOUND_TO_REMOVE, translatorToRemove ) );
        }

        // remove first occurrence
        translators[ 0 ].remove( transaction );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void rename( final UnitOfWork transaction,
                        final String newName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( newName, "newName" ); //$NON-NLS-1$

        super.rename( transaction, newName );
        String shortName = newName;
        if(newName.contains(StringConstants.FORWARD_SLASH)) {
            shortName = newName.substring(shortName.lastIndexOf(StringConstants.FORWARD_SLASH)+1);
        }
        setVdbName( transaction, shortName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setAllowedLanguages(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setAllowedLanguages( final UnitOfWork transaction,
                                     final String newAllowedLanguages ) throws KException {
        setObjectProperty( transaction,
                           "setAllowedLanguages", //$NON-NLS-1$
                           specialProperties[ALLOWED_LANGUAGES].toTeiidName(),
                           newAllowedLanguages );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setAuthenticationType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setAuthenticationType( final UnitOfWork transaction,
                                       final String newAuthenticationType ) throws KException {
        setObjectProperty( transaction,
                           "setAuthenticationType", //$NON-NLS-1$
                           specialProperties[AUTHENTICATION_TYPE].toTeiidName(),
                           newAuthenticationType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setConnectionType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setConnectionType( final UnitOfWork uow,
                                   final String newConnectionType ) throws KException {
        setObjectProperty(uow, "setConnectionType", VdbLexicon.Vdb.CONNECTION_TYPE, newConnectionType); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setDescription(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork uow,
                                final String newDescription ) throws KException {
        setObjectProperty(uow, "setDescription", VdbLexicon.Vdb.DESCRIPTION, newDescription); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setGssPattern(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setGssPattern( final UnitOfWork transaction,
                               final String newGssPattern ) throws KException {
        setObjectProperty( transaction, "setGssPattern", specialProperties[GSS_PATTERN].toTeiidName(), newGssPattern ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setOriginalFilePath( final UnitOfWork uow,
                                     final String newOriginalFilePath ) throws KException {
        ArgCheck.isNotEmpty(newOriginalFilePath, "newOriginalFilePath"); //$NON-NLS-1$
        setObjectProperty(uow, "setOriginalFilePath", VdbLexicon.Vdb.ORIGINAL_FILE, newOriginalFilePath); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setPasswordPattern(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setPasswordPattern( final UnitOfWork transaction,
                                    final String newPasswordPattern ) throws KException {
        setObjectProperty( transaction, "setPasswordPattern", specialProperties[PASSWORD_PATTERN].toTeiidName(), newPasswordPattern ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setPreview(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setPreview( final UnitOfWork uow,
                            final boolean newPreview ) throws KException {
        setObjectProperty(uow, "setPreview", VdbLexicon.Vdb.PREVIEW, newPreview); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setQueryTimeout(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setQueryTimeout( final UnitOfWork transaction,
                                 final int newQueryTimeout ) throws KException {
        // setting new value to null if timeout is less than zero to delete property
        Object newValue = null;

        if ( newQueryTimeout > -1 ) {
            newValue = newQueryTimeout;
        }

        setObjectProperty( transaction, "setQueryTimeout", specialProperties[QUERY_TIMEOUT].toTeiidName(), newValue ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setVdbName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setVdbName( final UnitOfWork uow,
                            final String newVdbName ) throws KException {
        setObjectProperty(uow, "setVdbName", VdbLexicon.Vdb.NAME, newVdbName); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setVersion(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setVersion( final UnitOfWork uow,
                            final int newVersion ) throws KException {
        setObjectProperty(uow, "setVersion", VdbLexicon.Vdb.VERSION, newVersion); //$NON-NLS-1$
    }

    @Override
    public DeployStatus deploy(UnitOfWork uow) {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        DeployStatus status = new DeployStatus();

        try {
            String vdbName = getName(uow);
            status.addProgressMessage("Starting deployment of vdb " + vdbName); //$NON-NLS-1$

            status.addProgressMessage("Attempting to deploy VDB " + vdbName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$

            // Get VDB content
            byte[] vdbXml = export(uow, null);
            if (vdbXml == null || vdbXml.length == 0) {
                status.addErrorMessage("VDB " + vdbName + " content is empty"); //$NON-NLS-1$ //$NON-NLS-2$
                return status;
            }

            String vdbToDeployName = getName(uow);
            String vdbDeploymentName = vdbToDeployName + VDB_DEPLOYMENT_SUFFIX;
            MetadataInstance metadata = getRepository().getMetadataInstance();
            metadata.deployDynamicVdb(getName(uow), vdbDeploymentName, new ByteArrayInputStream(vdbXml));

            status.addProgressMessage("VDB deployed " + vdbName + " to teiid"); //$NON-NLS-1$ //$NON-NLS-2$
        } catch (Exception ex) {
            status.addErrorMessage(ex);
        }

        
        return status;
    }
    
    @Override
    public Model getModel(UnitOfWork transaction, String name) throws KException {
    	ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (!hasChild(transaction, name, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
        	return null;
        }
        
        final KomodoObject kobject = super.getChild( transaction,
                                                                    VdbLexicon.Vdb.DECLARATIVE_MODEL,
                                                                    name );
        return new ModelImpl( transaction, getRepository(), kobject.getAbsolutePath() );
    }
    
}
