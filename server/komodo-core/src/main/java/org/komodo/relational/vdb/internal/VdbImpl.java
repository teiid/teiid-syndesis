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

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.Descriptor;
import org.komodo.core.repository.DescriptorImpl;
import org.komodo.core.repository.KPropertyFactory;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyDescriptor;
import org.komodo.core.repository.PropertyDescriptorImpl;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.core.visitor.VdbNodeVisitor;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.DataTypeService;
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
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoType;
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
     * An empty array of translators.
     */
    static final TranslatorImpl[] NO_TRANSLATORS = new TranslatorImpl[0];
	
    /**
     * An empty array of models.
     */
    static final ModelImpl[] NO_MODELS = new ModelImpl[0];
	
    /**
     * An empty array of VDB imports.
     */
    static final VdbImportImpl[] NO_IMPORTS = new VdbImportImpl[0];
	
    /**
     * The resolver of a {@link Vdb}.
     */
    public static final TypeResolver< VdbImpl > RESOLVER = new TypeResolver< VdbImpl >() {

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
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( kobject, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public VdbImpl resolve(
                            final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Vdb.TYPE_ID ) {
                return ( VdbImpl )kobject;
            }

            return new VdbImpl( kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath() );
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
            super(getTransaction(), repository, delegate.getName() );
            this.delegate = delegate;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.core.repository.DescriptorImpl#getPropertyDescriptors()
         */
        @Override
        public PropertyDescriptor[] getPropertyDescriptors() throws KException {
            final PropertyDescriptor[] propDescriptors = this.delegate.getPropertyDescriptors( );

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

        VdbManifestImpl(
                         final VdbImpl vdb, final Properties exportProperties ) throws KException {
            final StringWriter writer = new StringWriter();

            try {
                final XMLOutputFactory xof = XMLOutputFactory.newInstance();
                final XMLStreamWriter xsw = xof.createXMLStreamWriter(writer);
                final VdbNodeVisitor visitor = new VdbNodeVisitor(new DataTypeService(), xsw);
                if( exportProperties != null && !exportProperties.isEmpty() ) {
                	boolean useTabs = exportProperties.containsKey(Exportable.USE_TABS_PROP_KEY);
                	visitor.setShowTabs(useTabs);
                }
                visitor.visit(getTransaction(), vdb);
            } catch (final Exception e) {
                throw new KException(e);
            }

            // Create an XML Document from the filled writer
            this.xml = writer.toString().trim();

            if (LOGGER.isDebugEnabled()) {
                LOGGER.debug("VdbImpl#VdbManifestImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                             getTransaction().getName(),
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
        public String getName() throws KException {
            return getVdbName();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
         */
        @Override
        public byte[] export( Properties properties) {
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
    public KomodoType getTypeIdentifier() {
        return Vdb.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public VdbImportImpl addImport(
                                final String vdbName ) throws KException {
        return RelationalModelFactory.createVdbImport( getTransaction(), getRepository(), this, vdbName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public ModelImpl addModel(
                           final String modelName ) throws KException {
        return RelationalModelFactory.createModel( getTransaction(), getRepository(), this, modelName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#addTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public TranslatorImpl addTranslator(
                                     final String translatorName,
                                     final String translatorType ) throws KException {
        return RelationalModelFactory.createTranslator( getTransaction(), getRepository(), this, translatorName, translatorType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#createManifest(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public VdbManifest createManifest(
                                       final Properties properties ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final VdbManifest result = new VdbManifestImpl( this, properties );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export(
                          final Properties properties ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final byte[] result = createManifest( properties ).export( properties );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getAllowedLanguages(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getAllowedLanguages() throws KException {
        return getObjectProperty( getTransaction(),
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
    public String getAuthenticationType() throws KException {
        return getObjectProperty( getTransaction(),
                                  PropertyValueType.STRING,
                                  "getAuthenticationType", //$NON-NLS-1$
                                  specialProperties[AUTHENTICATION_TYPE].toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(java.lang.String)
     */
    @Override
    public KomodoObject getChild(
                                  final String name ) throws KException {
        // check models
    	KomodoObject[] kids = getModels( name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check translators
        kids = getTranslators( name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check imports
        kids = getImports( name );

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
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(java.lang.String, java.lang.String)
     */
    @Override
    public RelationalObjectImpl getChild(
                                  final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( typeName ) ) {
            final VdbImportImpl[] imports = getImports( name );

            if ( imports.length != 0 ) {
                return imports[ 0 ];
            }
        } else if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( typeName ) ) {
            final ModelImpl[] models = getModels( name );

            if ( models.length != 0 ) {
                return models[ 0 ];
            }
        } else if ( VdbLexicon.Translator.TRANSLATOR.equals( typeName ) ) {
            final TranslatorImpl[] translators = getTranslators( name );

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
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren(
                                       final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final VdbImport[] imports = getImports( namePatterns );
        final Model[] models = getModels( namePatterns );
        final Translator[] translators = getTranslators( namePatterns );

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
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(java.lang.String,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType(
                                             final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( type ) ) {
            result = getImports( namePatterns );
        } else if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( type ) ) {
            result = getModels( namePatterns );
        } else if ( VdbLexicon.Translator.TRANSLATOR.equals( type ) ) {
            result = getTranslators( namePatterns );
        } else {
            result = super.getChildrenOfType(type, namePatterns);
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
    public String getConnectionType() throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.STRING, "getConnectionType", VdbLexicon.Vdb.CONNECTION_TYPE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getDescription()
     */
    @Override
    public String getDescription() throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.STRING, "getDescription", VdbLexicon.Vdb.DESCRIPTION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getGssPattern(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getGssPattern() throws KException {
        return getObjectProperty( getTransaction(),
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
    public VdbImportImpl[] getImports(
                                   final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getImportsGroupingNode();

        if ( grouping != null ) {
            final List< VdbImportImpl > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( namePatterns ) ) {
                final VdbImportImpl vdbImport = new VdbImportImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
                temp.add( vdbImport );
            }

            return temp.toArray( new VdbImportImpl[ temp.size() ] );
        }

        return NO_IMPORTS;
    }

    private KomodoObject getImportsGroupingNode() {
        try {
            final KomodoObject[] groupings = getRawChildren( getTransaction(), VdbLexicon.Vdb.IMPORT_VDBS );

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
    public ModelImpl[] getModels(
                              final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< ModelImpl > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( VdbLexicon.Vdb.DECLARATIVE_MODEL,
                                                                    namePatterns ) ) {
            final ModelImpl model = new ModelImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
            result.add( model );
        }

        if ( result.isEmpty() ) {
            return NO_MODELS;
        }

        return result.toArray( new ModelImpl[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOriginalFilePath() throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.STRING, "getOriginalFilePath", VdbLexicon.Vdb.ORIGINAL_FILE); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getPasswordPattern(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getPasswordPattern() throws KException {
        return getObjectProperty( getTransaction(),
                                  PropertyValueType.STRING,
                                  "getPasswordPattern", //$NON-NLS-1$
                                  specialProperties[PASSWORD_PATTERN].toTeiidName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getPrimaryType()
     */
    @Override
    public Descriptor getPrimaryType() throws KException {
        return new PrimaryTypeDescriptor( getRepository(), super.getPrimaryType( ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getQueryTimeout(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getQueryTimeout() throws KException {
        final Integer value = getObjectProperty( getTransaction(),
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
    public TranslatorImpl[] getTranslators(
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getTranslatorsGroupingNode( );

        if ( grouping != null ) {
            final List< TranslatorImpl > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( namePatterns ) ) {
                final TranslatorImpl translator = new TranslatorImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
                temp.add( translator );
            }

            return temp.toArray( new TranslatorImpl[ temp.size() ] );
        }

        return NO_TRANSLATORS;
    }

    private KomodoObject getTranslatorsGroupingNode() {
        try {
            final KomodoObject[] groupings = getRawChildren( getTransaction(), VdbLexicon.Vdb.TRANSLATORS );

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
     * @see org.komodo.core.repository.KomodoObject#getTypeId()
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
    public String getVdbName() throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.STRING, "getVdbName", VdbLexicon.Vdb.NAME); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#getVersion(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public int getVersion( ) throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.INTEGER, "getVersion", VdbLexicon.Vdb.VERSION); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(java.lang.String)
     */
    @Override
    public boolean hasChild(
                             final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        return ( ( getImports( name ).length != 0 )
                 || ( getModels( name ).length != 0 )
                 || ( getTranslators( name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild(
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( VdbLexicon.ImportVdb.IMPORT_VDB.equals( typeName ) ) {
            return ( getImports( name ).length != 0 );
        }

        if ( VdbLexicon.Vdb.DECLARATIVE_MODEL.equals( typeName ) ) {
            return ( getModels( name ).length != 0 );
        }

        if ( VdbLexicon.Translator.TRANSLATOR.equals( typeName ) ) {
            return ( getTranslators( name ).length != 0 );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren()
     */
    @Override
    public boolean hasChildren( ) throws KException {
        if ( super.hasChildren( ) ) {
            return ( ( getImports(  ).length != 0 )
            || ( getModels(  ).length != 0 )
            || ( getTranslators(  ).length != 0 ) );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#isPreview(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isPreview( ) throws KException {
        return getObjectProperty(getTransaction(), PropertyValueType.BOOLEAN, "isPreview", VdbLexicon.Vdb.PREVIEW); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeImport(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeImport(
                              final String importToRemove ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( importToRemove, "importToRemove" ); //$NON-NLS-1$

        final VdbImportImpl[] vdbImports = getImports( importToRemove );

        if ( vdbImports.length == 0 ) {
            throw new KException( Messages.getString( Relational.VDB_IMPORT_NOT_FOUND_TO_REMOVE, importToRemove ) );
        }

        // remove first occurrence
        vdbImports[ 0 ].remove( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeModel(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeModel(
                             final String modelToRemove ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( modelToRemove, "modelToRemove" ); //$NON-NLS-1$

        final ModelImpl[] models = getModels( modelToRemove );

        if ( models.length == 0 ) {
            throw new KException( Messages.getString( Relational.MODEL_NOT_FOUND_TO_REMOVE, modelToRemove ) );
        }

        // remove first occurrence
        models[ 0 ].remove( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#removeTranslator(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void removeTranslator(
                                  final String translatorToRemove ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( translatorToRemove, "translatorToRemove" ); //$NON-NLS-1$

        final TranslatorImpl[] translators = getTranslators( translatorToRemove );

        if ( translators.length == 0 ) {
            throw new KException( Messages.getString( Relational.TRANSLATOR_NOT_FOUND_TO_REMOVE, translatorToRemove ) );
        }

        // remove first occurrence
        translators[ 0 ].remove( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void rename(UnitOfWork transaction,
                        final String newName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( newName, "newName" ); //$NON-NLS-1$

        super.rename( transaction, newName );
        String shortName = newName;
        if(newName.contains(StringConstants.FORWARD_SLASH)) {
            shortName = newName.substring(shortName.lastIndexOf(StringConstants.FORWARD_SLASH)+1);
        }
        setVdbName( shortName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setAllowedLanguages(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setAllowedLanguages(
                                     final String newAllowedLanguages ) throws KException {
        setObjectProperty( getTransaction(),
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
    public void setAuthenticationType(
                                       final String newAuthenticationType ) throws KException {
        setObjectProperty( getTransaction(),
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
    public void setConnectionType(
                                   final String newConnectionType ) throws KException {
        setObjectProperty(getTransaction(), "setConnectionType", VdbLexicon.Vdb.CONNECTION_TYPE, newConnectionType); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setDescription(java.lang.String)
     */
    @Override
    public void setDescription(
                                final String newDescription ) throws KException {
        setObjectProperty(getTransaction(), "setDescription", VdbLexicon.Vdb.DESCRIPTION, newDescription); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setGssPattern(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setGssPattern(
                               final String newGssPattern ) throws KException {
        setObjectProperty( getTransaction(), "setGssPattern", specialProperties[GSS_PATTERN].toTeiidName(), newGssPattern ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setOriginalFilePath(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setOriginalFilePath(
                                     final String newOriginalFilePath ) throws KException {
        ArgCheck.isNotEmpty(newOriginalFilePath, "newOriginalFilePath"); //$NON-NLS-1$
        setObjectProperty(getTransaction(), "setOriginalFilePath", VdbLexicon.Vdb.ORIGINAL_FILE, newOriginalFilePath); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setPasswordPattern(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setPasswordPattern(
                                    final String newPasswordPattern ) throws KException {
        setObjectProperty( getTransaction(), "setPasswordPattern", specialProperties[PASSWORD_PATTERN].toTeiidName(), newPasswordPattern ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setPreview(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setPreview(
                            final boolean newPreview ) throws KException {
        setObjectProperty(getTransaction(), "setPreview", VdbLexicon.Vdb.PREVIEW, newPreview); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setQueryTimeout(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setQueryTimeout(
                                 final int newQueryTimeout ) throws KException {
        // setting new value to null if timeout is less than zero to delete property
        Object newValue = null;

        if ( newQueryTimeout > -1 ) {
            newValue = newQueryTimeout;
        }

        setObjectProperty( getTransaction(), "setQueryTimeout", specialProperties[QUERY_TIMEOUT].toTeiidName(), newValue ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setVdbName(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setVdbName(
                            final String newVdbName ) throws KException {
        setObjectProperty(getTransaction(), "setVdbName", VdbLexicon.Vdb.NAME, newVdbName); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Vdb#setVersion(org.komodo.spi.repository.Repository.UnitOfWork, int)
     */
    @Override
    public void setVersion(
                            final int newVersion ) throws KException {
        setObjectProperty(getTransaction(), "setVersion", VdbLexicon.Vdb.VERSION, newVersion); //$NON-NLS-1$
    }

    @Override
    public ModelImpl getModel(String name) throws KException {
    	ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (!hasChild(name, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
        	return null;
        }
        
        final KomodoObject kobject = super.getChild( VdbLexicon.Vdb.DECLARATIVE_MODEL,
                                                                    name );
        return new ModelImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
    }
    
}
