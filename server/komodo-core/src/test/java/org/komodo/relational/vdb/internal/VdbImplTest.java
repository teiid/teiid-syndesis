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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.PropertyDescriptor;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.Vdb.VdbManifest;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManagerImpl;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbImplTest extends RelationalModelTest {

    private static final String PATH = "/Users/sledge/hammer/MyVdb.vdb";
    private static final String VDB_NAME = "vdb";

    protected VdbImpl vdb;

    @Before
    public void init() throws Exception {
        this.vdb = createVdb( VDB_NAME, PATH );
    }

    @Test
    public void shouldAddImport() throws Exception {
        final String name = "vdbImport";
        final VdbImport vdbImport = this.vdb.addImport( name );
        assertThat( vdbImport, is( notNullValue() ) );
        assertThat( this.vdb.getImports( ).length, is( 1 ) );

        final VdbImportImpl added = this.vdb.getImports( )[0];
        assertThat( added, is( vdbImport ) );
        assertThat( added.getName( ), is( name ) );
        assertThat( added.getPrimaryType( ).getName(), is( VdbLexicon.ImportVdb.IMPORT_VDB ) );
        assertThat( this.vdb.getChildren( )[0], is( instanceOf( VdbImport.class ) ) );

        assertThat( this.vdb.hasChild( name ), is( true ) );
        assertThat( this.vdb.hasChild( name, VdbLexicon.ImportVdb.IMPORT_VDB ), is( true ) );
        assertThat( this.vdb.hasChildren( ), is( true ) );
        assertThat( this.vdb.getChild( name ), is( added ) );
        assertThat( this.vdb.getChild( name, VdbLexicon.ImportVdb.IMPORT_VDB ), is( added ) );
    }

    @Test
    public void shouldAddModel() throws Exception {
        final String name = "model";
        final ModelImpl model = this.vdb.addModel( name );
        assertThat( model, is( notNullValue() ) );
        assertThat( this.vdb.getModels( ).length, is( 1 ) );

        final ModelImpl added = this.vdb.getModels( )[0];
        assertThat( added, is( model ) );
        assertThat( added.getName( ), is( name ) );
        assertThat( added.getPrimaryType( ).getName(), is( VdbLexicon.Vdb.DECLARATIVE_MODEL ) );
        assertThat( this.vdb.getChildren( )[0], is( instanceOf( ModelImpl.class ) ) );

        assertThat( this.vdb.hasChild( name ), is( true ) );
        assertThat( this.vdb.hasChild( name, VdbLexicon.Vdb.DECLARATIVE_MODEL ), is( true ) );
        assertThat( this.vdb.hasChildren( ), is( true ) );
        assertThat( this.vdb.getChild( name ), is( added ) );
        assertThat( this.vdb.getChild( name, VdbLexicon.Vdb.DECLARATIVE_MODEL ), is( added ) );
    }

    @Test
    public void shouldAddTranslator() throws Exception {
        final String name = "translator";
        final String type = "oracle";
        final Translator translator = this.vdb.addTranslator( name, type );
        assertThat( translator, is( notNullValue() ) );
        assertThat( this.vdb.getTranslators( ).length, is( 1 ) );

        final TranslatorImpl added = this.vdb.getTranslators( )[0];
        assertThat( added, is( translator ) );
        assertThat( added.getName( ), is( name ) );
        assertThat( added.getPrimaryType( ).getName(), is( VdbLexicon.Translator.TRANSLATOR ) );
        assertThat( added.getType( ), is( type ) );
        assertThat( added, is( instanceOf( Translator.class ) ) );

        assertThat( this.vdb.hasChild( name ), is( true ) );
        assertThat( this.vdb.hasChild( name, VdbLexicon.Translator.TRANSLATOR ), is( true ) );
        assertThat( this.vdb.hasChildren( ), is( true ) );
        assertThat( this.vdb.getChild( name ), is( added ) );
        assertThat( this.vdb.getChild( name, VdbLexicon.Translator.TRANSLATOR ), is( added ) );
    }

    @Test
    public void shouldCreateManifestForEmptyVdb() throws Exception {
        final VdbManifest manifest = this.vdb.createManifest( new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.asDocument(), is( notNullValue() ) );
    }

    @Test
    public void shouldCreateManifestForVdb() throws Exception {
        { // setup
            this.vdb.setVdbName( "twitter" );
            this.vdb.setVersion( 1 );
            this.vdb.setDescription( "Shows how to call Web Services" );
            this.vdb.setProperty( "UseConnectorMetadata", "cached" );

            final ModelImpl twitter = this.vdb.addModel( "twitter" );
            twitter.setModelType( Model.Type.PHYSICAL );

            final ModelImpl twitterview = this.vdb.addModel( "twitterview" );
            twitterview.setModelType( Model.Type.VIRTUAL );

            final TranslatorImpl translator = this.vdb.addTranslator( "rest", "ws" );
            translator.setProperty( "DefaultBinding", "HTTP" );
        }

        final VdbManifest manifest = this.vdb.createManifest( new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.asDocument(), is( notNullValue() ) );
    }

    @Test
    public void shouldExportEmptyVdb() throws Exception {
        byte[] manifest = this.vdb.export( new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertTrue( manifest.length > 0 );
    }

    @Test
    public void shouldExportVdb() throws Exception {
        { // setup
            this.vdb.setVdbName( "twitter" );
            this.vdb.setVersion( 1 );
            this.vdb.setDescription( "Shows how to call Web Services" );
            this.vdb.setProperty( "UseConnectorMetadata", "cached" );

            final ModelImpl twitter = this.vdb.addModel( "twitter" );
            twitter.setModelType( Model.Type.PHYSICAL );

            final ModelImpl twitterview = this.vdb.addModel( "twitterview" );
            twitterview.setModelType( Model.Type.VIRTUAL );

            final TranslatorImpl translator = this.vdb.addTranslator( "rest", "ws" );
            translator.setProperty( "DefaultBinding", "HTTP" );
        }

        // test
        byte[] manifest = this.vdb.export( new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertTrue( manifest.length > 0 );
    }

    @Test
    public void shouldFailConstructionIfNotVdb() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new VdbImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailGetChildWhenTypeIsWrong() throws Exception {
        final String name = "blah";
        this.vdb.getChild( name, "bogusType" );
    }

    @Test( expected = KException.class )
    public void shouldFailWhenChildNotFound() throws Exception {
        this.vdb.getChild( "bogus" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailRenameWhenNewNameIsEmpty() throws Exception {
        this.vdb.rename( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailRenameWhenNewNameIsNull() throws Exception {
        this.vdb.rename( getTransaction(), null );
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        assertThat( Arrays.asList( this.vdb.getChildTypes() ),
                    hasItems( Model.IDENTIFIER,
                              Translator.IDENTIFIER,
                              VdbImport.IDENTIFIER ) );
        assertThat(this.vdb.getChildTypes().length, is(3));
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.vdb.getName( ), is( VDB_NAME ) );
    }

    @Test
    public void shouldHaveCorrectOriginalFilePathAfterConstruction() throws Exception {
        assertThat( this.vdb.getOriginalFilePath( ), is( PATH ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.vdb.getPrimaryType( ).getName(), is( VdbLexicon.Vdb.VIRTUAL_DATABASE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.vdb.getTypeIdentifier( ), is(KomodoType.VDB));
    }

    @Test
    public void shouldHaveDefaultPreviewValueAfterConstruction() throws Exception {
        assertThat( this.vdb.isPreview( ), is( Vdb.DEFAULT_PREVIEW ) );
    }

    @Test
    public void shouldHaveDefaultVersionAfterConstruction() throws Exception {
        assertThat( this.vdb.getVersion( ), is( Vdb.DEFAULT_VERSION ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.vdb.getPropertyNames( );
        final String[] rawProps = this.vdb.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveStrongTypedChildren() throws Exception {
        this.vdb.addImport( "vdbImport" );
        this.vdb.addModel( "model" );
        assertThat( this.vdb.getChildren( ).length, is( 2 ) );
        assertThat( this.vdb.getChildren( )[0], is( instanceOf( VdbImport.class ) ) );
        assertThat( this.vdb.getChildren( )[1], is( instanceOf( ModelImpl.class ) ) );
    }

    @Test
    public void shouldIncludeSpecialPropertiesInPrimaryTypePropertyDescriptors() throws Exception {
        final PropertyDescriptor[] descriptors = this.vdb.getPrimaryType( ).getPropertyDescriptors( );
        final List< String > specialProps = new ArrayList<>();
        specialProps.add(Vdb.SECURITY_DOMAIN_TEIIDNAME);
        specialProps.add(Vdb.QUERY_TIMEOUT_TEIIDNAME);
        specialProps.add(Vdb.PASSWORD_PATTERN_TEIIDNAME);
        specialProps.add(Vdb.GSS_PATTERN_TEIIDNAME);
        specialProps.add(Vdb.AUTHENTICATION_TYPE_TEIIDNAME);
        specialProps.add(Vdb.ALLOWED_LANGUAGES_TEIIDNAME);

        // make sure we are returning more than just the special props
        assertThat( descriptors.length > specialProps.size(), is( true ) );

        for ( final PropertyDescriptor descriptor : descriptors ) {
            if ( specialProps.contains( descriptor.getName() ) ) {
                specialProps.remove( descriptor.getName() );
            }
        }

        // make sure we found all the special props
        assertThat( specialProps.isEmpty(), is( true ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyImport() throws Exception {
        this.vdb.addImport( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyModel() throws Exception {
        this.vdb.addModel( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyTranslator() throws Exception {
        this.vdb.addTranslator( StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullImport() throws Exception {
        this.vdb.addImport( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullModel() throws Exception {
        this.vdb.addModel( null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullTranslator() throws Exception {
        this.vdb.addTranslator( null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath( null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.vdb.getPropertyNames( );
        final Filter[] filters = ((VdbImpl)this.vdb).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveConnectionTypeAfterConstruction() throws Exception {
        assertThat( this.vdb.getConnectionType( ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveModelsAfterConstruction() throws Exception {
        assertThat( this.vdb.getModels( ), is( notNullValue() ) );
        assertThat( this.vdb.getModels( ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveTranslatorsAfterConstruction() throws Exception {
        assertThat( this.vdb.getTranslators( ), is( notNullValue() ) );
        assertThat( this.vdb.getTranslators( ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveVdbImportsAfterConstruction() throws Exception {
        assertThat( this.vdb.getImports( ), is( notNullValue() ) );
        assertThat( this.vdb.getImports( ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveAllowedLanguages() throws Exception {
        final String newValue = "newAllowedLanguages";
        this.vdb.setAllowedLanguages( newValue );
        this.vdb.setAllowedLanguages( null );
        assertThat( this.vdb.getAllowedLanguages( ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveAuthenticationType() throws Exception {
        final String newValue = "newAuthenticationType";
        this.vdb.setAuthenticationType( newValue );
        this.vdb.setAuthenticationType( null );
        assertThat( this.vdb.getAuthenticationType( ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveGssPattern() throws Exception {
        final String newValue = "newGssPattern";
        this.vdb.setGssPattern( newValue );
        this.vdb.setGssPattern( null );
        assertThat( this.vdb.getGssPattern( ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveModel() throws Exception {
        final String name = "model";
        this.vdb.addModel( name );
        assertThat( this.vdb.getModels( ).length, is( 1 ) );

        this.vdb.removeModel( name );
        assertThat( this.vdb.getModels( ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePasswordPattern() throws Exception {
        final String newValue = "newPasswordPattern";
        this.vdb.setPasswordPattern( newValue );
        this.vdb.setPasswordPattern( null );
        assertThat( this.vdb.getPasswordPattern( ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveQueryTimeout() throws Exception {
        final int newValue = 10;
        this.vdb.setQueryTimeout( newValue );
        this.vdb.setQueryTimeout( -100 );
        assertThat( this.vdb.getQueryTimeout( ), is( -1 ) );
    }

    @Test
    public void shouldRemoveTranslator() throws Exception {
        final String name = "translator";
        this.vdb.addTranslator( name, "oracle" );
        assertThat( this.vdb.getTranslators( ).length, is( 1 ) );

        this.vdb.removeTranslator( name );
        assertThat( this.vdb.getTranslators( ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveVdbImport() throws Exception {
        final String name = "vdbImport";
        this.vdb.addImport( name );
        assertThat( this.vdb.getImports( ).length, is( 1 ) );

        this.vdb.removeImport( name );
        assertThat( this.vdb.getImports( ).length, is( 0 ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "newVdbName";
        this.vdb.rename( getTransaction(), newName );
        assertThat( this.vdb.getName( ), is( newName ) );
        assertThat( this.vdb.getVdbName( ), is( newName ) );
    }

    @Test
    public void shouldRoundTripVdb() throws Exception {
        final InputStream vdbStream = getClass().getClassLoader().getResourceAsStream("AzureService-vdb.xml");
        assertThat( vdbStream, is( notNullValue() ) );

        final String name = "AzureService";
        final VdbImporter importer = new VdbImporter( _repo );
        final ImportOptions importOptions = new ImportOptions();
        importOptions.setOption( OptionKeys.NAME, name );
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        importer.importVdb( getTransaction(), vdbStream, workspace, importOptions, new ImportMessages() );

        commit(); // commit the import

        final VdbImpl[] vdbs = WorkspaceManagerImpl.getInstance( _repo, getTransaction() ).findVdbs( null );
        assertThat( vdbs.length, is( 2 ) );

        // find the imported VDB
        VdbImpl importedVdb = null;

        if ( name.equals( vdbs[ 0 ].getName( ) ) ) {
            importedVdb = vdbs[ 0 ];
        } else if ( name.equals( vdbs[ 1 ].getName( ) ) ) {
            importedVdb = vdbs[ 1 ];
        } else {
            fail();
        }

        final Vdb.VdbManifest manifest = importedVdb.createManifest( null );
        final Document doc = manifest.asDocument();
        final NodeList kids = doc.getChildNodes();
        assertThat( kids.getLength(), is( 1 ) );

        final Node vdbNode = kids.item( 0 );
        assertThat( vdbNode.getAttributes().getNamedItem( VdbLexicon.ManifestIds.NAME ).getNodeValue(), is( "AzureService" ) );
        assertThat( vdbNode.getAttributes().getNamedItem( VdbLexicon.ManifestIds.VERSION ).getNodeValue(), is( "1" ) );

        if ( vdbNode.getNodeType() != Node.ELEMENT_NODE ) {
            fail( "vdbNode is not an XML element" );
        }

        final Element vdbElement = ( Element )vdbNode;

        { // description
            final NodeList descriptionNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.DESCRIPTION );
            assertThat( descriptionNodes.getLength(), is( 1 ) );
            assertThat( descriptionNodes.item( 0 ).getTextContent(), is( "VDB for: AzureService, Version: 1" ) );
        }

        { // connection type
            final NodeList connectionTypeNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.CONNECTION_TYPE );
            assertThat( connectionTypeNodes.getLength(), is( 1 ) );
            assertThat( connectionTypeNodes.item( 0 ).getTextContent(), is( "BY_VERSION" ) );
        }

        { // properties
            final NodeList propertyNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.PROPERTY );
            assertThat( propertyNodes.getLength(), is( 2 ) );

            final Node node1 = propertyNodes.item( 0 );
            final Node node2 = propertyNodes.item( 1 );
            boolean node1Taken = false;
            boolean node2Taken = false;

            { // auto-generate property
                final String autoGenerateProp = "{http://teiid.org/rest}auto-generate";

                if ( autoGenerateProp.equals( node1.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node1.getAttributes().getNamedItem( "value" ).getTextContent(), is( "true" ) );
                    node1Taken = true;
                } else if ( autoGenerateProp.equals( node2.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node2.getAttributes().getNamedItem( "value" ).getTextContent(), is( "true" ) );
                    node2Taken = true;
                } else {
                    fail( "auto-generate property failure" );
                }
            }

            { // data-services-view property
                final String dataServiceViewProp = "data-service-view";

                if ( !node1Taken
                     && dataServiceViewProp.equals( node1.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node1.getAttributes().getNamedItem( "value" ).getTextContent(), is( "SvcView" ) );
                } else if ( !node2Taken
                            && dataServiceViewProp.equals( node2.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node2.getAttributes().getNamedItem( "value" ).getTextContent(), is( "SvcView" ) );
                } else {
                    fail( "data-service-view property failure" );
                }
            }
        }

        { // import VDB
            final NodeList importVdbNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.IMPORT_VDB );
            assertThat( importVdbNodes.getLength(), is( 1 ) );

            final Node node = importVdbNodes.item( 0 );
            final NamedNodeMap attributes = node.getAttributes();

            { // name
                final String attr = VdbLexicon.ManifestIds.NAME;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "SvcSourceVdb_AzurePricesDS" ) );
            }

            { // version
                final String attr = VdbLexicon.ManifestIds.VERSION;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "1" ) );
            }

            { // import-data-policies
                final String attr = VdbLexicon.ManifestIds.IMPORT_DATA_POLICIES;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "true" ) );
            }
        }

        { // model
            final NodeList modelNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.MODEL );
            assertThat( modelNodes.getLength(), is( 1 ) );

            final Node modelNode = modelNodes.item( 0 );
            final NamedNodeMap attributes = modelNode.getAttributes();

            if ( modelNode.getNodeType() != Node.ELEMENT_NODE ) {
                fail( "modelNode is not an XML element" );
            }

            final Element modelElement = ( Element )modelNode;

            { // name
                final String attr = VdbLexicon.ManifestIds.NAME;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "AzureService" ) );
            }

            { // type
                final String attr = VdbLexicon.ManifestIds.TYPE;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "VIRTUAL" ) );
            }
//
//            { // visible
//                final String attr = VdbLexicon.ManifestIds.VISIBLE;
//                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
//                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "true" ) );
//            }

            { // metadata
                final NodeList metaDataNodes = modelElement.getElementsByTagName( VdbLexicon.ManifestIds.METADATA );
                assertThat( metaDataNodes.getLength(), is( 1 ) );

                final Node metaDataNode = metaDataNodes.item( 0 );

                { // type
                    final String attr = VdbLexicon.ManifestIds.TYPE;
                    assertThat( metaDataNode.getAttributes().getNamedItem( attr ), is( notNullValue() ) );
                    assertThat( metaDataNode.getAttributes().getNamedItem( attr ).getTextContent(), is( "DDL" ) );
                }

                { // metadata
                    final String ddl = metaDataNode.getTextContent();
                    final String expected = "CREATE VIEW SvcView ( RowId integer, ProdCode string, SalePrice bigdecimal, PRIMARY KEY(RowId) ) AS SELECT ROW_NUMBER() OVER (ORDER BY ProdCode), ProdCode,SalePrice FROM \"Prices.dbo.PricesTable\"; SET NAMESPACE 'http://teiid.org/rest' AS REST; CREATE VIRTUAL PROCEDURE RestProc() RETURNS TABLE (result xml) OPTIONS (\"REST:URI\" 'rest', \"REST:METHOD\" 'GET') AS BEGIN SELECT XMLELEMENT(NAME Elems, XMLAGG(XMLELEMENT(NAME Elem, XMLFOREST(RowId,ProdCode,SalePrice)))) AS result FROM SvcView; END;;";
                    assertEquals(expected, ddl);

                    // since the actual export will have the CDATA marker make sure by actually doing an export here
                    byte[] exportBytes = importedVdb.export( null );
                    String export = new String(exportBytes);
                    assertThat( export.contains( "<![CDATA[" ), is( true ) );
                    assertThat( export.contains( "]]>" ), is( true ) );
                }
            }
        }
    }

    @Test
    public void shouldSetAllowedLanguages() throws Exception {
        final String newValue = "newAllowedLanguages";
        this.vdb.setAllowedLanguages( newValue );
        assertThat( this.vdb.getAllowedLanguages( ), is( newValue ) );
    }

    @Test
    public void shouldSetAuthenticationType() throws Exception {
        final String newValue = "newAuthenticationType";
        this.vdb.setAuthenticationType( newValue );
        assertThat( this.vdb.getAuthenticationType( ), is( newValue ) );
    }

    @Test
    public void shouldSetConnectionType() throws Exception {
        final String newValue = "newConnectionType";
        this.vdb.setConnectionType( newValue );
        assertThat( this.vdb.getConnectionType( ), is( newValue ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.vdb.setDescription( newValue );
        assertThat( this.vdb.getDescription( ), is( newValue ) );
    }

    @Test
    public void shouldSetGssPattern() throws Exception {
        final String newValue = "newGssPattern";
        this.vdb.setGssPattern( newValue );
        assertThat( this.vdb.getGssPattern( ), is( newValue ) );
    }

    @Test
    public void shouldSetOriginalFilePath() throws Exception {
        final String newValue = "newOriginalFilePath";
        this.vdb.setOriginalFilePath( newValue );
        assertThat( this.vdb.getOriginalFilePath( ), is( newValue ) );
    }

    @Test
    public void shouldSetPasswordPattern() throws Exception {
        final String newValue = "newPasswordPattern";
        this.vdb.setPasswordPattern( newValue );
        assertThat( this.vdb.getPasswordPattern( ), is( newValue ) );
    }

    @Test
    public void shouldSetPreviewValue() throws Exception {
        final boolean newValue = !Vdb.DEFAULT_PREVIEW;
        this.vdb.setPreview( newValue );
        assertThat( this.vdb.isPreview( ), is( newValue ) );
    }

    @Test
    public void shouldSetPreviewValueWithStringValue() throws Exception {
        this.vdb.setProperty( VdbLexicon.Vdb.PREVIEW, "blah" );
        assertThat( this.vdb.isPreview( ), is( false ) );
    }

    @Test
    public void shouldSetQueryTimeout() throws Exception {
        final int newValue = 10;
        this.vdb.setQueryTimeout( newValue );
        assertThat( this.vdb.getQueryTimeout( ), is( newValue ) );
    }

    @Test
    public void shouldSetVdbName() throws Exception {
        final String newValue = "newName";
        this.vdb.setVdbName( newValue );
        assertThat( this.vdb.getVdbName( ), is( newValue ) );
    }

    @Test
    public void shouldSetVersion() throws Exception {
        final int newValue = ( Vdb.DEFAULT_VERSION + 10 );
        this.vdb.setVersion( newValue );
        assertThat( this.vdb.getVersion( ), is( newValue ) );
    }

}
