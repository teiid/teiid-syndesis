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
package org.komodo.relational.dataservice;

import static org.komodo.spi.constants.StringConstants.COLON;

import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.komodo.core.KomodoLexicon;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.LexiconConstants.JcrLexicon;
import org.komodo.spi.lexicon.LexiconConstants.NTLexicon;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.w3c.dom.Document;

/**
 * Represents a data service XML manifest file.
 */
public class DataserviceManifest implements Exportable {

    private static final String[] HIDDEN_PROP_PREFIXES = new String[] { JcrLexicon.Namespace.PREFIX
                                                                        + COLON,
                                                                        NTLexicon.Namespace.PREFIX + COLON,
                                                                        DataVirtLexicon.Namespace.PREFIX + COLON,
                                                                        KomodoLexicon.Namespace.PREFIX + COLON };

    private static final SimpleDateFormat FORMATTER = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" ); //$NON-NLS-1$

    /**
     * The data service manifest archive entry path. Value is {@value}.
     */
    public static final String MANIFEST = "META-INF/dataservice.xml"; //$NON-NLS-1$

    protected static final KLog LOGGER = KLog.getLogger();

    private final Dataservice dataService;

    private String xml;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param dataService
     *        the data service whose manifest is being constructed (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public DataserviceManifest( final UnitOfWork transaction,
                                final Dataservice dataService ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        this.dataService = Objects.requireNonNull( dataService, "dataService" ); //$NON-NLS-1$
    }

    private void writeNewLine( XMLStreamWriter writer ) throws XMLStreamException {
        writer.writeCharacters( StringConstants.NEW_LINE );
    }

    private void writeTab( XMLStreamWriter writer ) throws XMLStreamException {
        writer.writeCharacters( StringConstants.TAB );
    }

    private void writeAttribute( final XMLStreamWriter writer,
                                 final String name,
                                 final String value ) throws XMLStreamException {
        writer.writeAttribute( name, value );
    }

    private void writeCharacters( final XMLStreamWriter writer,
                                  final String characters ) throws XMLStreamException {
        writer.writeCharacters( characters );
    }

    private void writeEndElement( final XMLStreamWriter writer ) throws XMLStreamException {
        writer.writeEndElement();
    }

    private void writePropertyElement( final XMLStreamWriter writer,
                                       final String propName,
                                       final String propValue ) throws XMLStreamException {
        writeNewLine( writer );
        writeTab( writer );
        writeStartElement( writer, DataVirtLexicon.DataServiceManifestId.PROPERTY );
        writeAttribute( writer, DataVirtLexicon.DataServiceManifestId.NAME, propName );
        writeCharacters( writer, propValue );
        writeEndElement( writer );
    }

    private void writeStartElement( final XMLStreamWriter writer,
                                    final String tag ) throws XMLStreamException {
        writer.writeStartElement( tag );
    }

    @Override
    public String getName( UnitOfWork transaction ) throws KException {
        return dataService.getName( transaction );
    }

    @Override
    public DocumentType getDocumentType( UnitOfWork transaction ) {
        return DocumentType.VDB_XML;
    }
//
//    private static XMLStreamReader createReader( InputStream srcStream ) throws Exception {
//        XMLInputFactory factory = XMLInputFactory.newInstance();
//        factory.setProperty( XMLInputFactory.IS_NAMESPACE_AWARE, false );
//        return factory.createXMLStreamReader( srcStream );
//    }
//
//    /**
//     * @param srcStream
//     *        an input stream of the data service manifest (cannot be <code>null</code>)
//     * @return Finds the data service name and returns it from the xml
//     * @throws KException
//     *         if an error occurs
//     */
//    public static String extractName( InputStream srcStream ) throws KException {
//        XMLStreamReader reader = null;
//        String name = null;
//
//        try {
//            reader = createReader( Objects.requireNonNull( srcStream, "srcStream" ) ); //$NON-NLS-1$
//
//            FIND_NAME: while ( reader.hasNext() ) {
//                switch ( reader.getEventType() ) {
//                    case XMLStreamConstants.START_ELEMENT:
//                        if ( DataVirtLexicon.DataServiceManifestId.DATASERVICE.equals( reader.getLocalName() ) ) {
//                            name = reader.getAttributeValue( DataVirtLexicon.DataServiceManifestId.NAME, reader.getLocalName() );
//                            break FIND_NAME;
//                        }
//
//                        break;
//                    default:
//                        break;
//                }
//
//                reader.next();
//            }
//        } catch ( Exception ex ) {
//            throw new KException( ex );
//        } finally {
//            if ( reader != null ) {
//                try {
//                    reader.close();
//                } catch ( Exception ex ) {
//                    // Do Nothing
//                }
//            }
//        }
//
//        return name;
//    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export( final UnitOfWork transaction,
                          final Properties properties ) throws KException {
        StringWriter writer = new StringWriter();
        XMLStreamWriter xsw = null;

        try {
            XMLOutputFactory xof = XMLOutputFactory.newInstance();
            xsw = xof.createXMLStreamWriter( writer );
            xsw.writeStartDocument( "UTF-8", "1.0" ); //$NON-NLS-1$ //$NON-NLS-2$

            // Root DataService element
            writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.DATASERVICE );
            writeAttribute( xsw, DataVirtLexicon.DataServiceManifestId.NAME, this.dataService.getName( transaction ) );
            writeNewLine( xsw );

            // data service description element
            if ( !StringUtils.isBlank( this.dataService.getDescription( transaction ) ) ) {
                writeNewLine( xsw );
                writeTab( xsw );
                writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.DESCRIPTION );
                writeCharacters( xsw, this.dataService.getDescription( transaction ) );
                writeEndElement( xsw );
            }

            // data service lastModified element
            if ( this.dataService.getLastModified( transaction ) != null ) {
                writeNewLine( xsw );
                writeTab( xsw );
                writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.LAST_MODIFIED );
                writeCharacters( xsw, FORMATTER.format( this.dataService.getLastModified( transaction ).getTime() ) );
                writeEndElement( xsw );
            }

            // data service modifiedBy element
            if ( !StringUtils.isBlank( this.dataService.getModifiedBy( transaction ) ) ) {
                writeNewLine( xsw );
                writeTab( xsw );
                writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.MODIFIED_BY );
                writeCharacters( xsw, this.dataService.getModifiedBy( transaction ) );
                writeEndElement( xsw );
            }

            writeNewLine( xsw );

            { // properties
                final String[] names = filter( this.dataService.getPropertyNames( transaction ) );

                if ( names.length != 0 ) {
                    for ( final String name : names ) {
                        final Property property = this.dataService.getProperty( transaction, name );
                        final Object value = property.getValue( transaction );

                        if ( ( value == null ) || ( value.toString().length() == 0 ) ) {
                            continue;
                        }

                        writePropertyElement( xsw, name, value.toString() );
                    }

                    writeNewLine( xsw );
                }
            }

            { // service VDB entry
                final ServiceVdbEntry entry = this.dataService.getServiceVdbEntry( transaction );

                if ( entry != null ) {
                    writeNewLine( xsw );
                    writeTab( xsw );
                    writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.SERVICE_VDB );
                    writeAttribute( xsw, DataVirtLexicon.DataServiceManifestId.PATH, entry.getEntryPath( transaction ) );
                    writeAttribute( xsw,
                                    DataVirtLexicon.DataServiceManifestId.PUBLISH,
                                    entry.getPublishPolicy( transaction ).toXml() );
                    writeAttribute( xsw, DataVirtLexicon.DataServiceManifestId.VDB_NAME, entry.getVdbName( transaction ) );
                    writeAttribute( xsw, DataVirtLexicon.DataServiceManifestId.VDB_VERSION, entry.getVdbVersion( transaction ) );

                    { // dependencies
                        final VdbEntry[] dependencies = entry.getDependencies( transaction );

                        if ( dependencies.length != 0 ) {
                            writeNewLine( xsw );
                            writeTab( xsw );
                            writeTab( xsw );
                            writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.DEPENDENCIES );

                            for ( final VdbEntry dependency : dependencies ) {
                                writeNewLine( xsw );
                                writeTab( xsw );
                                writeTab( xsw );
                                writeTab( xsw );
                                writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.VDB_FILE );
                                writeAttribute( xsw,
                                                DataVirtLexicon.DataServiceManifestId.PATH,
                                                dependency.getEntryPath( transaction ) );
                                writeAttribute( xsw,
                                                DataVirtLexicon.DataServiceManifestId.PUBLISH,
                                                dependency.getPublishPolicy( transaction ).toXml() );
                                writeAttribute( xsw,
                                                DataVirtLexicon.DataServiceManifestId.VDB_NAME,
                                                dependency.getVdbName( transaction ) );
                                writeAttribute( xsw,
                                                DataVirtLexicon.DataServiceManifestId.VDB_VERSION,
                                                dependency.getVdbVersion( transaction ) );
                                writeEndElement( xsw );
                            }

                            writeNewLine( xsw );
                            writeTab( xsw );
                            writeTab( xsw );
                            writeEndElement( xsw );
                        }
                    }

                    writeNewLine( xsw );
                    writeTab( xsw );
                    writeEndElement( xsw );
                }
            }

            { // vdb entries
                final VdbEntry[] entries = this.dataService.getVdbEntries( transaction );

                if ( entries.length != 0 ) {
                    writeNewLine( xsw );
                    writeTab( xsw );
                    writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.VDBS );

                    for ( final VdbEntry entry : entries ) {
                        writeNewLine( xsw );
                        writeTab( xsw );
                        writeTab( xsw );
                        writeStartElement( xsw, DataVirtLexicon.DataServiceManifestId.VDB_FILE );
                        writeAttribute( xsw, DataVirtLexicon.DataServiceManifestId.PATH, entry.getEntryPath( transaction ) );
                        writeAttribute( xsw,
                                        DataVirtLexicon.DataServiceManifestId.PUBLISH,
                                        entry.getPublishPolicy( transaction ).toXml() );
                        writeAttribute( xsw, DataVirtLexicon.DataServiceManifestId.VDB_NAME, entry.getVdbName( transaction ) );
                        writeAttribute( xsw,
                                        DataVirtLexicon.DataServiceManifestId.VDB_VERSION,
                                        entry.getVdbVersion( transaction ) );
                        writeEndElement( xsw );
                    }

                    writeNewLine( xsw );
                    writeTab( xsw );
                    writeEndElement( xsw );
                }
            }

            writeNewLine( xsw );
            writeNewLine( xsw );
            writeEndElement( xsw );
            xsw.writeEndDocument();
        } catch ( final Exception e ) {
            throw new KException( e );
        } finally {
            if ( xsw != null ) {
                try {
                    xsw.close();
                } catch ( Exception ex ) {
                    // Do Nothing
                }
            }
        }

        // Create an XML Document from the filled writer
        this.xml = writer.toString().trim();

        if ( LOGGER.isDebugEnabled() ) {
            LOGGER.debug( "VdbImpl#VdbManifestImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                          transaction.getName(),
                          this.xml );
        }
        return this.xml == null ? new byte[ 0 ] : this.xml.getBytes();
    }

    private String[] filter( final String[] propertyNames ) {
        final List< String > result = new ArrayList<>( propertyNames.length );

        FILTER: for ( final String name : propertyNames ) {
            for ( final String prefix : HIDDEN_PROP_PREFIXES ) {
                if ( name.startsWith( prefix ) ) {
                    continue FILTER;
                }
            }

            result.add( name );
        }

        return result.toArray( new String[ result.size() ] );
    }

    /**
     * @return the manifest as an XML document (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Document asDocument() throws KException {
        return FileUtils.createDocument( this.xml );
    }
}
