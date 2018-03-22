/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.core.visitor;

import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import org.junit.Test;
import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.metadata.DefaultMetadataInstance;
import org.komodo.metadata.TeiidConnectionProvider;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.TestUtilities;
import org.mockito.Mockito;
import org.w3c.dom.Document;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestVdbExport extends AbstractLocalRepositoryTest {

    private class VdbErrorHandler implements ErrorHandler {

        List<Exception> exceptions = new ArrayList<>();

        @Override
        public void warning(SAXParseException exception) throws SAXException {
            exceptions.add(exception);
        }

        @Override
        public void error(SAXParseException exception) throws SAXException {
            String msg = exception.getMessage();

            if (msg.contains("Document is invalid: no grammar found."))
                return; // Ignore
            if (msg.contains("Document root element \"vdb\", must match DOCTYPE root \"null\"."))
                return; // Ignore

            exceptions.add(exception);
        }

        @Override
        public void fatalError(SAXParseException exception) throws SAXException {
            exceptions.add(exception);
        }

        public boolean noExceptions() {
            for (Exception ex : exceptions) {
                System.err.println(ex.getMessage());
            }
            return exceptions.isEmpty();
        }
        
    };

    private VdbNodeVisitor createNodeVisitor(Writer writer) throws Exception {
        XMLOutputFactory xof = XMLOutputFactory.newInstance();
        XMLStreamWriter xtw = null;
        xtw = xof.createXMLStreamWriter(writer);

        TeiidConnectionProvider provider = Mockito.mock(TeiidConnectionProvider.class);
        DefaultMetadataInstance instance = new DefaultMetadataInstance(provider);
        return new VdbNodeVisitor(instance.getVersion(),instance.getDataTypeService(), xtw);
    }

    @Test(timeout=3000000)
    public void testBasicVdbExport() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject twitterExample = TestUtilities.createTweetExampleNode(getTransaction(), workspace);

        commit();

//        traverse(getTransaction(), twitterExample);

        //
        // Sequencing completed, now verify
        //
        KomodoObject tweet = verify(getTransaction(), twitterExample,"twitterview/Tweet", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
        verify(getTransaction(), tweet, TeiidSqlLexicon.Query.ID, TeiidSqlLexicon.Query.ID);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(getTransaction(), twitterExample);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
//        System.out.println(testXML);
        VdbErrorHandler errorHandler = new VdbErrorHandler();
        Document testDoc = TestUtilities.createDocument(testXML, errorHandler);
        assertTrue(errorHandler.noExceptions());

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = TestUtilities.tweetExample();
        errorHandler = new VdbErrorHandler();
        Document compareDoc = TestUtilities.createDocument(compareStream, errorHandler);
        assertTrue(errorHandler.noExceptions());

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        TestUtilities.compareDocuments(compareDoc, testDoc);
    }

    @Test(timeout=3000000)
    public void testBasicVdbExportUndefinedAttribute() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject twitterExample = TestUtilities.createTweetExampleNoTransDescripNode(getTransaction(), workspace);
        commit();

//        traverse(getTransaction(), twitterExample);

        //
        // Sequencing completed, now verify
        //
        KomodoObject tweet = verify(getTransaction(), twitterExample,"twitterview/Tweet", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
        verify(getTransaction(), tweet, TeiidSqlLexicon.Query.ID, TeiidSqlLexicon.Query.ID);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(getTransaction(), twitterExample);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
//        System.out.println(testXML);
        VdbErrorHandler errorHandler = new VdbErrorHandler();
        Document testDoc = TestUtilities.createDocument(testXML, errorHandler);
        assertTrue(errorHandler.noExceptions());

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = TestUtilities.undefinedAttrExample();
        errorHandler = new VdbErrorHandler();
        Document compareDoc = TestUtilities.createDocument(compareStream, errorHandler);
        assertTrue(errorHandler.noExceptions());

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        TestUtilities.compareDocuments(compareDoc, testDoc);
    }

    @Test(timeout=3000000)
    public void testAllElementsVdbExport() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject allElements = TestUtilities.createAllElementsExampleNode(getTransaction(), workspace);
        commit();

//        traverse(getTransaction(), allElements);

        //
        // Sequencing completed, now verify
        //
        KomodoObject testNode = verify(getTransaction(), allElements, "model-two/Test", TeiidDdlLexicon.CreateTable.VIEW_STATEMENT);
        verify(getTransaction(), testNode, TeiidSqlLexicon.Query.ID, TeiidSqlLexicon.Query.ID);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(getTransaction(), allElements);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
//        System.out.println(testXML);
        VdbErrorHandler errorHandler = new VdbErrorHandler();
        Document testDoc = TestUtilities.createDocument(testXML, errorHandler);
        assertTrue(errorHandler.noExceptions());

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = TestUtilities.allElementsExample();
        errorHandler = new VdbErrorHandler();
        Document compareDoc = TestUtilities.createDocument(compareStream, errorHandler);
        assertTrue(errorHandler.noExceptions());

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        TestUtilities.compareDocuments(compareDoc, testDoc);
    }
}