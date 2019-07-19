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
package org.komodo.core.visitor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.junit.Test;
import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.Property;
import org.komodo.metadata.DataTypeService;
import org.komodo.test.utils.TestUtilities;
import org.teiid.modeshape.sequencer.vdb.lexicon.CoreLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
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

        return new VdbNodeVisitor(new DataTypeService(), xtw);
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

    @Test(timeout=3000000)
    public void testPatientsVdbExport() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject patientsExample = TestUtilities.createPatientsExampleNode(getTransaction(), workspace);

        commit();

        traverse(getTransaction(), patientsExample);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(getTransaction(), patientsExample);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
        System.out.println(testXML);
        VdbErrorHandler errorHandler = new VdbErrorHandler();
        Document testDoc = TestUtilities.createDocument(testXML, errorHandler);
        assertTrue(errorHandler.noExceptions());

        //
        // Create comparison XML Document from the example xml files
        //
        InputStream compareStream = TestUtilities.patientsExample();
        errorHandler = new VdbErrorHandler();
        Document compareDoc = TestUtilities.createDocument(compareStream, errorHandler);
        assertTrue(errorHandler.noExceptions());

        // Compare the XML documents. Unlike Document.isEqualNode(document)
        // the document nodes can be in a different order and the documents are
        // still equal.
        TestUtilities.compareDocuments(compareDoc, testDoc);
    }

    @Test(timeout=3000000)
    public void testPatientsVdbAddModelThenExport() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject patientsExample = TestUtilities.createPatientsExampleNode(getTransaction(), workspace);

        commit();
//        traverse(getTransaction(), patientsExample);

        /*
         *      FurtherPatients
         *          @jcr:primaryType=vdb:declarativeModel
         *          @jcr:uuid={uuid-to-be-created}
         *          @mmcore:modelType=VIRTUAL
         *          @description=Further Patients View
         *          @vdb:metadataType=DDL
         *          @vdb:visible=true
         */
        String furtherPatientsModelName = "FurtherPatients";
        KomodoObject furtherPatientsModel = patientsExample.addChild(getTransaction(),
                                                                                    furtherPatientsModelName,
                                                                                    VdbLexicon.Vdb.DECLARATIVE_MODEL);
        furtherPatientsModel.setProperty(CoreLexicon.JcrId.MODEL_TYPE, CoreLexicon.ModelType.VIRTUAL);
        furtherPatientsModel.setProperty(VdbLexicon.Model.DESCRIPTION,  "Further Patients View");
        furtherPatientsModel.setProperty(VdbLexicon.Model.VISIBLE, true);
        furtherPatientsModel.setProperty(VdbLexicon.Model.METADATA_TYPE, "DDL");
        StringBuffer patientsModelDefn = new StringBuffer();
        String createViewTxt = "CREATE VIEW FurtherView (";
        patientsModelDefn.append(createViewTxt)
                            .append("id long, ")
                            .append("FurtherDetail1 clob, ")
                            .append("FurtherDetail2 clob, ")
                            .append("FurtherDetail3 clob, ")
                            .append("PRIMARY KEY(id) ")
                            .append(") ")
                            .append("AS ")
                            .append("SELECT id, furtherDetail1, furtherDetail2, furtherDetail3 FROM vdbwebtest.FURTHER_PATIENT;");
        furtherPatientsModel.setProperty(VdbLexicon.Model.MODEL_DEFINITION, patientsModelDefn.toString());

        commit();
//        traverse(getTransaction(), patientsExample);

        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(getTransaction(), patientsExample);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
        System.out.println(testXML);
        VdbErrorHandler errorHandler = new VdbErrorHandler();
        Document testDoc = TestUtilities.createDocument(testXML, errorHandler);
        assertTrue(errorHandler.noExceptions());

        Element docElement = testDoc.getDocumentElement();
        assertEquals("vdb", docElement.getNodeName());

        NodeList vdbNodes = docElement.getChildNodes();
        assertTrue(vdbNodes.getLength() > 0);

        XPath xPath = XPathFactory.newInstance().newXPath();
        String expression = "/vdb/model[@name='" + furtherPatientsModelName + "']/metadata[@type='DDL']";
        Node ddlNode = (Node) xPath.compile(expression).evaluate(testDoc, XPathConstants.NODE);
        assertNotNull(ddlNode);

        Node cDataNode = ddlNode.getFirstChild();
        assertNotNull(cDataNode);
        assertTrue(cDataNode instanceof CharacterData);

        String ddlText = ((CharacterData) cDataNode).getTextContent();
        assertNotNull(ddlText);
        assertTrue(ddlText.startsWith(createViewTxt));
    }

    /**
     * Test to update the vdb by editing the model definition property of the model
     * and ensure that once committed the correct model definition is exported to xml
     *
     * @throws Exception
     */
    @Test(timeout=3000000)
    public void testPatientsVdbUpdateViewThenExport() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject patientsExample = TestUtilities.createPatientsExampleNode(getTransaction(), workspace);

        commit();

        KomodoObject patientsModel = patientsExample.getChild(TestUtilities.PATIENTS_MODEL);
        Property modelDefnProperty = patientsModel.getProperty(VdbLexicon.Model.MODEL_DEFINITION);
        String modelDefn = modelDefnProperty.getStringValue(getTransaction());
        assertNotNull(modelDefn);

        String newViewContent = "CREATE VIEW AnotherServiceView ( id long, furtherDetail clob ) " +
        "AS " +
        "SELECT id, furtherDetail FROM vdbwebtest.MORE_PATIENT;";

        modelDefn = modelDefn + newViewContent;
        patientsModel.setProperty(VdbLexicon.Model.MODEL_DEFINITION, modelDefn);
        commit();

        traverse(getTransaction(), patientsExample);
        //
        // Create node visitor and visit the jcr nodes
        //
        StringWriter testWriter = new StringWriter();
        VdbNodeVisitor visitor = createNodeVisitor(testWriter);
        visitor.visit(getTransaction(), patientsExample);

        //
        // Create an XML Document from the filled writer
        //
        String testXML = testWriter.toString();
        System.out.println(testXML);
        VdbErrorHandler errorHandler = new VdbErrorHandler();
        Document testDoc = TestUtilities.createDocument(testXML, errorHandler);
        assertTrue(errorHandler.noExceptions());

        Element docElement = testDoc.getDocumentElement();
        assertEquals("vdb", docElement.getNodeName());

        NodeList vdbNodes = docElement.getChildNodes();
        assertTrue(vdbNodes.getLength() > 0);

        XPath xPath = XPathFactory.newInstance().newXPath();
        String expression = "/vdb/model[@name='" + TestUtilities.PATIENTS_MODEL + "']/metadata[@type='DDL']";
        Node ddlNode = (Node) xPath.compile(expression).evaluate(testDoc, XPathConstants.NODE);
        assertNotNull(ddlNode);

        Node cDataNode = ddlNode.getFirstChild();
        assertNotNull(cDataNode);
        assertTrue(cDataNode instanceof CharacterData);

        String ddlText = ((CharacterData) cDataNode).getTextContent();
        assertNotNull(ddlText);
        assertTrue(ddlText.contains(newViewContent));
    }
}