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
package org.komodo.rest.schema.json;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import com.sun.xml.xsom.parser.AnnotationContext;
import com.sun.xml.xsom.parser.AnnotationParser;

class XsdAnnotationParser extends AnnotationParser {

    private static final String DOCUMENTATION = "documentation"; //$NON-NLS-1$

    private StringBuilder docBuffer = new StringBuilder();

    @Override
    public ContentHandler getContentHandler(AnnotationContext context, String parentElementName, ErrorHandler handler, EntityResolver resolver) {
        return new ContentHandler() {
            private boolean parsingDocumentation = false;

            @Override
            public void characters(char[] ch, int start, int length) {
                if (parsingDocumentation) {
                    docBuffer.append(ch, start, length);
                }
            }

            @Override
            public void startElement(String uri, String localName, String qName, Attributes atts) {
                if (localName.equals(DOCUMENTATION)) {
                    parsingDocumentation = true;
                }
            }

            @Override
            public void endElement(String uri, String localName, String name) {
                if (localName.equals(DOCUMENTATION)) {
                    parsingDocumentation = false;
                }
            }

            @Override
            public void ignorableWhitespace(char[] ch, int start, int length) {
                // Not required
            }

            @Override
            public void skippedEntity(String name) {
                // Not required
            }

            @Override
            public void setDocumentLocator(Locator locator) {
                // Not required
            }

            @Override
            public void startDocument() {
                // Not required
            }

            @Override
            public void endDocument() {
                // Not required
            }

            @Override
            public void startPrefixMapping(String prefix, String uri) {
                // Not required
            }

            @Override
            public void endPrefixMapping(String prefix) {
                // Not required
            }

            @Override
            public void processingInstruction(String target, String data) {
                // Not required
            }
        };
    }

    @Override
    public Object getResult(Object existing) {
        return docBuffer.toString().trim();
    }
}
