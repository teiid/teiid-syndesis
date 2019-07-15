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
package org.komodo.metadata.internal;

import org.komodo.spi.StringConstants;

public class DocumentType implements StringConstants {

    /**
     * VDB-XML
     */
    public static final DocumentType VDB_XML = new DocumentType(StringConstants.VDB_DEPLOYMENT_SUFFIX);

    /**
     * CONNECTION
     */
    public static final DocumentType CONNECTION = new DocumentType(StringConstants.CONNECTION_SUFFIX);

    /**
     * ZIP
     */
    public static final DocumentType ZIP = new DocumentType(StringConstants.ZIP);

    /**
     * DDL
     */
    public static final DocumentType DDL = new DocumentType(StringConstants.DDL);

    /**
     * JAR
     */
    public static final DocumentType JAR = new DocumentType(StringConstants.JAR);

    /**
     * UNKNOWN
     */
    public static final DocumentType UNKNOWN = new DocumentType(StringConstants.EMPTY_STRING);

    private String type;

    public DocumentType(String type) {
        this.type = type;
    }

    @Override
    public String toString() {
        return this.type;
    }

    /**
     * @param name
     * @return a file name from the given name and the document type
     */
    public String fileName(String name) {
        if (name.endsWith(DOT + type))
            return name; // nothing to do

        if (type.contains(DOT))
            return name + type; // eg. myVdb -vdb.xml

        return name + DOT + type;
    }

    public static DocumentType createDocumentType(String name) {
        if (name == null)
            return DocumentType.UNKNOWN;

        if (name.endsWith(VDB_DEPLOYMENT_SUFFIX))
            return DocumentType.VDB_XML;

        if (name.endsWith(CONNECTION_SUFFIX))
            return DocumentType.CONNECTION;

        int dotIndex = name.lastIndexOf(DOT);
        if (dotIndex == -1)
            return DocumentType.UNKNOWN;

        String suffix = name.substring(dotIndex + 1);
        return new DocumentType(suffix);
    }

    public static DocumentType documentType(String docTypeValue) {
        return new DocumentType(docTypeValue);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DocumentType other = (DocumentType)obj;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equals(other.type))
            return false;
        return true;
    }
}
