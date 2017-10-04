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
package org.komodo.spi.lexicon;

import org.komodo.spi.constants.StringConstants;

public interface LexiconConstants extends StringConstants {

    interface NTLexicon {

        /**
         * The URI and prefix constants of the base namespace.
         */
        interface Namespace {
        
            /**
             * The base namespace prefix. Value is {@value}.
             */
            String PREFIX = "nt";
        
            /**
             * The base namespace URI. Value is {@value}.
             */
            String URI = "http://www.jcp.org/jcr/nt/1.0";
        
        }

        String NT_UNSTRUCTURED = Namespace.PREFIX + COLON + "unstructured";

        String NT_RESOURCE = Namespace.PREFIX + COLON + "resource";

        String NT_FILE = Namespace.PREFIX + COLON + "file";

        String MODEL_TYPE = Namespace.PREFIX + COLON + "modelType";

        /**
         * The model types processed by the Teiid sequencers.
         */
        interface ModelType {
            String PHYSICAL = "PHYSICAL";
        
            String VIRTUAL = "VIRTUAL";
        }
    }

    interface JcrLexicon {

        interface Namespace {
            
            /**
             * The base namespace prefix. Value is {@value}.
             */
            String PREFIX = "jcr";
        
            /**
             * The base namespace URI. Value is {@value}.
             */
            String URI = "http://www.jcp.org/jcr/1.0";
        }

        String JCR_PRIMARY_TYPE = Namespace.PREFIX + COLON + "primaryType";

        String JCR_MIXIN_TYPES = Namespace.PREFIX + COLON + "mixinTypes";

        String JCR_CONTENT = Namespace.PREFIX + COLON + "content";

        String JCR_DATA = Namespace.PREFIX + COLON + "data";
    }
}
