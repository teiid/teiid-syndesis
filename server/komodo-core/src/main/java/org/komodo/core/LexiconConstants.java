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
package org.komodo.core;

import org.komodo.spi.StringConstants;

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

        /*
         * JcrConstants directly from modeshape
         */
        
        String JCR_SYSTEM = Namespace.PREFIX + COLON + "system";

        String JCR_UUID = Namespace.PREFIX + COLON + "uuid";
    }

}
