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

        String JCR_PATH = Namespace.PREFIX + COLON + "path";

        String JCR_SYSTEM = Namespace.PREFIX + COLON + "system";

        String JCR_UUID = Namespace.PREFIX + COLON + "uuid";
    }

    interface ModeshapeLexicon {

        interface Namespace {
            String URI = "http://www.modeshape.org/1.0";

            String PREFIX = "mode";
        }

        String LOCALNAME = Namespace.PREFIX + COLON + "localName";

        String MODE_SHA1 = Namespace.PREFIX + COLON + "sha1";
    }

    interface CoreLexicon {

        /**
         * The URI and prefix constants of the core namespace.
         */
        public interface Namespace {
            String URI = "http://www.metamatrix.com/metamodels/Core";
            String PREFIX = "mmcore";
        }

        /**
         * The model types processed by the Teiid sequencers.
         */
        interface ModelType {
            String PHYSICAL = "PHYSICAL";

            String VIRTUAL = "VIRTUAL";
        }

        String MODEL_TYPE = Namespace.PREFIX + COLON + "modelType";
    }
}
