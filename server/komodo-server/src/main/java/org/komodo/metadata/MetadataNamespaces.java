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
package org.komodo.metadata;

import java.util.Map;

import org.komodo.spi.StringConstants;
import org.komodo.utils.KeyInValueMap;
import org.komodo.utils.KeyInValueMap.KeyFromValueAdapter;

/**
 *
 */
public interface MetadataNamespaces extends StringConstants {

    /**
     * Teiid Prefix
     */
    String TEIID_RESERVED = "teiid_"; //$NON-NLS-1$

    /**
     * Teiid Saleforce Prefix
     */
    String TEIID_SF = "teiid_sf"; //$NON-NLS-1$

    /**
     * Teiid Relational Prefix
     */
    String TEIID_RELATIONAL = "teiid_rel"; //$NON-NLS-1$

    /**
     * Teiid Web Services Prefix
     */
    String TEIID_WS = "teiid_ws"; //$NON-NLS-1$

    /**
     * Teiid Mongo Prefix
     */
    String TEIID_MONGO = "teiid_mongo"; //$NON-NLS-1$

    /**
     * Teiid Odata Prefix
     */
    String TEIID_ODATA = "teiid_odata"; //$NON-NLS-1$

    /**
     * Teiid Accumulo Prefix
     */
    String TEIID_ACCUMULO = "teiid_accumulo"; //$NON-NLS-1$

    /**
     * Teiid Excel Prefix
     */
    String TEIID_EXCEL = "teiid_excel"; //$NON-NLS-1$

    /**
     * Teiid JPA Prefix
     */
    String TEIID_JPA = "teiid_jpa"; //$NON-NLS-1$

    /**
     * Relational URI
     */
    String RELATIONAL_URI = "{http://www.teiid.org/ext/relational/2012}"; //$NON-NLS-1$

    /**
     * Salesforce URI
     */
    String SF_URI = "{http://www.teiid.org/translator/salesforce/2012}"; //$NON-NLS-1$

    /**
     * Web Services URI
     */
    String WS_URI = "{http://www.teiid.org/translator/ws/2012}"; //$NON-NLS-1$

    /**
     * Mongo URI
     */
    String MONGO_URI = "{http://www.teiid.org/translator/mongodb/2013}"; //$NON-NLS-1$

    /**
     * Odata URI
     */
    String ODATA_URI = "{http://www.jboss.org/teiiddesigner/ext/odata/2012}"; //$NON-NLS-1$

    /**
     * Accumulo URI
     */
    String ACCUMULO_URI = "{http://www.teiid.org/translator/accumulo/2013}"; //$NON-NLS-1$

    /**
     * Excel URI
     */
    String EXCEL_URI = "{http://www.teiid.org/translator/excel/2014}"; //$NON-NLS-1$

    /**
     * JPA URI
     */
    String JPA_URI = "{http://www.teiid.org/translator/jpa/2014}"; //$NON-NLS-1$

    /**
     * Enumerator of the namespaces
     */
    public class URI {

        /**
         * Salesforce
         */
        public static URI SF = new URI(TEIID_SF, SF_URI);

        /**
         * Relational
         */
        public static URI RELATIONAL = new URI(TEIID_RELATIONAL, RELATIONAL_URI);

        /**
         * Web Services
         */
        public static URI WS = new URI(TEIID_WS, WS_URI);

        /**
         * Mongo
         */
        public static URI MONGO = new URI(TEIID_MONGO, MONGO_URI);

        /**
         * Odata
         */
        public static URI ODATA = new URI(TEIID_ODATA, ODATA_URI);

        /**
         * Accumulo
         */
        public static URI ACCUMULO = new URI(TEIID_ACCUMULO, ACCUMULO_URI);

        /**
         * Excel
         */
        public static URI EXCEL = new URI(TEIID_EXCEL, EXCEL_URI);

        /**
         * JPA
         */
        public static URI JPA = new URI(TEIID_JPA, JPA_URI);

        private String prefix;

        private String uri;

        private String unbracedURI;

        private static KeyInValueMap<String, URI> nsMap = null;

        /**
         * @param prefix uri prefix
         * @param uri the uri
         */
        public URI(String prefix, String uri) {
            this.prefix = prefix;
            this.uri = uri;

            this.unbracedURI = uri;
            if (this.unbracedURI.startsWith(OPEN_BRACE))
                this.unbracedURI = this.unbracedURI.substring(1);

            if (this.unbracedURI.endsWith(CLOSE_BRACE))
                this.unbracedURI = this.unbracedURI.substring(0, this.unbracedURI.length() - 1);
        }

        /**
         * @return the prefix
         */
        public String getPrefix() {
            return this.prefix;
        }

        /**
         * @return the uri
         */
        public String getUri() {
            return this.uri;
        }

        /**
         * @return the unbracedURI
         */
        public String getUnbracedURI() {
            return this.unbracedURI;
        }

        /**
         * @return a map of the URIs keyed by their prefixes
         */
        public static Map<String, URI> map() {
            if (nsMap == null) {
                nsMap = new KeyInValueMap<String, MetadataNamespaces.URI>(new URIMapAdapter());
                nsMap.add(SF);
                nsMap.add(RELATIONAL);
                nsMap.add(WS);
                nsMap.add(MONGO);
                nsMap.add(ODATA);
                nsMap.add(ACCUMULO);
                nsMap.add(EXCEL);
                nsMap.add(JPA);
            }

            return nsMap;
        }
    }

    /**
     * Adapter for getting the prefix key from a {@link URI} value
     */
    class URIMapAdapter implements KeyFromValueAdapter<String, URI> {
        @Override
        public String getKey(URI value) {
            return value.getPrefix();
        }
    }

}
