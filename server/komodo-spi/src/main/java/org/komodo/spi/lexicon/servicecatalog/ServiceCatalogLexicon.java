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
package org.komodo.spi.lexicon.servicecatalog;

/**
 * Constants associated with the DataVirtualization namespace
 */
public interface ServiceCatalogLexicon {

    /**
     * The URI and prefix constants of the DV namespace.
     */
    interface Namespace {
        /**
         * The service catalog namespace prefix. Value is {@value}.
         */
        String PREFIX = "sc";

        /**
         * The service catalog namespace URI. Value is {@value}.
         */
        String URI = "http://www.jboss.org/sc/1.0";
    }
    
    interface DataService {
        /**
         * The name of the property whose value contains the data service name. Value is {@value}.
         */
        String NAME = Namespace.PREFIX + ":name";
        
        /**
         * The name of the property whose value contains the data service type. Value is {@value}.
         */
        String TYPE = Namespace.PREFIX + ":type";        

        /**
         * The name of the property whose value contains the data service is bound or not. Value is {@value}.
         */
        String BOUND = Namespace.PREFIX + ":bound";

        /**
         * The name of the property whose value contains the data service's recommended translator. Value is {@value}.
         */		
        String TRANSLATOR = Namespace.PREFIX + ":translator";           
    }
}
