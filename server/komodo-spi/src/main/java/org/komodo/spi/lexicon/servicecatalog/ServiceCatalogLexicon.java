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
