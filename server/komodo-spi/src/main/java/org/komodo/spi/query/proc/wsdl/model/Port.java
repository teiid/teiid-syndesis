/*************************************************************************************
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
package org.komodo.spi.query.proc.wsdl.model;

/**
 * This class represents a port as defined in a WSDL
 *
 *
 */
public interface Port extends WsdlElement {
	
	static final String HTTP = "HTTP"; //$NON-NLS-1$
    static final String SOAP11 = "SOAP11"; //$NON-NLS-1$
    static final String SOAP12 = "SOAP12"; //$NON-NLS-1$
	
	static final String HTTP_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/http/"; //$NON-NLS-1$
	static final String SOAP11_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/soap/"; //$NON-NLS-1$
	static final String SOAP12_TRANSPORT_URI = "http://schemas.xmlsoap.org/wsdl/soap12/"; //$NON-NLS-1$

    /**
     * @return a binding defined in this port
     */
    Binding getBinding();

    /**
     * @return the service that defines this port
     */
    Service getService();
    
    /**
     * @param uri - the binding type (SOAP11, SOAP12 or HTTP). 
     */
    String getBindingType( );
    
    /**
     * @param uri - the binding namespace URI attribute of the <soap:address> element. 
     */
    String getBindingTypeURI();

    /**
     * @return the location attribute of the <soap:address> element. The endpoint URL for the port.
     */
    String getLocationURI();

    String getNamespaceURI();

}
