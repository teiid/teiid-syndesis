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
 * This class represents a Binding as defined in a WSDL
 *
 *
 */
public interface Binding extends WsdlElement {

    /**
     * @return returns the operations defined within the Binding
     */
    Operation[] getOperations();

    /**
     * @return the port that contains this binding
     */
    Port getPort();

    /**
     * @return uri the URI for the SOAP Binding
     */
    String getTransportURI();

    /**
     * This returns the style information returned by the SOAP binding (RPC or DOC)
     * 
     * @return the style for the SOAP web service
     */
    String getStyle();
}
