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
 * This class represents an Operation as defined in the WSDL It does not contain any information about the messages that are used
 * by the operation as they are of no interest until it is time to actually create an MM model
 *
 *
 */
public interface Operation extends WsdlElement {

    /**
     * @return the binding that contains this operation
     */
    Binding getBinding();

    /**
     * @return the name of the input message
     */
    Message getInputMessage();

    /**
     * @return the name of the output message
     */
    Message getOutputMessage();

    /**
     * @return the style of the operation
     */
    String getStyle();

    /**
     * @return an array of the names of possible faults
     */
    Fault[] getFaults();

    String getSOAPAction();

    boolean canModel();

    String[] getProblemMessages();
}
