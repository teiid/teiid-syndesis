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
package org.komodo.spi.query.proc.wsdl;

/**
 *
 */
public interface WsdlConstants {

    String KEY_REQUEST_PROCEDURE_NAME = "requestProcedureName"; //$NON-NLS-1$
    String KEY_RESPONSE_PROCEDURE_NAME = "responseProcedureName"; //$NON-NLS-1$
    String KEY_WRAPPER_PROCEDURE_NAME = "wrapperProcedureName"; //$NON-NLS-1$
    String SQL_BEGIN = "CREATE VIRTUAL PROCEDURE\nBEGIN\n"; //$NON-NLS-1$
    String SQL_END = "\nEND"; //$NON-NLS-1$
    String REQUEST = "REQUEST"; //$NON-NLS-1$
    String RESPONSE = "RESPONSE"; //$NON-NLS-1$
    String REQUEST_LOWER = "request"; //$NON-NLS-1$
    String RESPONSE_LOWER = "response"; //$NON-NLS-1$
    String TABLE_EXEC = "TABLE(EXEC "; //$NON-NLS-1$
    String XMI_EXTENSION = ".xmi"; //$NON-NLS-1$
    String RESULT_LOWER = "result"; //$NON-NLS-1$
    String INVOKE_SEGMENT_1 = "invoke('"; //$NON-NLS-1$
    String INVOKE_SEGMENT_2 = "', null, REQUEST.xml_out, null, TRUE))"; //$NON-NLS-1$
    String NULL_LOWER = "null"; //$NON-NLS-1$
    String XSI_NAMESPACE_PREFIX = "xsi"; //$NON-NLS-1$
    int TYPE_BODY = 0;
    int TYPE_HEADER = 1;
    Object XML_OUT = "xml_out"; //$NON-NLS-1$
    
    enum ProcedureType {
        REQUEST,
        RESPONSE,
        BOTH
    }

}
