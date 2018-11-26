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

import java.util.HashMap;
import java.util.Map;

import org.komodo.spi.query.proc.wsdl.model.Operation;

/**
 *
 */
public interface WsdlProcedureInfo extends WsdlConstants {
    
    String getDefaultProcedureName();

    Map<String, String> getNamespaceMap();

    ProcedureType getType();

    WsdlColumnInfo[] getBodyColumnInfoList();

    WsdlColumnInfo[] getHeaderColumnInfoList();

    String getProcedureName();
    
    HashMap<String, String> getReverseNSMap();

    /**
     * 
     * @return rootPath the root path xquery expression
     */
    String getRootPath();

    Operation getOperation();

    String getUniqueBodyColumnName(String proposedName);

    String getUniqueHeaderColumnName(String proposedName);

    WsdlWrapperInfo getWrapperProcedure();

}
