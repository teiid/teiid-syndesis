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
package org.komodo.relational.dataservice;

import org.komodo.spi.StringConstants;

/**
 * Interface for SqlProjectedColumn
 */
public interface SqlProjectedColumn  extends StringConstants {

    /**
     * @param name
     *        the new value for the <code>name</code> property
     */
    void setName(String name);
    
    /**
     * @return the value of the <code>name</code> property
     */
    String getName();
    
    /**
     * @param type
     *        the new value for the <code>type</code> property
     */
    void setType(String type);
    
    /**
     * @return the value of the <code>type</code> property
     */
    String getType();
    
    /**
     * @param selected value for selected
     */
    void setSelected( boolean selected);
    
    /**
     * @return boolean value of selected
     */
    boolean isSelected();
    
}
