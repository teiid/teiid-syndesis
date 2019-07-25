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

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;

/**
 * Interface for SqlProjectedColumn
 */
public interface SqlProjectedColumn  extends RelationalObject, StringConstants {

    /**
     * The type identifier.
     */
    int TYPE_ID = SqlProjectedColumn.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.SQL_PROJECTED_COLUMN;
    
    /**
     * @param name
     *        the new value for the <code>name</code> property
     * @throws KException
     *         if an error occurs
     */
    void setName(String name) throws KException;
    
    /**
     * @return the value of the <code>name</code> property
     * @throws KException
     *         if an error occurs
     */
    @Override
    String getName() throws KException;
    
    /**
     * @param type
     *        the new value for the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    void setType(String type) throws KException;
    
    /**
     * @return the value of the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    String getType() throws KException;
    
    /**
     * @param selected value for selected
     * @throws KException
     *         if an error occurs         
     */
    void setSelected( boolean selected) throws KException;
    
    /**
     * @return boolean value of selected
     * @throws KException
     *         if an error occurs         
     */
    boolean isSelected() throws KException;
    
}
