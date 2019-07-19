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
package org.komodo.relational.profile;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;

/**
 * Interface for SqlComposition
 */
public interface SqlComposition  extends RelationalObject, StringConstants {

    /**
     * The type identifier.
     */
    int TYPE_ID = SqlComposition.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.SQL_COMPOSITION;
    
    /**
     * @param branch
     *        the new value for the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription(String description) throws KException;
    
    /**
     * @return the value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    String getDescription() throws KException;
    
    /**
     * @param branch
     *        the new value for the <code>leftSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLeftSourcePath(String leftSource) throws KException;
    
    /**
     * @return the value of the <code>leftSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    String getLeftSourcePath() throws KException;
    
    /**
     * @param branch
     *        the new value for the <code>rightSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRightSourcePath(String rightSource) throws KException;
    
    /**
     * @return the value of the <code>rightSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    String getRightSourcePath() throws KException;
    
    /**
     * @param branch
     *        the new value for the <code>leftCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLeftCriteriaColumn(String leftCriteriaColumn) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>leftCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    String getLeftCriteriaColumn() throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>rightCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRightCriteriaColumn(String rightCriteriaColumn) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>rightCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    String getRightCriteriaColumn() throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    void setType(String type) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    String getType() throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>operator</code> property
     * @throws KException
     *         if an error occurs
     */
    void setOperator(String operator) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>operator</code> property
     * @throws KException
     *         if an error occurs
     */
    String getOperator() throws KException;
    
    
}
