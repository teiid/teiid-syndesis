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

/**
 * Interface for SqlComposition
 */
public interface SqlComposition  extends RelationalObject, StringConstants {

    /**
     * @param branch
     *        the new value for the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription(String description);
    
    /**
     * @return the value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    String getDescription();
    
    /**
     * @param branch
     *        the new value for the <code>leftSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLeftSourcePath(String leftSource);
    
    /**
     * @return the value of the <code>leftSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    String getLeftSourcePath();
    
    /**
     * @param branch
     *        the new value for the <code>rightSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRightSourcePath(String rightSource);
    
    /**
     * @return the value of the <code>rightSourcePath</code> property
     * @throws KException
     *         if an error occurs
     */
    String getRightSourcePath();
    
    /**
     * @param branch
     *        the new value for the <code>leftCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLeftCriteriaColumn(String leftCriteriaColumn);
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>leftCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    String getLeftCriteriaColumn();
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>rightCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRightCriteriaColumn(String rightCriteriaColumn);
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>rightCriteriaColumn</code> property
     * @throws KException
     *         if an error occurs
     */
    String getRightCriteriaColumn();
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    void setType(String type);
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>type</code> property
     * @throws KException
     *         if an error occurs
     */
    String getType();
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param branch
     *        the new value for the <code>operator</code> property
     * @throws KException
     *         if an error occurs
     */
    void setOperator(String operator);
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>operator</code> property
     * @throws KException
     *         if an error occurs
     */
    String getOperator();
    
    
}
