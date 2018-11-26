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
package org.komodo.spi.repository;

import java.io.File;
import java.util.List;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;

/**
 * Validation Manager interface
 */
public interface ValidationManager {

    /**
     * @param rulesXmlFile
     *        the file whose rule definitions are being validated (cannot be <code>null</code>)
     * @return a list of errors (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    List< String > validateRules( final File rulesXmlFile ) throws KException;

    /**
     * @param uow the transaction
     * @param rulesXmlFile
     *        the file whose rule definitions are being imported (cannot be <code>null</code>)
     * @param overwriteExisting 'true' will replace all existing rules.
     * @throws KException
     *         if an error occurs
     */
    void importRules( final UnitOfWork uow, final File rulesXmlFile, boolean overwriteExisting ) throws KException;
    
    /**
     * Gets all of the current rules
     * @param uow the transaction
     * @return all rules (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Rule[] getAllRules( final UnitOfWork uow ) throws KException;

    /**
     * Gets the Rules that are valid for the supplied KomodoObject
     * @param uow the transaction
     * @param kObj the KomodoObject
     * @return applicable rules (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Rule[] getRules( final UnitOfWork uow, final KomodoObject kObj ) throws KException;

    /**
     * Get the rule with the specified RuleId
     * @param uow the transaction
     * @param ruleId the Rule ID
     * @return the rule or <code>null</code> if not found
     * @throws KException if an error occurs
     */
    Rule getRule( final UnitOfWork uow, final String ruleId ) throws KException;

    /**
     * Determine if the specified RuleId is applicable for the supplied KomodoObject
     * @param uow the transaction
     * @param ruleId the Rule ID
     * @param kObj the Komodo Object
     * @return <code>true</code> if the rule is applicable, <code>false</code> if not.
     * @throws KException if an error occurs
     */
    boolean isApplicable( final UnitOfWork uow, final String ruleId, final KomodoObject kObj ) throws KException;

    /**
     * Set the Rule enablement
     * @param uow the transaction
     * @param isEnabled <code>true</code> to enable Rule, <code>false</code> to disable.
     * @param ruleId the Rule IDs
     * @throws KException if an error occurs
     */
    void setRuleEnabled(final UnitOfWork uow, final boolean isEnabled, final String... ruleId ) throws KException; 

    /**
     * Set the Rule severity
     * @param uow the transaction
     * @param severity the rule severity
     * @param ruleId the Rule IDs
     * @throws KException if an error occurs
     */
    void setRuleSeverity(final UnitOfWork uow, final Outcome.Level severity, final String... ruleId ) throws KException; 
    
    /**
     * Evaluate the supplied object using all applicable rules for the object.
     * @param uow the transaction
     * @param kObject the object to validate
     * @param full <code>true</code> will validate the node and all of its ancestors.  <code>false</code> only validates the supplied node.
     * @return the results from all rule validations (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Result[] evaluate( final UnitOfWork uow, final KomodoObject kObject, boolean full  ) throws KException;

    /**
     * Evaluate the supplied object using the specified rules (if the rule is applicable for the object).
     * @param uow the transaction
     * @param kObject the kObj
     * @param ruleId the rule IDs
     * @return the results of the evaluation (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Result[] evaluate( final UnitOfWork uow, final KomodoObject kObject, final String... ruleId ) throws KException;

}
