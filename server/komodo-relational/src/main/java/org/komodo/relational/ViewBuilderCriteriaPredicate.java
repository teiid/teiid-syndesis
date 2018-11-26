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
package org.komodo.relational;

import java.util.Map;
import org.komodo.utils.StringUtils;

/**
 * Contains Criteria Predicates supplied to ViewDdlBuilder
 */
public class ViewBuilderCriteriaPredicate {
    
    private static final String LH_COLUMN_KEY = "lhColName";  //$NON-NLS-1$
    private static final String RH_COLUMN_KEY = "rhColName";  //$NON-NLS-1$
    private static final String OPERATOR_KEY = "operatorName";  //$NON-NLS-1$
    private static final String COMBINE_KEYWORD_KEY = "combineKeyword";  //$NON-NLS-1$

    private static final String UNDEFINED = "undef";  //$NON-NLS-1$
    
    private String lhColumn;
    private String rhColumn;
    private String operator;
    private String combineKeyword;

    
    /**
     * Constructor
     */
    public ViewBuilderCriteriaPredicate() {
    }
    
    /**
     * Construct the Criteria predicate using the provided map
     * @param predicateMap the map of predicate values
     */
    public ViewBuilderCriteriaPredicate(Map<String,String> predicateMap) {
        setLhColumn(predicateMap.get(LH_COLUMN_KEY));
        setRhColumn(predicateMap.get(RH_COLUMN_KEY));
        setOperator(predicateMap.get(OPERATOR_KEY));
        setCombineKeyword(predicateMap.get(COMBINE_KEYWORD_KEY));
    }
    /**
     * @return the lhColumn
     */
    public String getLhColumn() {
        return StringUtils.isBlank(this.lhColumn) ? UNDEFINED : this.lhColumn;
    }
    /**
     * @param lhColumn the lhColumn to set
     */
    public void setLhColumn(String lhColumn) {
        this.lhColumn = lhColumn;
    }
    /**
     * @return the rhColumn
     */
    public String getRhColumn() {
        return StringUtils.isBlank(this.rhColumn) ? UNDEFINED : this.rhColumn;
    }
    /**
     * @param rhColumn the rhColumn to set
     */
    public void setRhColumn(String rhColumn) {
        this.rhColumn = rhColumn;
    }
    /**
     * @return the operator
     */
    public String getOperator() {
        return StringUtils.isBlank(this.operator) ? UNDEFINED : this.operator;
    }
    /**
     * @param operator the operator to set
     */
    public void setOperator(String operator) {
        this.operator = operator;
    }
    /**
     * @return the combineKeyword
     */
    public String getCombineKeyword() {
        return StringUtils.isBlank(this.combineKeyword) ? UNDEFINED : this.combineKeyword;
    }
    /**
     * @param combineKeyword the combineKeyword to set
     */
    public void setCombineKeyword(String combineKeyword) {
        this.combineKeyword = combineKeyword;
    }

    /**
     * @return 'true' if all values are provided
     */
    public boolean isComplete() {
        if(StringUtils.isBlank(this.lhColumn) || StringUtils.isBlank(this.rhColumn) || StringUtils.isBlank(this.operator) || StringUtils.isBlank(this.combineKeyword)) {
            return false;
        }
        return true;
    }
}
