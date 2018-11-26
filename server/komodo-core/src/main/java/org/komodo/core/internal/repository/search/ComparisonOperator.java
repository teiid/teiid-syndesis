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
package org.komodo.core.internal.repository.search;

/**
 *
 */
public enum ComparisonOperator {

    /**
     * Equals
     */
    EQUALS("="), //$NON-NLS-1$

    /**
     * Not Equals
     */
    NOT_EQUALS("!="), //$NON-NLS-1$

    /**
     * Less Than
     */
    LESS_THAN("<"), //$NON-NLS-1$

    /**
     * Less Than or Equal To
     */
    LESS_THAN_EQUAL_TO("<="), //$NON-NLS-1$

    /**
     * Greater Than
     */
    GREATER_THAN(">"), //$NON-NLS-1$

    /**
     * Greater Than or Equal To
     */
    GREATER_THAN_EQUAL_TO(">="), //$NON-NLS-1$

    /**
     * Like
     */
    LIKE("LIKE"), //$NON-NLS-1$

    /**
     * Not Like
     */
    NOT_LIKE("NOT LIKE"); //$NON-NLS-1$

    private String symbol;

    ComparisonOperator(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return this.symbol;
    }

    /**
     * @param symbol the symbol
     * @return the operator for the symbol or null
     */
    public static ComparisonOperator findOperator(String symbol) {
        for (ComparisonOperator operator : ComparisonOperator.values()) {
            if (operator.toString().equalsIgnoreCase(symbol))
                return operator;
        }

        return null;
    }
}
