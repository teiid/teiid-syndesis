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
package org.komodo.spi.query;

/**
 * Types of statement
 */
public enum StatementType {

    /** 
    * Represents an unknown type of statement 
    */
    TYPE_UNKNOWN,

    /**
     * Represents a IF statement
     */
    TYPE_IF,

    /**
     * Represents a SQL COMMAND statement
     */
    TYPE_COMMAND,

    /**
     * Represents a DECLARE statement
     */
    TYPE_DECLARE,

    /**
     * Represents a ERROR statement
     */
    TYPE_ERROR,

    /**
     * Represents a ASSIGNMENT statement
     */
    TYPE_ASSIGNMENT,

    /**
     * Represents a LOOP statement
     */
    TYPE_LOOP,

    /**
     * Represents a WHILE statement
     */
    TYPE_WHILE,

    /**
     * Represents a CONTINUE statement
     */
    TYPE_CONTINUE,

    /**
     * Represents a BREAK statement
     */
    TYPE_BREAK,

    /**
     * Represents a UPDATE statement
     */
    TYPE_UPDATE,

    /**
     * Represents a COMPOUND statement
     */
    TYPE_COMPOUND,

    /**
     * Represents a LEAVE statement
     */
    TYPE_LEAVE,

    /**
     * Represents a RETURN statement
     */
    TYPE_RETURN;

    /**
     * @param name
     * @return StatementType for given name
     */
    public static StatementType findStatementType(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (StatementType statementType : values()) {
            if (statementType.name().equals(name))
                return statementType;
        }

        return null;
    }

    /**
     * @param index
     * @return StatementType for given index
     */
    public static StatementType findStatementType(int index) {
        for (StatementType type : values()) {
            if (type.ordinal() == index)
                return type;
        }

        return null;
    }
}