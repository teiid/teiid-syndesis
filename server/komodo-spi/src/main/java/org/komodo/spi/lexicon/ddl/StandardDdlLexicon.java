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
package org.komodo.spi.lexicon.ddl;

/**
 * Lexicon for DDL concepts.
 */
public class StandardDdlLexicon {

    public static class Namespace {
        public static final String URI = "http://www.modeshape.org/ddl/1.0";
        public static final String PREFIX = "ddl";
    }

    public static final String PARSER_ID = Namespace.PREFIX + ":parserId";
    public static final String STATEMENTS_CONTAINER = Namespace.PREFIX + ":statements";
    /*
     * mixin types
     *
     * SQL-92 Spec
     *
     *         CREATE SCHEMA
     *        CREATE DOMAIN
     *         CREATE [ { GLOBAL | LOCAL } TEMPORARY ] TABLE
     *        CREATE VIEW
     *        CREATE ASSERTION
     *        CREATE CHARACTER SET
     *        CREATE COLLATION
     *        CREATE TRANSLATION
     *
     *        ALTER TABLE
     */

    public static final String TYPE_MISSING_TERMINATOR = Namespace.PREFIX + ":missingTerminator";
    public static final String TYPE_UNKNOWN_STATEMENT = Namespace.PREFIX + ":unknownStatement";

    public static final String TYPE_OPERATION = Namespace.PREFIX + ":operation";
    public static final String TYPE_OPERAND = Namespace.PREFIX + ":operand";
    public static final String TYPE_STATEMENT = Namespace.PREFIX + ":statement";

    public static final String TYPE_CREATEABLE = Namespace.PREFIX + ":creatable";
    public static final String TYPE_ALTERABLE = Namespace.PREFIX + ":alterable";
    public static final String TYPE_DROPPABLE = Namespace.PREFIX + ":droppable";
    public static final String TYPE_INSERTABLE = Namespace.PREFIX + ":insertable";
    public static final String TYPE_SETTABLE = Namespace.PREFIX + ":settable";
    public static final String TYPE_GRANTABLE = Namespace.PREFIX + ":grantable";
    public static final String TYPE_REVOKABLE = Namespace.PREFIX + ":revokable";

    public static final String TYPE_SCHEMA_OPERAND = Namespace.PREFIX + ":schemaOperand";
    public static final String TYPE_TABLE_OPERAND = Namespace.PREFIX + ":tableOperand";
    public static final String TYPE_DOMAIN_OPERAND = Namespace.PREFIX + ":domainOperand";
    public static final String TYPE_VIEW_OPERAND = Namespace.PREFIX + ":viewOperand";
    public static final String TYPE_ASSERTION_OPERAND = Namespace.PREFIX + ":assertionOperand";
    public static final String TYPE_CHARACTER_SET_OPERAND = Namespace.PREFIX + ":characterSetOperand";
    public static final String TYPE_COLLATION_OPERAND = Namespace.PREFIX + ":collationOperand";
    public static final String TYPE_TRANSLATION_OPERAND = Namespace.PREFIX + ":translationOperand";
    public static final String TYPE_COLUMN_OPERAND = Namespace.PREFIX + ":columnOperand";
    public static final String TYPE_TABLE_CONSTRAINT_OPERAND = Namespace.PREFIX + ":tableConstraintOperand";
    public static final String TYPE_REFERENCE_OPERAND = Namespace.PREFIX + ":referenceOperand";

    public static final String TYPE_CREATE_TABLE_STATEMENT = Namespace.PREFIX + ":createTableStatement";
    public static final String TYPE_CREATE_SCHEMA_STATEMENT = Namespace.PREFIX + ":createSchemaStatement";
    public static final String TYPE_CREATE_VIEW_STATEMENT = Namespace.PREFIX + ":createViewStatement";
    public static final String TYPE_CREATE_DOMAIN_STATEMENT = Namespace.PREFIX + ":createDomainStatement";
    public static final String TYPE_CREATE_ASSERTION_STATEMENT = Namespace.PREFIX + ":createAssertionStatement";
    public static final String TYPE_CREATE_CHARACTER_SET_STATEMENT = Namespace.PREFIX + ":createCharacterSetStatement";
    public static final String TYPE_CREATE_COLLATION_STATEMENT = Namespace.PREFIX + ":createCollationStatement";
    public static final String TYPE_CREATE_TRANSLATION_STATEMENT = Namespace.PREFIX + ":createTranslationStatement";

    public static final String TYPE_ALTER_TABLE_STATEMENT = Namespace.PREFIX + ":alterTableStatement";
    public static final String TYPE_ALTER_DOMAIN_STATEMENT = Namespace.PREFIX + ":alterDomainStatement";
    public static final String TYPE_GRANT_STATEMENT = Namespace.PREFIX + ":grantStatement";
    public static final String TYPE_GRANT_ON_TABLE_STATEMENT = Namespace.PREFIX + ":grantOnTableStatement";
    public static final String TYPE_GRANT_ON_DOMAIN_STATEMENT = Namespace.PREFIX + ":grantOnDomainStatement";
    public static final String TYPE_GRANT_ON_COLLATION_STATEMENT = Namespace.PREFIX + ":grantOnCollationStatement";
    public static final String TYPE_GRANT_ON_CHARACTER_SET_STATEMENT = Namespace.PREFIX + ":grantOnCharacterSetStatement";
    public static final String TYPE_GRANT_ON_TRANSLATION_STATEMENT = Namespace.PREFIX + ":grantOnTranslationStatement";
    public static final String TYPE_REVOKE_STATEMENT = Namespace.PREFIX + ":revokeStatement";
    public static final String TYPE_REVOKE_ON_TABLE_STATEMENT = Namespace.PREFIX + ":revokeOnTableStatement";
    public static final String TYPE_REVOKE_ON_DOMAIN_STATEMENT = Namespace.PREFIX + ":revokeOnDomainStatement";
    public static final String TYPE_REVOKE_ON_COLLATION_STATEMENT = Namespace.PREFIX + ":revokeOnCollationStatement";
    public static final String TYPE_REVOKE_ON_CHARACTER_SET_STATEMENT = Namespace.PREFIX + ":revokeOnCharacterSetStatement";
    public static final String TYPE_REVOKE_ON_TRANSLATION_STATEMENT = Namespace.PREFIX + ":revokeOnTranslationStatement";
    public static final String TYPE_SET_STATEMENT = Namespace.PREFIX + ":setStatement";
    public static final String TYPE_INSERT_STATEMENT = Namespace.PREFIX + ":insertStatement";

    public static final String TYPE_DROP_SCHEMA_STATEMENT = Namespace.PREFIX + ":dropSchemaStatement";
    public static final String TYPE_DROP_TABLE_STATEMENT = Namespace.PREFIX + ":dropTableStatement";
    public static final String TYPE_DROP_VIEW_STATEMENT = Namespace.PREFIX + ":dropViewStatement";
    public static final String TYPE_DROP_DOMAIN_STATEMENT = Namespace.PREFIX + ":dropDomainStatement";
    public static final String TYPE_DROP_CHARACTER_SET_STATEMENT = Namespace.PREFIX + ":dropCharacterSetStatement";
    public static final String TYPE_DROP_COLLATION_STATEMENT = Namespace.PREFIX + ":dropCollationStatement";
    public static final String TYPE_DROP_TRANSLATION_STATEMENT = Namespace.PREFIX + ":dropTranslationStatement";
    public static final String TYPE_DROP_ASSERTION_STATEMENT = Namespace.PREFIX + ":dropAssertionStatement";

    public static final String TYPE_DROP_COLUMN_DEFINITION = Namespace.PREFIX + ":dropColumnDefinition";
    public static final String TYPE_ALTER_COLUMN_DEFINITION = Namespace.PREFIX + ":alterColumnDefinition";
    public static final String TYPE_ADD_COLUMN_DEFINITION = Namespace.PREFIX + ":addColumnDefinition";
    public static final String TYPE_DROP_TABLE_CONSTRAINT_DEFINITION = Namespace.PREFIX + ":dropTableConstraintDefinition";
    public static final String TYPE_ADD_TABLE_CONSTRAINT_DEFINITION = Namespace.PREFIX + ":addTableConstraintDefinition";

    public static final String TYPE_PROBLEM = Namespace.PREFIX + ":ddlProblem";
    public static final String TYPE_COLUMN_DEFINITION = Namespace.PREFIX + ":columnDefinition";
    public static final String TYPE_COLUMN_REFERENCE = Namespace.PREFIX + ":columnReference";
    public static final String TYPE_TABLE_CONSTRAINT = Namespace.PREFIX + ":tableConstraint";
    public static final String TYPE_STATEMENT_OPTION = Namespace.PREFIX + ":statementOption";
    public static final String TYPE_TABLE_REFERENCE = Namespace.PREFIX + ":tableReference";
    public static final String TYPE_FK_COLUMN_REFERENCE = Namespace.PREFIX + ":fkColumnReference";
    public static final String TYPE_CLAUSE = Namespace.PREFIX + ":clause";
    public static final String TYPE_DOMAIN_CONSTRAINT = Namespace.PREFIX + ":domainConstraint";
    public static final String TYPE_SIMPLE_PROPERTY = Namespace.PREFIX + ":simpleProperty";
    /*
     * node property names
     */
    public static final String DDL_EXPRESSION = Namespace.PREFIX + ":expression";
    public static final String DDL_ORIGINAL_EXPRESSION = Namespace.PREFIX + ":originalExpression";
    public static final String DDL_START_LINE_NUMBER = Namespace.PREFIX + ":startLineNumber";
    public static final String DDL_START_COLUMN_NUMBER = Namespace.PREFIX + ":startColumnNumber";
    public static final String DDL_START_CHAR_INDEX = Namespace.PREFIX + ":startCharIndex";
    public static final String DDL_PROBLEM = Namespace.PREFIX + ":problem";
    public static final String DDL_LENGTH = Namespace.PREFIX + ":length";

    public static final String OPTION = Namespace.PREFIX + ":option";
    public static final String TYPE = Namespace.PREFIX + ":type";
    public static final String NEW_NAME = Namespace.PREFIX + ":newName";
    public static final String SQL = Namespace.PREFIX + ":sql";
    public static final String TEMPORARY = Namespace.PREFIX + ":temporary";
    public static final String ON_COMMIT_VALUE = Namespace.PREFIX + ":onCommitValue";
    public static final String NULLABLE = Namespace.PREFIX + ":nullable";
    public static final String DEFAULT_OPTION = Namespace.PREFIX + ":defaultOption";
    public static final String COLLATION_NAME = Namespace.PREFIX + ":collationName";
    public static final String CONSTRAINT_TYPE = Namespace.PREFIX + ":constraintType";
    public static final String DEFERRABLE = Namespace.PREFIX + ":deferrable";
    public static final String CHECK_SEARCH_CONDITION = Namespace.PREFIX + ":searchCondition";
    public static final String DATATYPE_NAME = Namespace.PREFIX + ":datatypeName";
    public static final String DATATYPE_LENGTH = Namespace.PREFIX + ":datatypeLength";
    public static final String DATATYPE_PRECISION = Namespace.PREFIX + ":datatypePrecision";
    public static final String DATATYPE_SCALE = Namespace.PREFIX + ":datatypeScale";
    public static final String DATATYPE_ARRAY_DIMENSIONS = Namespace.PREFIX + ":datatypeArrayDimensions";
    public static final String DEFAULT_VALUE = Namespace.PREFIX + ":defaultValue";
    public static final String DEFAULT_PRECISION = Namespace.PREFIX + ":defaultprecision";
    public static final String VALUE = Namespace.PREFIX + ":value";
    public static final String DROP_BEHAVIOR = Namespace.PREFIX + ":dropBehavior";
    public static final String PROPERTY_VALUE = Namespace.PREFIX + ":propValue";
    public static final String PROBLEM_LEVEL = Namespace.PREFIX + ":problemLevel";
    public static final String GRANT_PRIVILEGE = Namespace.PREFIX + ":grantPrivilege";
    public static final String ALL_PRIVILEGES = Namespace.PREFIX + ":allPrivileges";
    public static final String WITH_GRANT_OPTION = Namespace.PREFIX + ":withGrantOption";
    public static final String GRANTEE = Namespace.PREFIX + ":grantee";

    public static final String CREATE_VIEW_QUERY_EXPRESSION = Namespace.PREFIX + ":queryExpression";
    public static final String CREATE_VIEW_OPTION_CLAUSE = Namespace.PREFIX + ":createViewOption";

    public static final String MESSAGE = Namespace.PREFIX + ":message";

    public static final String EXISTING_NAME = Namespace.PREFIX + ":existingName";
    public static final String COLLATION_CHARACTER_SET_NAME = Namespace.PREFIX + ":characterSetName";
    public static final String COLLATION_SOURCE = Namespace.PREFIX + ":collationSource";
    public static final String PAD_ATTRIBUTE = Namespace.PREFIX + ":padAttribute";

    public static final String SOURCE_CHARACTER_SET_NAME = Namespace.PREFIX + ":sourceCharacterSetName";
    public static final String TARGET_CHARACTER_SET_NAME = Namespace.PREFIX + ":targetCharacterSetName";

    public static final String DROP_OPTION = Namespace.PREFIX + ":dropOption";
    public static final String COLUMN_ATTRIBUTE = Namespace.PREFIX + ":columnAttribute";

    /**
     * value constraints
     */
    public static final String PAD_ATTRIBUTE_PAD = "PAD SPACE";
    public static final String PAD_ATTRIBUTE_NO_PAD = "NO PAD";
    public static final String DEFAULT_ID_LITERAL = "LITERAL";
    public static final String DEFAULT_ID_DATETIME = "DATETIME";
    public static final String DEFAULT_ID_USER = "USER";
    public static final String DEFAULT_ID_CURRENT_USER = "CURRENT_USER";
    public static final String DEFAULT_ID_SESSION_USER = "SESSION_USER";
    public static final String DEFAULT_ID_SYSTEM_USER = "SYSTEM_USER";
    public static final String DEFAULT_ID_NULL = "NULL";

    /*
     * node child types
     */
    public static final String TYPE_DROP_OPTION = Namespace.PREFIX + ":dropOption";
    public static final String TYPE_CONSTRAINT_ATTRIBUTE = Namespace.PREFIX + ":constraintAttribute";
}
