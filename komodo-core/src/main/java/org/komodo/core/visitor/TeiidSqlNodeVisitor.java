/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.core.visitor;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants.NonReserved;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants.Reserved;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlConstants.Tokens;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlContext;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon;
import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.*;
import org.komodo.spi.query.AggregateFunctions;
import org.komodo.spi.query.BranchingMode;
import org.komodo.spi.query.CriteriaOperator;
import org.komodo.spi.query.DisplayMode;
import org.komodo.spi.query.JoinTypeTypes;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.query.MatchMode;
import org.komodo.spi.query.Operation;
import org.komodo.spi.query.ParameterInfo;
import org.komodo.spi.query.PredicateQuantifier;
import org.komodo.spi.query.TriggerEvent;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.OperationType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.MetadataVersion;
import org.komodo.spi.type.DataTypeService;
import org.komodo.spi.type.DataTypeService.DataTypeName;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.teiid.language.SortSpecification;
import org.teiid.language.SortSpecification.NullOrdering;

/**
 *
 */
public class TeiidSqlNodeVisitor extends AbstractNodeVisitor implements StringConstants,
    NonReserved, Reserved, Tokens {

    protected static final String UNDEFINED = "<undefined>"; //$NON-NLS-1$

    protected static final String BEGIN_HINT = "/*+"; //$NON-NLS-1$

    protected static final String END_HINT = "*/"; //$NON-NLS-1$

    private static final String NODE_KEY = "node"; //$NON-NLS-1$

    private static final String KEYWORD_KEY = "keyword"; //$NON-NLS-1$

    private static final String SHORT_NAME_ONLY_KEY = "shortNameOnly"; //$NON-NLS-1$

    protected class TeiidSqlNodeContext implements TeiidSqlContext {

        private final UnitOfWork transaction;

        private final KomodoObject node;

        private Map<String, Object> index = new HashMap<String, Object>();

        public TeiidSqlNodeContext(UnitOfWork transaction, KomodoObject node) {
            this.transaction = transaction;
            this.node = node;
        }

        public UnitOfWork tx() {
            return transaction;
        }
    
        public Object get(String key) {
            if (NODE_KEY.equals(key))
                return node;

            return index.get(key);
        }

    
        public void add(String key, Object obj) {
            index.put(key, obj);
        }
    }

    private KLog logger = KLog.getLogger();

    private StringBuilder builder;

    /**
     * Create new instance
     *
     * @param teiidVersion teiid version
     * @param dataTypeService the data type service
     */
    public TeiidSqlNodeVisitor(MetadataVersion teiidVersion, DataTypeService dataTypeService) {
        super(teiidVersion, dataTypeService);
    }


    protected String undefined() {
        return UNDEFINED;
    }

    /**
     * @param node node to be visited
     * @return SQL String representation of the given node
     * @throws Exception if node causes a failure
     */
    public String getTeiidSql(UnitOfWork transaction, KomodoObject node) throws Exception {
        if (node == null)
            return undefined();

        this.builder = new StringBuilder();

        node.accept(transaction, this);
        return builder.toString();
    }

    protected void append(String value) {
        builder.append(value);
    }

    protected void beginClause(int level) {
        append(SPACE);
    }

    protected String stripNameSpace(String name) {
        name = name.replace(TeiidSqlLexicon.Namespace.PREFIX, EMPTY_STRING);
        name = name.replaceAll("[\\p{C}\\p{Z}:]", EMPTY_STRING); //$NON-NLS-1$
        return name;
    }

    protected void appendToken(String name) {
        name = stripNameSpace(name);
        append(name.toUpperCase());
    }

    protected void appendToken(UnitOfWork transaction, KomodoObject node) throws Exception {
        String name = node.getName(transaction);
        appendToken(name);
    }

    protected KomodoObject reference(UnitOfWork transaction, KomodoObject node, String refName) throws Exception {
        if (node == null || refName == null)
            return null;
        
        if (! node.hasChild(transaction, refName))
            return null;

        return node.getChild(transaction, refName);
    }

    protected int size(UnitOfWork transaction, KomodoObject node, String refName)  throws Exception {
        if (node == null || refName == null)
            return 0;

        if (! node.hasChild(transaction, refName))
            return 0;

        KomodoObject[] nodes = node.getChildren(transaction, refName);
        return nodes.length;
    }

    protected KomodoObject[] references(UnitOfWork transaction, KomodoObject node, String refName) throws Exception {
        if (node == null || refName == null)
            return null;

        if (! node.hasChild(transaction, refName))
            return new KomodoObject[0];

        return node.getChildren(transaction, refName);
    }

    protected void iterate(UnitOfWork transaction, KomodoObject[] nodes) throws Exception {
        for (int i = 0; i < nodes.length; ++i) {
            if (i > 0)
                append(COMMA + SPACE);

            KomodoObject node = nodes[i];
            visit(transaction, node);
        }
    }

    protected void iterate(UnitOfWork transaction, KomodoObject node, String refName) throws Exception {
        KomodoObject[] nodes = references(transaction, node, refName);
        for (int i = 0; i < nodes.length; ++i) {
            if (i > 0)
                append(COMMA + SPACE);

            KomodoObject childKomodoObject = nodes[i];
            visit(transaction, childKomodoObject);
        }
    }

    protected boolean propertyBoolean(UnitOfWork transaction, KomodoObject node, String propName) throws Exception {
        Property property = property(transaction, node, propName);
        if (property == null)
            return false;
    
        Boolean value = property.getBooleanValue(transaction);
        return value;
    }

    protected String propertyString(UnitOfWork transaction, KomodoObject node, String propName) throws Exception {
        Property property = property(transaction, node, propName);
        if (property == null)
            return null;
    
        String value = property.getStringValue(transaction);
        return value;
    }

    protected long propertyLong(UnitOfWork transaction, KomodoObject node, String propName) throws Exception {
        Property property = property(transaction, node, propName);
        if (property == null)
            return -1L;
    
        Long value = property.getLongValue(transaction);
        return value;
    }

    @SuppressWarnings( "unchecked" )
    private <T> T propertyValue(UnitOfWork transaction, Property property, DataTypeName dataTypeName) throws Exception {
        if (property == null)
            return null;

        ArgCheck.isNotNull(dataTypeName, "dataTypeName");
        Object valueObject = null;

        switch (dataTypeName) {
            case STRING:
            case CHAR:
            case VARCHAR:
            case XML:
            case DATE:
            case TIME:
            case TIMESTAMP:
                valueObject = property.getStringValue(transaction);
                break;
            case DOUBLE:
            case FLOAT:
            case DECIMAL:
            case BIGDECIMAL:
                valueObject = property.getDoubleValue(transaction);
                break;
            case LONG:
            case BYTE:
            case INTEGER:
            case REAL:
            case BIGINT:
            case SHORT:
            case TINYINT:
            case SMALLINT:
            case BIGINTEGER:
                valueObject = property.getLongValue(transaction);
                break;
            case BOOLEAN:
                valueObject = property.getBooleanValue(transaction);
                break;
            case BLOB:
            case CLOB:
            case OBJECT:
            case VARBINARY:
                valueObject = property.getBinaryValue(transaction);
                break;
            default:
                throw new UnsupportedOperationException();
        }

        return (T) valueObject;
    }

    protected <T> T propertyValue(UnitOfWork transaction, KomodoObject node, String propName, DataTypeName dataTypeName) throws Exception {
        Property property = property(transaction, node, propName);
        if (property == null)
            return null;

        T value = propertyValue(transaction, property, dataTypeName);
        return value;
    }

    @SuppressWarnings( "unchecked" )
    protected <T> Collection<T> propertyValues(UnitOfWork transaction, KomodoObject node, String propName, DataTypeName dataTypeName) throws Exception {
        ArgCheck.isNotNull(dataTypeName, "dataTypeName");

        Collection<T> collection = Collections.emptyList();
        Property property = property(transaction, node, propName);
        if (property == null)
            return collection;

        if (! property.isMultiple(transaction)) {

            T value = propertyValue(transaction, property, dataTypeName);
            if (value != null)
                collection = Collections.singleton(value);

        } else {
            T[] values = null;

            switch (dataTypeName) {
                case STRING:
                case CHAR:
                case VARCHAR:
                case XML:
                case DATE:
                case TIME:
                case TIMESTAMP:
                    values = (T[]) property.getStringValues(transaction);
                    break;
                case DOUBLE:
                case FLOAT:
                case DECIMAL:
                case BIGDECIMAL:
                    values = (T[]) property.getDoubleValues(transaction);
                    break;
                case LONG:
                case BYTE:
                case INTEGER:
                case REAL:
                case BIGINT:
                case SHORT:
                case TINYINT:
                case SMALLINT:
                case BIGINTEGER:
                    values = (T[]) property.getLongValues(transaction);
                    break;
                case BOOLEAN:
                    values =(T[]) property.getBooleanValues(transaction);
                    break;
                default:
                    throw new UnsupportedOperationException();
            }

            collection = Arrays.asList(values);
        }

        return collection;
    }

    protected boolean isTeiidSqlType(Descriptor nodeType) {
        if (nodeType == null)
            return false;

        return nodeType.getName().startsWith(TeiidSqlLexicon.Namespace.PREFIX + COLON);
    }

    protected boolean instanceOf(UnitOfWork transaction, KomodoObject node, LexTokens superTypeToken) throws Exception {
        if (node == null)
            return false;

        Descriptor tsqlType = findTeiidSqlType(transaction, node);
        if (tsqlType == null)
            return false;

        LexTokens stToken = LexTokens.findClass(tsqlType.getName());
        if (superTypeToken.equals(stToken))
            return true;

        Descriptor[] superTypes = node.getObjectFactory().getParentDescriptors(transaction, node.getRepository(), tsqlType);

        for (Descriptor superType : superTypes) {
            if (! isTeiidSqlType(superType))
                continue;

            stToken = LexTokens.findClass(superType.getName());
            if (superTypeToken.equals(stToken))
                return true;
        }

        return false;
    }

    protected Descriptor findTeiidSqlType(UnitOfWork transaction, KomodoObject node) throws Exception {
        Descriptor[] mixinTypes = node.getDescriptors(transaction);
        if (mixinTypes.length == 0)
            return null;

        Descriptor tsqlMixinType = null;
        for (Descriptor mixinType : mixinTypes) {
            if (! isTeiidSqlType(mixinType))
                continue;

            tsqlMixinType = mixinType;
            break;
        }

        return tsqlMixinType;
    }

    protected void addWithClause(UnitOfWork transaction, KomodoObject node) throws Exception {
        KomodoObject[] withKomodoObjects = references(transaction, node, QueryCommand.WITH_REF_NAME);
        if (withKomodoObjects.length == 0)
            return;

        appendToken(QueryCommand.WITH_REF_NAME);
        append(SPACE);
        iterate(transaction, node, QueryCommand.WITH_REF_NAME);
        beginClause(0);
    }

    protected boolean hasHint(UnitOfWork transaction, KomodoObject node) throws Exception {
        boolean optional = propertyBoolean(transaction, node, FromClause.OPTIONAL_PROP_NAME);
        boolean makeInd = propertyBoolean(transaction, node, FromClause.MAKE_IND_PROP_NAME);
        boolean makeNotDep = propertyBoolean(transaction, node, FromClause.MAKE_NOT_DEP_PROP_NAME);
        boolean noUnnest = propertyBoolean(transaction, node, FromClause.NO_UNNEST_PROP_NAME);
        boolean preserve = propertyBoolean(transaction, node, FromClause.PRESERVE_PROP_NAME);
        KomodoObject makeDep = reference(transaction, node, FromClause.MAKE_DEPENDENCY_REF_NAME);

        return optional || makeInd || makeNotDep || noUnnest || preserve || (makeDep != null);
    }

    protected boolean isMakeDepSimple(UnitOfWork transaction, KomodoObject makeDep) throws Exception {
        if (makeDep == null)
            return false;

        boolean hasMax = makeDep.hasProperty(transaction, MakeDep.MAX_PROP_NAME);
        boolean join = propertyBoolean(transaction, makeDep, MakeDep.JOIN_PROP_NAME);
  
        return !hasMax && !join;
    }
    
    protected void addHintComment(UnitOfWork transaction, KomodoObject node) throws Exception {

        boolean optional = propertyBoolean(transaction, node, FromClause.OPTIONAL_PROP_NAME);
        boolean makeInd = propertyBoolean(transaction, node, FromClause.MAKE_IND_PROP_NAME);
        boolean makeNotDep = propertyBoolean(transaction, node, FromClause.MAKE_NOT_DEP_PROP_NAME);
        boolean noUnnest = propertyBoolean(transaction, node, FromClause.NO_UNNEST_PROP_NAME);
        boolean preserve = propertyBoolean(transaction, node, FromClause.PRESERVE_PROP_NAME);
        KomodoObject makeDep = reference(transaction, node, FromClause.MAKE_DEPENDENCY_REF_NAME);

        if (hasHint(transaction, node)) {
            append(BEGIN_HINT);
            append(SPACE);

            if (optional) {
                append(OPTIONAL);
                append(SPACE);
            }

            if (makeDep != null && isMakeDepSimple(transaction, makeDep)) {
                append(MAKEDEP);
                append(SPACE);
            }

            if (makeNotDep) {
                append(MAKENOTDEP);
                append(SPACE);
            }

            if (makeInd) {
                append(MAKEIND);
                append(SPACE);
            }

            if (noUnnest) {
                append(NOUNNEST);
                append(SPACE);
            }

            if (preserve) {
                append(PRESERVE);
                append(SPACE);
            }

            append(END_HINT);
            append(SPACE);
        }
    }

    protected void addMakeDep(UnitOfWork transaction, KomodoObject node) throws Exception {
        KomodoObject makeDep = reference(transaction, node, FromClause.MAKE_DEPENDENCY_REF_NAME);

        if (makeDep != null && !isMakeDepSimple(transaction, makeDep)) {
            append(SPACE);
            append(MAKEDEP);
            visit(transaction, makeDep);
        }
    }

    /**
     * Take a string literal and escape it as necessary. By default, this converts ' to ''.
     * 
     * @param str String literal value (unquoted), never null
     * @return Escaped string literal value
     */
    protected String escapeStringValue(String str, String tick) {
        return StringUtils.replaceAll(str, tick, tick + tick);
    }

    protected String escapeSinglePart(String token) {
        if (TeiidSqlConstants.isReservedWord(token)) {
            return ID_ESCAPE_CHAR + token + ID_ESCAPE_CHAR;
        }

        boolean escape = true;
        char start = token.charAt(0);
        if (start == '#' || start == '@' || StringUtils.isLetter(start)) {
            escape = false;
            for (int i = 1; !escape && i < token.length(); i++) {
                char c = token.charAt(i);
                escape = !StringUtils.isLetterOrDigit(c) && c != '_';
            }
        }

        if (escape) {
            return ID_ESCAPE_CHAR + escapeStringValue(token, SPEECH_MARK) + ID_ESCAPE_CHAR;
        }

        return token;
    }

    protected void appendDisplayName(String name) {
        String[] pathParts = name.split("\\."); //$NON-NLS-1$
        for (int i = 0; i < pathParts.length; i++) {
            if (i > 0) {
                append(Symbol.SEPARATOR);
            }

            append(escapeSinglePart(pathParts[i]));
        }
    }

    protected String shortName(String name) {
        int index = name.lastIndexOf(Symbol.SEPARATOR);
        if(index >= 0) {
            return name.substring(index + 1);
        }

        return name;
    }

    protected String outputName(UnitOfWork transaction, KomodoObject node) throws Exception {
        String name = propertyString(transaction, node,  Symbol.NAME_PROP_NAME);
        String outputName = propertyString(transaction, node,  Symbol.OUTPUT_NAME_PROP_NAME);
        return outputName == null ? name : outputName;
    }

    protected void appendNested(UnitOfWork transaction, KomodoObject node) throws Exception {
        boolean useParens = instanceOf(transaction, node, LexTokens.CRITERIA);
        if (useParens) {
            append(OPEN_BRACKET);
        }

        visit(transaction, node);

        if (useParens) {
            append(CLOSE_BRACKET);
        }
    }

    protected void appendLiteral(Class<?> type, boolean multiValued, Object value) throws Exception {
        Class<?> booleanClass = getDataTypeService().getDefaultDataClass(DataTypeName.BOOLEAN);

        String[] constantParts = null;
        if (multiValued) {
            constantParts = new String[] {QUESTION_MARK}; 
        } else if (value == null) {
            
            if (booleanClass.equals(type)) {
                constantParts = new String[] {UNKNOWN};
            } else {
                constantParts = new String[] {NULL.toLowerCase()};
            }
        } else {

            if (value instanceof java.sql.Array) {
                java.sql.Array av = (java.sql.Array) value; 
                append(OPEN_BRACKET);

                try {
                    Object[] values = (Object[])av.getArray();
                    for (int i = 0; i < values.length; i++) {
                        if (i > 0) {
                            append(COMMA);
                            append(SPACE);
                        }

                        Object value2 = values[i];
                        appendLiteral(value2 != null ? value2.getClass() : values.getClass().getComponentType(), multiValued, value2);
                    }

                } catch (Exception ex) {
                    logger.error(ex.getMessage(), ex);
                    append(ERROR);
                }

                append(CLOSE_BRACKET);
                return;
            }

            if (Number.class.isAssignableFrom(type)) {
                constantParts = new String[] {value.toString()};
            } else if (booleanClass.equals(type)) {
                constantParts = new String[] {value.equals(Boolean.TRUE) ? TRUE : FALSE};
            } else if (type.equals(getDataTypeService().getDefaultDataClass(DataTypeName.TIMESTAMP))) {
                constantParts = new String[] {OPEN_BRACE + "ts'", value.toString(), QUOTE_MARK + CLOSE_BRACE};  //$NON-NLS-1$
            } else if (type.equals(getDataTypeService().getDefaultDataClass(DataTypeName.TIME))) {
                constantParts = new String[] {OPEN_BRACE + "t'", value.toString(), QUOTE_MARK + CLOSE_BRACE};  //$NON-NLS-1$
            } else if (type.equals(getDataTypeService().getDefaultDataClass(DataTypeName.DATE))) {
                constantParts = new String[] {OPEN_BRACE + "d'", value.toString(), QUOTE_MARK + CLOSE_BRACE};  //$NON-NLS-1$
            } else if (type.equals(getDataTypeService().getDefaultDataClass(DataTypeName.VARBINARY))) {
                constantParts = new String[] {"X'", value.toString(), QUOTE_MARK};  //$NON-NLS-1$
            }

            if (constantParts == null) {
                if (getDataTypeService().isLOB(type)) {
                    constantParts = new String[] {QUESTION_MARK};
                } else {
                    String strValue = value.toString();
                    strValue = escapeStringValue(strValue, QUOTE_MARK);
                    constantParts = new String[] {QUOTE_MARK, strValue, QUOTE_MARK};
                }

            }
        }

        for (String string : constantParts) {
            append(string);
        }
    }

    protected void appendLabel(UnitOfWork transaction, KomodoObject node) throws Exception {
        String label = propertyString(transaction, node, Labeled.LABEL_PROP_NAME);
        if (label != null) {
            appendDisplayName(label);
            append(SPACE);
            append(COLON);
            append(SPACE);
        }
    }

    protected void appendStatements(UnitOfWork transaction, KomodoObject node, String refName) throws Exception {
        KomodoObject[] statements = references(transaction, node, refName);
        for(int i = 0; i < statements.length; ++i) {
            // Add each statement
            visit(transaction, statements[i]);
            append(NEW_LINE);
        }
    }

    protected void visit(KomodoObject node, TeiidSqlNodeContext context) throws Exception {
        if (node == null) {
            append(undefined());
            return;
        }

        Descriptor tsqlMixinType = findTeiidSqlType(context.tx(), node);
        if (tsqlMixinType == null) {
            visitChildren(context.tx(), node); // Not a tsql node but may contain them
            return;
        }

        LexTokens LexEnum = LexTokens.findClass(tsqlMixinType.getName());
        try {
            switch(LexEnum) {
                case CRITERIA:
                    criteria(context);
                    break;
                case COMPARE_CRITERIA:
                    compareCriteria(context);
                    break;
                case SUBQUERY_COMPARE_CRITERIA:
                    subqueryCompareCriteria(context);
                    break;
                case SET_CRITERIA:
                    setCriteria(context);
                    break;
                case SUBQUERY_SET_CRITERIA:
                    subquerySetCriteria(context);
                    break;
                case BETWEEN_CRITERIA:
                    betweenCriteria(context);
                    break;
                case COMPOUND_CRITERIA:
                    compoundCriteria(context);
                    break;
                case EXISTS_CRITERIA:
                    existsCriteria(context);
                    break;
                case EXPRESSION_CRITERIA:
                    expressionCriteria(context);
                    break;
                case IS_NULL_CRITERIA:
                    isNullCriteria(context);
                    break;
                case MATCH_CRITERIA:
                    matchCriteria(context);
                    break;
                case NOT_CRITERIA:
                    notCriteria(context);
                    break;
                case ALTER_PROCEDURE:
                    alterProcedure(context);
                    break;
                case ALTER_TRIGGER:
                    alterTrigger(context);
                    break;
                case ALTER_VIEW:
                    alterView(context);
                    break;
                case DELETE:
                    delete(context);
                    break;
                case INSERT:
                    insert(context);
                    break;
                case STORED_PROCEDURE:
                    storedProcedure(context);
                    break;
                case UPDATE:
                    update(context);
                    break;
                case DYNAMIC_COMMAND:
                    dynamicCommand(context);
                    break;
                case QUERY:
                    query(context);
                    break;
                case SET_QUERY:
                    setQuery(context);
                    break;
                case CREATE_PROCEDURE_COMMAND:
                    createProcedureCommand(context);
                    break;
                case TRIGGER_ACTION:
                    triggerAction(context);
                    break;
                case ARRAY_TABLE:
                    arrayTable(context);
                    break;
                case OBJECT_TABLE:
                    objectTable(context);
                    break;
                case TEXT_TABLE:
                    textTable(context);
                    break;
                case XML_TABLE:
                    xmlTable(context);
                    break;
                case JOIN_PREDICATE:
                    joinPredicate(context);
                    break;
                case SUBQUERY_FROM_CLAUSE:
                    subqueryFromClause(context);
                    break;
                case UNARY_FROM_CLAUSE:
                    unaryFromClause(context);
                    break;
                case FROM:
                    from(context);
                    break;
                case GROUP_BY:
                    groupBy(context);
                    break;
                case INTO:
                    into(context);
                    break;
                case JOIN_TYPE:
                    joinType(context);
                    break;
                case LIMIT:
                    limit(context);
                    break;
                case MAKE_DEP:
                    makeDep(context);
                    break;
                case NAMESPACE_ITEM:
                    namespaceItem(context);
                    break;
                case NULL_NODE:
                    nullNode(context);
                    break;
                case PROJECTED_COLUMN:
                    projectedColumn(context);
                    break;
                case OBJECT_COLUMN:
                    objectColumn(context);
                    break;
                case TEXT_COLUMN:
                    textColumn(context);
                    break;
                case XML_COLUMN:
                    xmlColumn(context);
                    break;
                case OPTION:
                    option(context);
                    break;
                case ORDER_BY:
                    orderBy(context);
                    break;
                case ORDER_BY_ITEM:
                    orderByItem(context);
                    break;
                case SP_PARAMETER:
                    spParameter(context);
                    break;
                case SELECT:
                    select(context);
                    break;
                case SET_CLAUSE:
                    setClause(context);
                    break;
                case SET_CLAUSE_LIST:
                    setClauseList(context);
                    break;
                case SOURCE_HINT:
                    sourceHint(context);
                    break;
                case SPECIFIC_HINT:
                    specificHint(context);
                    break;
                case SUBQUERY_HINT:
                    subqueryHint(context);
                    break;
                case WITH_QUERY_COMMAND:
                    withQueryCommand(context);
                    break;
                case ASSIGNMENT_STATEMENT:
                    assignmentStatement(context);
                    break;
                case DECLARE_STATEMENT:
                    declareStatement(context);
                    break;
                case RETURN_STATEMENT:
                    returnStatement(context);
                    break;
                case BLOCK:
                    block(context);
                    break;
                case BRANCHING_STATEMENT:
                    branchingStatement(context);
                    break;
                case COMMAND_STATEMENT:
                    commandStatement(context);
                    break;
                case IF_STATEMENT:
                    ifStatement(context);
                    break;
                case LOOP_STATEMENT:
                    loopStatement(context);
                    break;
                case RAISE_STATEMENT:
                    raiseStatement(context);
                    break;
                case WHILE_STATEMENT:
                    whileStatement(context);
                    break;
                case EXCEPTION_EXPRESSION:
                    exceptionExpression(context);
                    break;
                case FUNCTION:
                    function(context);
                    break;
                case AGGREGATE_SYMBOL:
                    aggregateSymbol(context);
                    break;
                case ALIAS_SYMBOL:
                    aliasSymbol(context);
                    break;
                case ELEMENT_SYMBOL:
                    elementSymbol(context);
                    break;
                case EXPRESSION_SYMBOL:
                    expressionSymbol(context);
                    break;
                case GROUP_SYMBOL:
                    groupSymbol(context);
                    break;
                case ARRAY_SYMBOL:
                    arraySymbol(context);
                    break;
                case CASE_EXPRESSION:
                    caseExpression(context);
                    break;
                case CONSTANT:
                    constant(context);
                    break;
                case DERIVED_COLUMN:
                    derivedColumn(context);
                    break;
                case JSON_OBJECT:
                    jsonObject(context);
                    break;
                case MULTIPLE_ELEMENT_SYMBOL:
                    multipleElementSymbol(context);
                    break;
                case QUERY_STRING:
                    queryString(context);
                    break;
                case REFERENCE:
                    reference(context);
                    break;
                case SCALAR_SUBQUERY:
                    scalarSubquery(context);
                    break;
                case SEARCHED_CASE_EXPRESSION:
                    searchedCaseExpression(context);
                    break;
                case TEXT_LINE:
                    textLine(context);
                    break;
                case WINDOW_FUNCTION:
                    windowFunction(context);
                    break;
                case WINDOW_SPECIFICATION:
                    windowSpecification(context);
                    break;
                case XML_ATTRIBUTES:
                    xmlAttributes(context);
                    break;
                case XML_ELEMENT:
                    xmlElement(context);
                    break;
                case XML_FOREST:
                    xmlForest(context);
                    break;
                case XML_NAMESPACES:
                    xmlNamespaces(context);
                    break;
                case XML_PARSE:
                    xmlParse(context);
                    break;
                case XML_QUERY:
                    xmlQuery(context);
                    break;
                case XML_SERIALIZE:
                    xmlSerialize(context);
                    break;
                case CACHE_HINT:
                    cacheHint(context);
                    break;
                default:
                    throw new UnsupportedOperationException();
            }
        } catch (Exception ex) {
            throw ex;
        }
    }

    @Override
    public Object visit(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        visit(kObject, new TeiidSqlNodeContext(transaction, kObject));
        return null;
    }

    public Object nullNode(TeiidSqlNodeContext context) throws Exception {
        append(undefined());

        return null;
    }

    public Object criteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        String keyword = (String) context.get(KEYWORD_KEY);
        if (keyword != null) {
            append(keyword);
            append(SPACE);
        }

        visit(context.tx(), node);

        return null;
    }

    public Object compareCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject leftExp = reference(context.tx(), node, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME);
        visit(context.tx(), leftExp);
        append(SPACE);

        String opString = propertyString(context.tx(), node, AbstractCompareCriteria.OPERATOR_PROP_NAME);
        CriteriaOperator.Operator operator = CriteriaOperator.Operator.findOperator(opString);
        append(operator.getSymbols().iterator().next());
        append(SPACE);

        KomodoObject rightExp = reference(context.tx(), node, CompareCriteria.RIGHT_EXPRESSION_REF_NAME);
        visit(context.tx(), rightExp);

        return null;
    }

    public Object subqueryCompareCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject leftExp = reference(context.tx(), node, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME);
        visit(context.tx(), leftExp);

        String opString = propertyString(context.tx(), node, AbstractCompareCriteria.OPERATOR_PROP_NAME);
        CriteriaOperator.Operator operator = CriteriaOperator.Operator.findOperator(opString);

        String quantString = propertyString(context.tx(), node, SubqueryCompareCriteria.PREDICATE_QUANTIFIER_PROP_NAME);
        PredicateQuantifier quantifier = PredicateQuantifier.findPredicateQuantifier(quantString);

        // operator and beginning of list
        append(SPACE);
        append(operator.getSymbols().iterator().next());
        append(SPACE);
        append(quantifier.name());
        append(SPACE);

        append(OPEN_BRACKET);
        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        visit(context.tx(), command);
        append(CLOSE_BRACKET);

        return null;
    }

    public Object setCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject expression = reference(context.tx(), node, AbstractSetCriteria.EXPRESSION_REF_NAME);
        appendNested(context.tx(), expression);

        // operator and beginning of list
        append(SPACE);

        boolean negated = propertyBoolean(context.tx(), node, AbstractSetCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(IN);
        append(SPACE + OPEN_BRACKET);

        // value list
        iterate(context.tx(), node, SetCriteria.VALUES_REF_NAME);
        append(CLOSE_BRACKET);

        return null;
    }

    public Object subquerySetCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject expression = reference(context.tx(), node, AbstractSetCriteria.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);

        // operator and beginning of list
        append(SPACE);

        boolean negated = propertyBoolean(context.tx(), node, AbstractSetCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(IN);

        KomodoObject subqueryHint = reference(context.tx(), node, SubquerySetCriteria.SUBQUERY_HINT_REF_NAME);
        visit(context.tx(), subqueryHint);

        append(SPACE + OPEN_BRACKET);
        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        visit(context.tx(), command);
        append(CLOSE_BRACKET);

        return null;
    }

    public Object betweenCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject expression = reference(context.tx(), node, BetweenCriteria.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);
        append(SPACE);

        boolean negated = propertyBoolean(context.tx(), node,  BetweenCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(BETWEEN);
        append(SPACE);

        KomodoObject lowerExpression = reference(context.tx(), node, BetweenCriteria.LOWER_EXPRESSION_REF_NAME);
        visit(context.tx(), lowerExpression);

        append(SPACE);
        append(AND);
        append(SPACE);

        KomodoObject upperExpression = reference(context.tx(), node, BetweenCriteria.UPPER_EXPRESSION_REF_NAME);
        visit(context.tx(), upperExpression);

        return null;
    }

    public Object compoundCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        // Get operator string
        long operator = propertyLong(context.tx(), node, CompoundCriteria.OPERATOR_PROP_NAME);
        String operatorStr = EMPTY_STRING;
        if (operator == LogicalOperator.AND.ordinal()) {
            operatorStr = AND;
        } else if (operator == LogicalOperator.OR.ordinal()) {
            operatorStr = OR;
        }

        // Get criteria

        KomodoObject[] criteria = references(context.tx(), node, CompoundCriteria.CRITERIA_REF_NAME);
        int size = size(context.tx(), node, CompoundCriteria.CRITERIA_REF_NAME);

        // Build parts
        if (size == 1) {
            // Special case - should really never happen, but we are tolerant
            visit(context.tx(), criteria[0]);
        } else {
            // Add first criteria

            for (int i = 0; i < criteria.length; ++i) {
                if (i > 0) {
                    // Add connector
                    append(SPACE);
                    append(operatorStr);
                    append(SPACE);
                }

                // Add criteria
                KomodoObject crit = criteria[i];
                append(OPEN_BRACKET);
                visit(context.tx(), crit);
                append(CLOSE_BRACKET);
            }
        }

        return null;
    }

    public Object existsCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean negated = propertyBoolean(context.tx(), node,  ExistsCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(EXISTS);

        KomodoObject subqueryHint = reference(context.tx(), node, ExistsCriteria.SUBQUERY_HINT_REF_NAME);
        visit(context.tx(), subqueryHint);

        append(SPACE + OPEN_BRACKET);
        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        visit(context.tx(), command);
        append(CLOSE_BRACKET);

        return null;
    }

    public Object expressionCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        
        KomodoObject expression = reference(context.tx(), node, ExpressionCriteria.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);

        return null;
    }

    public Object isNullCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject expression = reference(context.tx(), node, IsNullCriteria.EXPRESSION_REF_NAME);
        appendNested(context.tx(), expression);

        append(SPACE);
        append(IS);
        append(SPACE);

        boolean negated = propertyBoolean(context.tx(), node,  IsNullCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        append(NULL);

        return null;
    }

    public Object matchCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject leftExpression = reference(context.tx(), node, MatchCriteria.LEFT_EXPRESSION_REF_NAME);
        visit(context.tx(), leftExpression);

        append(SPACE);
        boolean negated = propertyBoolean(context.tx(), node,  MatchCriteria.NEGATED_PROP_NAME);
        if (negated) {
            append(NOT);
            append(SPACE);
        }

        String modeString = propertyString(context.tx(), node,  MatchCriteria.MODE_PROP_NAME);
        MatchMode mode = MatchMode.findMatchMode(modeString);
        switch (mode) {
            case SIMILAR:
                append(SIMILAR);
                append(SPACE);
                append(TO);
                break;
            case LIKE:
                append(LIKE);
                break;
            case REGEX:
                append(LIKE_REGEX);
                break;
        }

        append(SPACE);

        KomodoObject rightExpression = reference(context.tx(), node, MatchCriteria.RIGHT_EXPRESSION_REF_NAME);
        visit(context.tx(), rightExpression);

        String escapeChar = propertyString(context.tx(), node,  MatchCriteria.ESCAPE_CHAR_PROP_NAME);
        if (! Character.toString(TeiidSqlConstants.NULL_ESCAPE_CHAR).equals(escapeChar)) {
            append(SPACE);
            append(ESCAPE);
            append(SPACE);
            appendLiteral(String.class, false, escapeChar);
        }

        return null;
    }

    public Object notCriteria(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(NOT);
        append(SPACE + OPEN_BRACKET);

        KomodoObject criteria = reference(context.tx(), node, NotCriteria.CRITERIA_REF_NAME);
        visit(context.tx(), criteria);

        append(CLOSE_BRACKET);

        return null;
    }

    public Object alterProcedure(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(ALTER);
        append(SPACE);
        append(PROCEDURE);
        append(SPACE);

        KomodoObject target = reference(context.tx(), node, Alter.TARGET_REF_NAME);
        visit(context.tx(), target);

        beginClause(1);
        append(AS);

        KomodoObject definition = reference(context.tx(), node, Alter.DEFINITION_REF_NAME);
        if (instanceOf(context.tx(), definition, LexTokens.CREATE_PROCEDURE_COMMAND)) {
            KomodoObject defnBlock = reference(context.tx(), definition, CreateProcedureCommand.BLOCK_REF_NAME);
            visit(context.tx(), defnBlock);
        }

        return null;
    }

    public Object alterTrigger(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean create = propertyBoolean(context.tx(), node, AlterTrigger.CREATE_PROP_NAME);
        if (create) {
            append(CREATE);
        } else {
            append(ALTER);
        }

        append(SPACE);
        append(TRIGGER);
        append(SPACE);
        append(ON);
        append(SPACE);

        KomodoObject target = reference(context.tx(), node, Alter.TARGET_REF_NAME);
        visit(context.tx(), target);

        beginClause(0);
        append(INSTEAD);
        append(SPACE);
        append(OF);
        append(SPACE);

        String eventName = propertyString(context.tx(), node, AlterTrigger.EVENT_PROP_NAME);
        TriggerEvent triggerEvent = TriggerEvent.findTriggerEvent(eventName);
        append(triggerEvent.name());

        KomodoObject definition = reference(context.tx(), node, Alter.DEFINITION_REF_NAME);
        if (definition != null) {
            beginClause(0);
            append(AS);
            append(NEW_LINE);
            visit(context.tx(), definition);
        } else {
            append(SPACE);

            boolean enabled = propertyBoolean(context.tx(), node, AlterTrigger.ENABLED_PROP_NAME);
            append(enabled ? ENABLED : DISABLED);
        }

        return null;
    }

    public Object alterView(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(ALTER);
        append(SPACE);
        append(VIEW);
        append(SPACE);

        KomodoObject target = reference(context.tx(), node, Alter.TARGET_REF_NAME);
        visit(context.tx(), target);

        beginClause(0);
        append(AS);
        append(NEW_LINE);

        KomodoObject definition = reference(context.tx(), node, Alter.DEFINITION_REF_NAME);
        visit(context.tx(), definition);

        return null;
    }

    public Object delete(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(DELETE);

        KomodoObject sourceHint = reference(context.tx(), node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(context.tx(), sourceHint);

        append(SPACE);

        // add from clause
        append(FROM);
        append(SPACE);

        KomodoObject group = reference(context.tx(), node, TargetedCommand.GROUP_REF_NAME);
        visit(context.tx(), group);

        // add where clause
        KomodoObject criteria = reference(context.tx(), node, Delete.CRITERIA_REF_NAME);
        if (criteria != null) {
            beginClause(0);

            TeiidSqlNodeContext ccontext = new TeiidSqlNodeContext(context.tx(), criteria);
            ccontext.add(KEYWORD_KEY, WHERE);
            criteria(ccontext);
        }

        // Option clause
        KomodoObject option = reference(context.tx(), node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(0);
            visit(context.tx(), option);
        }

        return null;
    }

    public Object insert(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean merge = propertyBoolean(context.tx(), node, Insert.MERGE_PROP_NAME);
        if (merge) {
            append(MERGE);
        } else {
            append(INSERT);
        }

        KomodoObject sourceHint = reference(context.tx(), node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(context.tx(), sourceHint);

        append(SPACE);
        append(INTO);
        append(SPACE);

        KomodoObject group = reference(context.tx(), node, TargetedCommand.GROUP_REF_NAME);
        visit(context.tx(), group);

        KomodoObject[] variables = references(context.tx(), node, Insert.VARIABLES_REF_NAME);
        int size = size(context.tx(), node, Insert.VARIABLES_REF_NAME);
        if (size > 0) {
            beginClause(2);

            // Columns clause
            append(OPEN_BRACKET);

            for (int i = 0; i < variables.length; i++) {
                KomodoObject variable = variables[i];
                if (i > 0) {
                    append(COMMA + SPACE);
                }

                TeiidSqlNodeContext varContext = new TeiidSqlNodeContext(context.tx(), variable);
                varContext.add(SHORT_NAME_ONLY_KEY, true);
                visit(variable, varContext);
            }

            append(CLOSE_BRACKET);
        }

        beginClause(1);

        KomodoObject queryExp = reference(context.tx(), node, Insert.QUERY_EXPRESSION_REF_NAME);
        KomodoObject[] values = references(context.tx(), node, Insert.VALUES_REF_NAME);

        if (queryExp != null) {
            visit(context.tx(), queryExp);
        } else if (values.length > 0) {
            append(VALUES);
            beginClause(2);
            append(OPEN_BRACKET);
            iterate(context.tx(), node, Insert.VALUES_REF_NAME);
            append(CLOSE_BRACKET);
        }

        // Option clause
        KomodoObject option = reference(context.tx(), node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(context.tx(), option);
        }

        return null;
    }

    public Object storedProcedure(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean calledWithReturn = propertyBoolean(context.tx(), node, StoredProcedure.CALLED_WITH_RETURN_PROP_NAME);
        boolean displayNamedParams = propertyBoolean(context.tx(), node, StoredProcedure.DISPLAY_NAMED_PARAMETERS_PROP_NAME);

        if (calledWithReturn) {
            KomodoObject[] parameters = references(context.tx(), node, StoredProcedure.PARAMETERS_REF_NAME);

            for (int i = 0; i < parameters.length; ++i) {
                KomodoObject parameter = parameters[i];
                long paramType = propertyLong(context.tx(), parameter, SPParameter.PARAMETER_TYPE_PROP_NAME);
                if (paramType == ParameterInfo.RETURN_VALUE.index()) {
                    KomodoObject expression = reference(context.tx(), parameter, SPParameter.EXPRESSION_REF_NAME);
                    if (expression == null) {
                        append(QUESTION_MARK);
                    } else {
                        visit(context.tx(), expression);
                    }

                }

            }

            append(SPACE);
            append(EQ);
            append(SPACE);
        }

        // exec clause
        append(EXEC);
        append(SPACE);

        String procedureName = propertyString(context.tx(), node, StoredProcedure.PROCEDURE_NAME_PROP_NAME);
        append(procedureName);

        append(OPEN_BRACKET);

        boolean first = true;
        KomodoObject[] parameters = references(context.tx(), node, StoredProcedure.PARAMETERS_REF_NAME);
        for (int i = 0; i < parameters.length; ++i) {
            KomodoObject parameter = parameters[i];
            
            boolean usingDefault = propertyBoolean(context.tx(), parameter, SPParameter.USING_DEFAULT_PROP_NAME);
            long paramType = propertyLong(context.tx(), parameter, SPParameter.PARAMETER_TYPE_PROP_NAME);
            KomodoObject expression = reference(context.tx(), parameter, SPParameter.EXPRESSION_REF_NAME);

            if (usingDefault)
                continue;
            if (paramType == ParameterInfo.RETURN_VALUE.index() || 
                    paramType == ParameterInfo.RESULT_SET.index())
                continue;
            
            if(expression == null)
                continue;

            if (first)
                first = false;
            else
                append(COMMA + SPACE);

            if (displayNamedParams) {
                String paramName = propertyString(context.tx(), parameter, SPParameter.NAME_PROP_NAME);
                append(escapeSinglePart(shortName(paramName)));
                append(SPACE + EQUALS + CLOSE_ANGLE_BRACKET + SPACE);
            }

            boolean addParens = displayNamedParams && instanceOf(context.tx(), expression, LexTokens.COMPARE_CRITERIA);
            if (addParens) {
                append(OPEN_BRACKET);
            }

            visit(context.tx(), expression);

            if (addParens) {
                append(CLOSE_BRACKET);
            }
        }

        append(CLOSE_BRACKET);

        // Option clause
        KomodoObject option = reference(context.tx(), node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(context.tx(), option);
        }

        return null;
    }

    public Object update(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(UPDATE);

        KomodoObject sourceHint = reference(context.tx(), node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(context.tx(), sourceHint);

        append(SPACE);
        
        KomodoObject group = reference(context.tx(), node, TargetedCommand.GROUP_REF_NAME);
        visit(context.tx(), group);

        beginClause(1);

        // Set clause
        append(SET);
        beginClause(2);

        KomodoObject changeList = reference(context.tx(), node, Update.CHANGE_LIST_REF_NAME);
        visit(context.tx(), changeList);

        // add where clause
        KomodoObject criteria = reference(context.tx(), node, Update.CRITERIA_REF_NAME);
        if (criteria != null) {
            beginClause(0);

            TeiidSqlNodeContext ccontext = new TeiidSqlNodeContext(context.tx(), criteria);
            ccontext.add(KEYWORD_KEY, WHERE);
            criteria(ccontext);
        }

        // Option clause
        KomodoObject option = reference(context.tx(), node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(context.tx(), option);
        }

        return null;
    }

    public Object dynamicCommand(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(EXECUTE);
        append(SPACE);
        append(IMMEDIATE);
        append(SPACE);

        KomodoObject sql = reference(context.tx(), node, DynamicCommand.SQL_REF_NAME);
        visit(context.tx(), sql);

        boolean asClauseSet = propertyBoolean(context.tx(), node, DynamicCommand.AS_CLAUSE_SET_PROP_NAME);
        if (asClauseSet) {
            beginClause(1);
            append(AS);
            append(SPACE);

            KomodoObject[] asColumns = references(context.tx(), node, DynamicCommand.AS_COLUMNS_REF_NAME);
            for (int i = 0; i < asColumns.length; ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                KomodoObject asColumn = asColumns[i];
                String asColName = propertyString(context.tx(), asColumn, Symbol.NAME_PROP_NAME);

                append(shortName(asColName));
                append(SPACE);

                String typeName = propertyString(context.tx(), asColumn, Expression.TYPE_CLASS_PROP_NAME);
                DataTypeName dataTypeName = DataTypeName.findDataTypeName(typeName);
                Class<?> dataTypeClass = getDataTypeService().getDefaultDataClass(dataTypeName);
                append(getDataTypeService().getDataTypeName(dataTypeClass));
            }
        }

        KomodoObject intoGroup = reference(context.tx(), node, DynamicCommand.INTO_GROUP_REF_NAME);
        if (intoGroup != null) {
            beginClause(1);
            append(INTO);
            append(SPACE);
            visit(context.tx(), intoGroup);
        }

        KomodoObject using = reference(context.tx(), node, DynamicCommand.USING_REF_NAME);
        KomodoObject[] usingList = references(context.tx(), using, SetClauseList.SET_CLAUSES_REF_NAME);
        if (using != null && usingList.length > 0) {
            beginClause(1);
            append(USING);
            append(SPACE);
            visit(context.tx(), using);
        }

        long updatingModelCount = propertyLong(context.tx(), node, DynamicCommand.UPDATING_MODEL_COUNT_PROP_NAME);
        if (updatingModelCount > 0) {
            beginClause(1);
            append(UPDATE);
            append(SPACE);

            if (updatingModelCount > 1) {
                append(STAR);
            } else {
                append(Integer.toString(1));
            }
        }

        return null;
    }

    public Object query(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        addWithClause(context.tx(), node);
        appendToken(Select.ID);

        KomodoObject sourceHint = reference(context.tx(), node, Command.SOURCE_HINT_REF_NAME);
        if (sourceHint != null)
            visit(context.tx(), sourceHint);

        KomodoObject select = reference(context.tx(), node, Query.SELECT_REF_NAME);
        if (select != null)
            visit(context.tx(), select);

        KomodoObject into = reference(context.tx(), node, Query.INTO_REF_NAME);
        if (into != null) {
            beginClause(1);
            visit(context.tx(), into);
        }

        KomodoObject from = reference(context.tx(), node, Query.FROM_REF_NAME);
        if (from != null) {
            beginClause(1);
            visit(context.tx(), from);
        }

        // Where clause
        KomodoObject criteria = reference(context.tx(), node, Query.CRITERIA_REF_NAME);
        if (criteria != null) {
            beginClause(1);

            TeiidSqlNodeContext ccontext = new TeiidSqlNodeContext(context.tx(), criteria);
            ccontext.add(KEYWORD_KEY, WHERE);
            criteria(ccontext);
        }

        // Group by clause
        KomodoObject groupBy = reference(context.tx(), node, Query.GROUP_BY_REF_NAME);
        if (groupBy != null) {
            beginClause(1);
            visit(context.tx(), groupBy);
        }

        // Having clause
        KomodoObject having = reference(context.tx(), node, Query.HAVING_REF_NAME);
        if (having != null) {
            beginClause(1);
            TeiidSqlNodeContext hcontext = new TeiidSqlNodeContext(context.tx(), having);
            hcontext.add(KEYWORD_KEY, HAVING);
            criteria(hcontext);
        }

        // Order by clause
        KomodoObject orderBy = reference(context.tx(), node, QueryCommand.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            beginClause(1);
            visit(context.tx(), orderBy);
        }

        KomodoObject limit = reference(context.tx(), node, QueryCommand.LIMIT_REF_NAME);
        if (limit != null) {
            beginClause(1);
            visit(context.tx(), limit);
        }

        // Option clause
        KomodoObject option = reference(context.tx(), node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(context.tx(), option);
        }

        return null;
    }

    protected void appendSetQuery(UnitOfWork transaction, KomodoObject parent, KomodoObject queryCommand, boolean right) throws Exception {
        
        KomodoObject limit = reference(transaction, queryCommand, QueryCommand.LIMIT_REF_NAME);
        KomodoObject orderBy = reference(transaction, queryCommand, QueryCommand.ORDER_BY_REF_NAME);

        boolean isSetQuery = instanceOf(transaction, queryCommand, LexTokens.SET_QUERY);
        
        boolean parentIsAll = propertyBoolean(transaction, parent, SetQuery.ALL_PROP_NAME);
        boolean cmdIsAll = propertyBoolean(transaction, queryCommand, SetQuery.ALL_PROP_NAME);

        String parentOpName = propertyString(transaction, parent, SetQuery.OPERATION_PROP_NAME);
        Operation parentOp = Operation.findOperation(parentOpName);

        String cmdOpName = propertyString(transaction, queryCommand, SetQuery.OPERATION_PROP_NAME);
        Operation cmdOp = Operation.findOperation(cmdOpName);

        if (limit != null || orderBy != null ||
            (right && 
                ((isSetQuery && 
                    (parentIsAll && !(cmdIsAll) || parentOp.equals(cmdOp) ))))) {
            append(OPEN_BRACKET);
            visit(transaction, queryCommand);
            append(CLOSE_BRACKET);
        } else {
            visit(transaction, queryCommand);
        }
    }

    public Object setQuery(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addWithClause(context.tx(), node);

        KomodoObject queryCommand = reference(context.tx(), node, SetQuery.LEFT_QUERY_REF_NAME);
        appendSetQuery(context.tx(), node, queryCommand, false);

        beginClause(0);

        String opName = propertyString(context.tx(), node, SetQuery.OPERATION_PROP_NAME);
        Operation op = Operation.findOperation(opName);
        append(op.name());

        boolean isAll = propertyBoolean(context.tx(), node, SetQuery.ALL_PROP_NAME);
        if (isAll) {
            append(SPACE);
            append(ALL);
        }

        beginClause(0);

        KomodoObject rightQuery = reference(context.tx(), node, SetQuery.RIGHT_QUERY_REF_NAME);
        appendSetQuery(context.tx(), node, rightQuery, true);

        // Order by clause
        KomodoObject orderBy = reference(context.tx(), node, QueryCommand.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            beginClause(1);
            visit(context.tx(), orderBy);
        }

        KomodoObject limit = reference(context.tx(), node, QueryCommand.LIMIT_REF_NAME);
        if (limit != null) {
            beginClause(1);
            visit(context.tx(), limit);
        }

        // Option clause
        KomodoObject option = reference(context.tx(), node, Command.OPTION_REF_NAME);
        if (option != null) {
            beginClause(1);
            visit(context.tx(), option);
        }

        return null;
    }

    public Object createProcedureCommand(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        KomodoObject block = reference(context.tx(), node, CreateProcedureCommand.BLOCK_REF_NAME);
        visit(context.tx(), block);

        return null;
    }

    public Object triggerAction(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(FOR);
        append(SPACE);
        append(EACH);
        append(SPACE);
        append(ROW);
        append(NEW_LINE);

        KomodoObject block = reference(context.tx(), node, TriggerAction.BLOCK_REF_NAME);
        visit(context.tx(), block);

        return null;
    }

    public Object arrayTable(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addHintComment(context.tx(), node);

        append(ARRAYTABLE);
        append(OPEN_BRACKET);

        KomodoObject arrayValue = reference(context.tx(), node, ArrayTable.ARRAY_VALUE_REF_NAME);
        visit(context.tx(), arrayValue);

        append(SPACE);
        append(COLUMNS);

        iterate(context.tx(), node, ArrayTable.COLUMNS_REF_NAME);

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(context.tx(), node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(context.tx(), node);

        return null;
    }

    public Object objectTable(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addHintComment(context.tx(), node);

        append(OBJECTTABLE);
        append(OPEN_BRACKET);

        String scriptLanguage = propertyString(context.tx(), node, ObjectTable.SCRIPTING_LANGUAGE_PROP_NAME);
        if (scriptLanguage != null) {
            append(LANGUAGE);
            append(SPACE);
            append(scriptLanguage);
            append(SPACE);
        }

        String rowScript = propertyString(context.tx(), node, ObjectTable.ROW_SCRIPT_PROP_NAME);
        appendLiteral(String.class, false, rowScript);

        KomodoObject[] passings = references(context.tx(), node, ObjectTable.PASSING_REF_NAME);
        if (passings.length > 0) {
            append(SPACE);
            append(PASSING);
            append(SPACE);
            iterate(context.tx(), node, ObjectTable.PASSING_REF_NAME);
        }

        append(SPACE);
        append(COLUMNS);

        KomodoObject[] columns = references(context.tx(), node, ObjectTable.COLUMNS_REF_NAME);
        for (int i = 0; i < columns.length; ++i) {
            if (i > 0)
                append(COMMA + SPACE);

            visit(context.tx(), columns[i]);
        }

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(context.tx(), node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(context.tx(), node);

        return null;
    }

    public Object textTable(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addHintComment(context.tx(), node);

        append(TEXTTABLE);
        append(OPEN_BRACKET);

        KomodoObject fileExp = reference(context.tx(), node, TextTable.FILE_REF_NAME);
        visit(context.tx(), fileExp);

        String selector = propertyString(context.tx(), node, TextTable.SELECTOR_PROP_NAME);
        if (selector != null) {
            append(SPACE);
            append(SELECTOR);
            append(SPACE);
            append(escapeSinglePart(selector));
        }

        append(SPACE);
        append(COLUMNS);

        KomodoObject[] columns = references(context.tx(), node, TextTable.COLUMNS_REF_NAME);
        for (int i = 0; i < columns.length; ++i) {
            if (i > 0)
                append(COMMA);

            visit(context.tx(), columns[i]);
        }

        boolean usingRowDelimiter = propertyBoolean(context.tx(), node, TextTable.USING_ROW_DELIMITER_PROP_NAME);
        if (!usingRowDelimiter) {
            append(SPACE);
            append(NO);
            append(SPACE);
            append(ROW);
            append(SPACE);
            append(DELIMITER);
        }

        String delimiter = propertyString(context.tx(), node, TextTable.DELIMITER_PROP_NAME);
        if (delimiter != null) {
            append(SPACE);
            append(DELIMITER);
            append(SPACE);
            appendLiteral(String.class, false, delimiter);
        }

        String quote = propertyString(context.tx(), node, TextTable.QUOTE_PROP_NAME);
        if (quote != null) {
            append(SPACE);

            boolean escape = propertyBoolean(context.tx(), node, TextTable.ESCAPE_PROP_NAME);
            if (escape) {
                append(ESCAPE);
            } else {
                append(QUOTE);
            }

            append(SPACE);
            appendLiteral(String.class, false, quote);
        }

        if (node.hasProperty(context.tx(), TextTable.HEADER_PROP_NAME)) {
            append(SPACE);
            append(HEADER);
            long header = propertyLong(context.tx(), node, TextTable.HEADER_PROP_NAME);
            if (1 != header) {
                append(SPACE);
                append(Long.toString(header));
            }
        }

        if (node.hasProperty(context.tx(), TextTable.SKIP_PROP_NAME)) {
            append(SPACE);
            append(SKIP);
            append(SPACE);

            long skip = propertyLong(context.tx(), node, TextTable.SKIP_PROP_NAME);
            append(Long.toString(skip));
        }

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(context.tx(), node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(context.tx(), node);

        return null;
    }

    public Object xmlTable(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addHintComment(context.tx(), node);

        append(XMLTABLE);
        append(OPEN_BRACKET);

        KomodoObject namespaces = reference(context.tx(), node, XMLTable.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            visit(context.tx(), namespaces);
            append(COMMA);
            append(SPACE);
        }

        String xquery = propertyString(context.tx(), node, XMLTable.XQUERY_PROP_NAME);
        appendLiteral(String.class, false, xquery);

        KomodoObject[] passings = references(context.tx(), node, XMLTable.PASSING_REF_NAME);
        if (passings.length > 0) {
            append(SPACE);
            append(PASSING);
            append(SPACE);
            iterate(context.tx(), node, XMLTable.PASSING_REF_NAME);
        }

        KomodoObject[] columns = references(context.tx(), node, XMLTable.COLUMNS_REF_NAME);
        boolean usingDefColumn = propertyBoolean(context.tx(), node, XMLTable.USING_DEFAULT_COLUMN_PROP_NAME);

        if (columns.length > 0 && !usingDefColumn) {
            append(SPACE);
            append(COLUMNS);

            for (int i = 0; i < columns.length; ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                visit(context.tx(), columns[i]);
            }
        }

        append(CLOSE_BRACKET);
        append(SPACE);
        append(AS);
        append(SPACE);

        String name = propertyString(context.tx(), node, TableFunctionReference.NAME_PROP_NAME);
        appendDisplayName(name);
        addMakeDep(context.tx(), node);

        return null;
    }

    public Object joinPredicate(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addHintComment(context.tx(), node);
        boolean hasHint = hasHint(context.tx(), node);

        if (hasHint) {
            append(OPEN_BRACKET);
        }

        // left clause
        KomodoObject leftClause = reference(context.tx(), node, JoinPredicate.LEFT_CLAUSE_REF_NAME);
        if (instanceOf(context.tx(), leftClause, LexTokens.JOIN_PREDICATE) && !hasHint(context.tx(), leftClause)) {
            append(OPEN_BRACKET);
            visit(context.tx(), leftClause);
            append(CLOSE_BRACKET);
        } else
            visit(context.tx(), leftClause);

        // join type
        append(SPACE);

        KomodoObject joinType = reference(context.tx(), node, JoinPredicate.JOIN_TYPE_REF_NAME);
        visit(context.tx(), joinType);
        append(SPACE);

        // right clause
        KomodoObject rightClause = reference(context.tx(), node, JoinPredicate.RIGHT_CLAUSE_REF_NAME);
        if (instanceOf(context.tx(), rightClause, LexTokens.JOIN_PREDICATE)
                && !hasHint(context.tx(), rightClause)) {
            append(OPEN_BRACKET);
            visit(context.tx(), rightClause);
            append(CLOSE_BRACKET);
        } else
            visit(context.tx(), rightClause);

        // join criteria
        KomodoObject[] criterions = references(context.tx(), node, JoinPredicate.JOIN_CRITERIA_REF_NAME);
        int size = size(context.tx(), node, JoinPredicate.JOIN_CRITERIA_REF_NAME);
        System.out.println(size);
        if (criterions.length > 0) {
            append(SPACE);
            append(ON);
            append(SPACE);
            for (int i = 0; i < criterions.length; ++i) {
                if (i > 0) {
                    append(SPACE);
                    append(AND);
                    append(SPACE);
                }
                    
                KomodoObject criterion = criterions[i];
                if (instanceOf(context.tx(), criterion, LexTokens.PREDICATE_CRITERIA) ||
                    instanceOf(context.tx(), criterion, LexTokens.NOT_CRITERIA)) {
                    visit(context.tx(), criterion);
                } else {
                    append(OPEN_BRACKET);
                    visit(context.tx(), criterion);
                    append(CLOSE_BRACKET);
                }

            }
        }

        if (hasHint) {
            append(CLOSE_BRACKET);
        }

        addMakeDep(context.tx(), node);

        return null;
    }

    public Object subqueryFromClause(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addHintComment(context.tx(), node);

        boolean table = propertyBoolean(context.tx(), node, SubqueryFromClause.TABLE_PROP_NAME);
        if (table) {
            append(TABLE);
        }

        append(OPEN_BRACKET);

        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        visit(context.tx(), command);

        append(CLOSE_BRACKET);
        append(SPACE + AS+ SPACE);

        String name = propertyString(context.tx(), node, SubqueryFromClause.NAME_PROP_NAME);
        append(escapeSinglePart(name));

        addMakeDep(context.tx(), node);

        return null;
    }

    public Object unaryFromClause(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        addHintComment(context.tx(), node);

        KomodoObject group = reference(context.tx(), node, UnaryFromClause.GROUP_REF_NAME);
        visit(context.tx(), group);
        addMakeDep(context.tx(), node);
        return null;
    }

    public Object from(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        appendToken(context.tx(), node);
        beginClause(1);
        
        iterate(context.tx(), node, From.CLAUSES_REF_NAME);

        return null;
    }

    public Object groupBy(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(GROUP);
        append(SPACE);
        append(BY);
        append(SPACE);

        boolean rollup = propertyBoolean(context.tx(), node, GroupBy.ROLLUP_PROP_NAME);
        if (rollup) {
            append(ROLLUP);
            append(OPEN_BRACKET);
        }

        iterate(context.tx(), node, GroupBy.SYMBOLS_REF_NAME);

        if (rollup) {
            append(CLOSE_BRACKET);
        }

        return null;
    }

    public Object into(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        appendToken(context.tx(), node);
        append(SPACE);
        KomodoObject group = reference(context.tx(), node, Into.GROUP_REF_NAME);
        visit(context.tx(), group);

        return null;
    }

    public Object joinType(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String kindName = propertyString(context.tx(), node, JoinType.KIND_PROP_NAME);
        JoinTypeTypes kind = JoinTypeTypes.findType(kindName);

        append(kind.toPrintStatement());

        return null;
    }

    public Object limit(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean strict = propertyBoolean(context.tx(), node, Limit.STRICT_PROP_NAME);
        if (!strict) {
            append(BEGIN_HINT);
            append(SPACE);
            append(Limit.NON_STRICT);
            append(SPACE);
            append(END_HINT);
            append(SPACE);
        }

        KomodoObject offset = reference(context.tx(), node, Limit.OFFSET_REF_NAME);
        KomodoObject rowLimit = reference(context.tx(), node, Limit.ROW_LIMIT_REF_NAME);
        if (rowLimit == null) {
            append(OFFSET);
            append(SPACE);            
            visit(context.tx(), offset);
            append(SPACE);
            append(ROWS);
        } else {

            append(LIMIT);
            if (offset != null) {
                append(SPACE);
                visit(context.tx(), offset);
                append(COMMA);
            }

            append(SPACE);
            visit(context.tx(), rowLimit);
        }

        return null;
    }

    public Object namespaceItem(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String prefix = propertyString(context.tx(), node, NamespaceItem.PREFIX_PROP_NAME);
        String uri = propertyString(context.tx(), node, NamespaceItem.URI_PROP_NAME);

        if (prefix == null) {
            if (uri == null) {
                append(XMLNamespaces.NO_DEFAULT);
            } else {
                append(DEFAULT + SPACE);
                appendLiteral(String.class, false, uri);
            }
        } else {
            appendLiteral(String.class, false, uri);
            append(SPACE + AS + SPACE);
            appendLiteral(String.class, false, prefix);
        }

        return null;
    }

    public Object projectedColumn(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String name = propertyString(context.tx(), node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(context.tx(), node, ProjectedColumn.TYPE_PROP_NAME);
        
        append(SPACE);
        appendDisplayName(name);
        append(SPACE);
        append(type);

        return null;
    }

    public Object objectColumn(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String name = propertyString(context.tx(), node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(context.tx(), node, ProjectedColumn.TYPE_PROP_NAME);
        String path = propertyString(context.tx(), node, ObjectColumn.PATH_PROP_NAME);

        append(SPACE);
        appendDisplayName(name);
        append(SPACE);
        append(type);
        append(SPACE);

        appendLiteral(String.class, false, path);

        KomodoObject defaultExp = reference(context.tx(), node, ObjectColumn.DEFAULT_EXPRESSION_REF_NAME);
        if (defaultExp != null) {
            append(SPACE);
            append(DEFAULT);
            append(SPACE);
            visit(context.tx(), defaultExp);
        }

        return null;
    }

    public Object textColumn(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String name = propertyString(context.tx(), node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(context.tx(), node, ProjectedColumn.TYPE_PROP_NAME);
        boolean ordinal = propertyBoolean(context.tx(), node, TextColumn.ORDINAL_PROP_NAME);

        append(SPACE);
        appendDisplayName(name);
        append(SPACE);

        if (ordinal) {
            // Will only ever come in here if Teiid 8.7 or greater
            append(FOR);
            append(SPACE);
            append(ORDINALITY);
        } else {
            append(type);

            String width = propertyString(context.tx(), node, TextColumn.WIDTH_PROP_NAME);
            if (width != null) {
                append(SPACE);
                append(WIDTH);
                append(SPACE);
                append(width);
            }

            boolean noTrim = propertyBoolean(context.tx(), node, TextColumn.NO_TRIM_PROP_NAME);
            if (noTrim) {
                append(SPACE);
                append(NO);
                append(SPACE);
                append(TRIM);
            }

            String colSelector = propertyString(context.tx(), node, TextColumn.SELECTOR_PROP_NAME);
            if (colSelector != null) {
                append(SPACE);
                append(SELECTOR);
                append(SPACE);
                append(escapeSinglePart(colSelector));
                append(SPACE);

                long position = propertyLong(context.tx(), node, TextColumn.POSITION_PROP_NAME);
                append(Long.toString(position));
            }
        }

        return null;
    }

    public Object xmlColumn(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        
        String name = propertyString(context.tx(), node, ProjectedColumn.NAME_PROP_NAME);
        String type = propertyString(context.tx(), node, ProjectedColumn.TYPE_PROP_NAME);
        String path = propertyString(context.tx(), node, XMLColumn.PATH_PROP_NAME);
        KomodoObject defaultExp = reference(context.tx(), node, XMLColumn.DEFAULT_EXPRESSION_REF_NAME);
        boolean ordinal = propertyBoolean(context.tx(), node, XMLColumn.ORDINAL_PROP_NAME);

        append(SPACE);
        appendDisplayName(name);
        append(SPACE);

        if (ordinal) {
            append(FOR);
            append(SPACE);
            append(ORDINALITY);
        } else {
            append(type);

            if (defaultExp != null) {
                append(SPACE);
                append(DEFAULT);
                append(SPACE);
                visit(context.tx(), defaultExp);
            }

            if (path != null) {
                append(SPACE);
                append(PATH);
                append(SPACE);
                appendLiteral(String.class, false, path);
            }
        }

        return null;
    }

    public Object makeDep(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean hasMax = node.hasProperty(context.tx(), MakeDep.MAX_PROP_NAME);
        boolean join = propertyBoolean(context.tx(), node, MakeDep.JOIN_PROP_NAME);

        boolean parens = false;
        if (hasMax || join) {
            append(OPEN_BRACKET);
            parens = true;
        }

        boolean space = false;

        if (hasMax) {
            if (space) {
                append(SPACE);
            } else {
                space = true;
            }

            append(MAX);
            append(COLON);

            long max = propertyLong(context.tx(), node, MakeDep.MAX_PROP_NAME);
            append(Long.toString(max));
        }

        if (join) {
            if (space) {
                append(SPACE);
            } else {
                space = true;
            }

            append(JOIN);
        }

        if (parens) {
            append(CLOSE_BRACKET);
        }

        return null;
    }

    public Object option(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(OPTION);

        Collection<String> groups = propertyValues(context.tx(), node, Option.DEPENDENT_GROUPS_PROP_NAME, DataTypeName.STRING);
        if (groups != null && !groups.isEmpty()) {
            append(SPACE);
            append(MAKEDEP);
            append(SPACE);

            Iterator<String> iter = groups.iterator();
            KomodoObject[] groupOptions = references(context.tx(), node, Option.DEPENDENT_GROUP_OPTIONS_REF_NAME);
            Iterator<KomodoObject> groupOptionsIter = Arrays.asList(groupOptions).iterator();

            for(int i = 0; iter.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                appendDisplayName(iter.next());

                if (groupOptionsIter.hasNext())
                    visit(context.tx(), groupOptionsIter.next());

            }
        }

        Collection<String> notGroups = propertyValues(context.tx(), node, Option.NOT_DEPENDENT_GROUPS_PROP_NAME, DataTypeName.STRING);
        if (notGroups != null && !notGroups.isEmpty()) {
            append(SPACE);
            append(MAKENOTDEP);
            append(SPACE);

            Iterator<String> iter = groups.iterator();
            for (int i = 0; iter.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                appendDisplayName(iter.next());
            }
        }

        boolean noCache = propertyBoolean(context.tx(), node, Option.NO_CACHE_PROP_NAME);
        Collection<String> noCacheGroups = propertyValues(context.tx(), node, Option.NO_CACHE_GROUPS_PROP_NAME, DataTypeName.STRING);
        if (noCacheGroups != null && !noCacheGroups.isEmpty()) {
            append(SPACE);
            append(NOCACHE);
            append(SPACE);

            Iterator<String> iter = noCacheGroups.iterator();

            for (int i = 0; iter.hasNext(); ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                appendDisplayName(iter.next());
            }
        } else if (noCache) {
            append(SPACE);
            append(NOCACHE);
        }

        return null;
    }

    public Object orderBy(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(ORDER);
        append(SPACE);
        append(BY);
        append(SPACE);

        iterate(context.tx(), node, OrderBy.ORDER_BY_ITEMS_REF_NAME);

        return null;
    }

    /**
     * @param name string version of a NullOrdering enum value
     * @return value with given name
     */
    private NullOrdering findNullOrdering(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (NullOrdering no : SortSpecification.NullOrdering.values()) {
            if (no.name().equals(name))
                return no;
        }
        return null;
    }

    public Object orderByItem(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject ses = reference(context.tx(), node, OrderByItem.SYMBOL_REF_NAME);
        if (instanceOf(context.tx(), ses, LexTokens.ALIAS_SYMBOL)) {
            appendDisplayName(outputName(context.tx(), ses));
        } else {
            visit(context.tx(), ses);
        }

        boolean ascending = propertyBoolean(context.tx(), node, OrderByItem.ASCENDING_PROP_NAME);
        if (!ascending) {
            append(SPACE);
            append(DESC);
        } // Don't print default "ASC"

        String nullOrderingName = propertyString(context.tx(), node, OrderByItem.NULL_ORDERING_PROP_NAME);
        NullOrdering nullOrdering = findNullOrdering(nullOrderingName);
        if (nullOrdering != null) {
            append(SPACE);
            append(NULLS);
            append(SPACE);
            append(nullOrdering.name());
        }

        return null;
    }

    public Object spParameter(TeiidSqlNodeContext context) {
        return null;
    }

    public Object select(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean distinct = propertyBoolean(context.tx(), node, Select.DISTINCT_PROP_NAME);
        if (distinct) {
            append(SPACE);
            append(DISTINCT);
        }

        append(SPACE);

        iterate(context.tx(), node, Select.SYMBOLS_REF_NAME);

        return null;
    }

    public Object setClause(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject symbol = reference(context.tx(), node, SetClause.SYMBOL_REF_NAME);
        String name = propertyString(context.tx(), symbol, Symbol.NAME_PROP_NAME);
        append(shortName(name));

        append(SPACE + EQUALS + SPACE);

        KomodoObject value = reference(context.tx(), node, SetClause.VALUE_REF_NAME);
        visit(context.tx(), value);

        return null;
    }

    public Object setClauseList(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        iterate(context.tx(), node, SetClauseList.SET_CLAUSES_REF_NAME);

        return null;
    }

    protected void appendSourceHintValue(String sh) {
        append(COLON);
        append(QUOTE_MARK);
        append(escapeStringValue(sh, QUOTE_MARK));
        append(QUOTE_MARK);
        append(SPACE);
    }

    public Object sourceHint(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(SPACE);
        append(BEGIN_HINT);
        append("sh"); //$NON-NLS-1$

        boolean useAliases = propertyBoolean(context.tx(), node, SourceHint.USE_ALIASES_PROP_NAME);
        if (useAliases) {
            append(SPACE);
            append("KEEP ALIASES"); //$NON-NLS-1$
        }

        String generalHint = propertyString(context.tx(), node, SourceHint.GENERAL_HINT_PROP_NAME);
        if (generalHint != null) {
            appendSourceHintValue(generalHint);
        } else {
            append(SPACE);
        }

        KomodoObject[] specificHints = references(context.tx(), node, SourceHint.SOURCE_HINTS_REF_NAME);
        for(int i = 0; i < specificHints.length; ++i) {
            visit(context.tx(), specificHints[i]);
        }

        append(END_HINT);

        return null;
    }

    public Object specificHint(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean useAliases = propertyBoolean(context.tx(), node, SpecificHint.USE_ALIASES_PROP_NAME);
        String translator = propertyString(context.tx(), node, SpecificHint.TRANSLATOR_NAME_PROP_NAME);
        String hint = propertyString(context.tx(), node, SpecificHint.HINT_PROP_NAME);
        
        append(translator);
        if (useAliases) {
            append(SPACE);
            append("KEEP ALIASES"); //$NON-NLS-1$
        }

        appendSourceHintValue(hint);

        return null;
    }

    public Object subqueryHint(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean noUnnest = propertyBoolean(context.tx(), node, SubqueryHint.NO_UNNEST_PROP_NAME);
        boolean depJoin = propertyBoolean(context.tx(), node, SubqueryHint.DEP_JOIN_PROP_NAME);
        boolean mergeJoin = propertyBoolean(context.tx(), node, SubqueryHint.MERGE_JOIN_PROP_NAME);

        if (noUnnest) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(NOUNNEST);
            append(SPACE);
            append(END_HINT);
        } else if (depJoin) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(SubqueryHint.DJ);
            append(SPACE);
            append(END_HINT);
        } else if (mergeJoin) {
            append(SPACE);
            append(BEGIN_HINT);
            append(SPACE);
            append(SubqueryHint.MJ);
            append(SPACE);
            append(END_HINT);
        }

        return null;
    }

    public Object withQueryCommand(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject groupSymbol = reference(context.tx(), node, WithQueryCommand.GROUP_SYMBOL_REF_NAME);
        visit(context.tx(), groupSymbol);
        
        append(SPACE);

        KomodoObject[] columns = references(context.tx(), node, WithQueryCommand.COLUMNS_REF_NAME);
        if (columns != null && columns.length > 0) {
            append(OPEN_BRACKET);

            for(int i = 0; i < columns.length; ++i) {
                if (i > 0)
                    append(COMMA + SPACE);

                KomodoObject column = columns[i];
            
                TeiidSqlNodeContext ccontext = new TeiidSqlNodeContext(context.tx(), column);
                ccontext.add(SHORT_NAME_ONLY_KEY, true);
                visit(column, ccontext);
            }

            append(CLOSE_BRACKET);
            append(SPACE);
        }

        append(AS);
        append(SPACE);
        append(OPEN_BRACKET);

        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        if (command == null) {
            append("<dependent values>"); //$NON-NLS-1$
        } else {
            visit(context.tx(), command);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    private void createAssignment(UnitOfWork transaction, KomodoObject node) throws Exception {
        KomodoObject variable = reference(transaction, node, AssignmentStatement.VARIABLE_REF_NAME);
        visit(transaction, variable);

        KomodoObject value = reference(transaction, node, AssignmentStatement.VALUE_REF_NAME);
        if (value == null)
            value = reference(transaction, node, ExpressionStatement.EXPRESSION_REF_NAME);

        if (value != null) {
            append(SPACE + EQUALS + SPACE);
            visit(transaction, value);
        }

        append(SEMI_COLON);
    }

    public Object assignmentStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        createAssignment(context.tx(), node);

        return null;
    }

    public Object declareStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(DECLARE);
        append(SPACE);

        String varType = propertyString(context.tx(), node, DeclareStatement.VARIABLE_TYPE_PROP_NAME);
        append(varType);
        append(SPACE);
        createAssignment(context.tx(), node);

        return null;
    }

    public Object returnStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(RETURN);

        KomodoObject expression = reference(context.tx(), node, ExpressionStatement.EXPRESSION_REF_NAME);
        if (expression != null) {
            append(SPACE);
            visit(context.tx(), expression);
        }

        append(SEMI_COLON);

        return null;
    }

    public Object block(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        appendLabel(context.tx(), node);

        // Add first clause
        append(BEGIN);

        boolean atomic = propertyBoolean(context.tx(), node, Block.ATOMIC_PROP_NAME);
        if (atomic) {
            append(SPACE);
            append(ATOMIC);
        }

        append(NEW_LINE);

        appendStatements(context.tx(), node, Block.STATEMENTS_REF_NAME);

        String exceptionGroup = propertyString(context.tx(), node, Block.EXCEPTION_GROUP_PROP_NAME);
        if (exceptionGroup != null) {
            append(EXCEPTION);
            append(SPACE);
            appendDisplayName(exceptionGroup);
            append(NEW_LINE);

            appendStatements(context.tx(), node, Block.EXCEPTION_STATEMENTS_REF_NAME);
        }

        append(END);

        return null;
    }

    public Object branchingStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String modeName = propertyString(context.tx(), node, BranchingStatement.MODE_PROP_NAME);
        BranchingMode mode = BranchingMode.findBranchingMode(modeName);

        append(mode.name());

        String label = propertyString(context.tx(), node, Labeled.LABEL_PROP_NAME);
        if (label != null) {
            append(SPACE);
            appendDisplayName(label);
        }

        append(SEMI_COLON);
        
        return null;
    }

    public Object commandStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        visit(context.tx(), command);

        boolean returnable = propertyBoolean(context.tx(), node, CommandStatement.RETURNABLE_PROP_NAME);
        if (!returnable) {
            append(SPACE);
            append(WITHOUT);
            append(SPACE);
            append(RETURN);
        }

        append(SEMI_COLON);

        return null;
    }

    public Object ifStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(IF);
        append(OPEN_BRACKET);

        KomodoObject condition = reference(context.tx(), node, IfStatement.CONDITION_REF_NAME);
        visit(context.tx(), condition);

        append(CLOSE_BRACKET);
        append(NEW_LINE);

        KomodoObject ifBlock = reference(context.tx(), node, IfStatement.IF_BLOCK_REF_NAME);
        visit(context.tx(), ifBlock);

        KomodoObject elseBlock = reference(context.tx(), node, IfStatement.ELSE_BLOCK_REF_NAME);
        if (elseBlock != null) {
            append(NEW_LINE);
            append(ELSE);
            append(NEW_LINE);
            visit(context.tx(), elseBlock);
        }

        return null;
    }

    public Object loopStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        appendLabel(context.tx(), node);
        append(LOOP);
        append(SPACE);
        append(ON);
        append(SPACE + OPEN_BRACKET);

        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        visit(context.tx(), command);

        append(CLOSE_BRACKET + SPACE);
        append(AS);
        append(SPACE);

        String cursorName = propertyString(context.tx(), node, LoopStatement.CURSOR_NAME_PROP_NAME);
        appendDisplayName(cursorName);

        append(NEW_LINE);
 
        KomodoObject block = reference(context.tx(), node, LoopStatement.BLOCK_REF_NAME);
        visit(context.tx(), block);

        return null;
    }

    public Object raiseStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(RAISE);
        append(SPACE);

        boolean warning = propertyBoolean(context.tx(), node, RaiseStatement.WARNING_PROP_NAME);
        if (warning) {
            append(SQLWARNING);
            append(SPACE);
        }

        KomodoObject expression = reference(context.tx(), node, ExpressionStatement.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);
        append(SEMI_COLON);

        return null;
    }

    public Object whileStatement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        appendLabel(context.tx(), node);
        append(WHILE);
        append(OPEN_BRACKET);

        KomodoObject condition = reference(context.tx(), node, WhileStatement.CONDITION_REF_NAME);
        visit(context.tx(), condition);
        append(SEMI_COLON + NEW_LINE);
 
        KomodoObject block = reference(context.tx(), node, WhileStatement.BLOCK_REF_NAME);
        visit(context.tx(), block);

        return null;
    }

    public Object exceptionExpression(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(SQLEXCEPTION);
        append(SPACE);

        KomodoObject message = reference(context.tx(), node, ExceptionExpression.MESSAGE_REF_NAME);
        KomodoObject sqlState = reference(context.tx(), node, ExceptionExpression.SQL_STATE_REF_NAME);
        KomodoObject errorCode = reference(context.tx(), node, ExceptionExpression.ERROR_CODE_REF_NAME);
        KomodoObject parent = reference(context.tx(), node, ExceptionExpression.PARENT_EXPRESSION_REF_NAME);

        visit(context.tx(), message);

        if (sqlState != null) {
            append(SPACE);
            append(SQLSTATE);
            append(SPACE);
            visit(context.tx(), sqlState);

            if (errorCode != null) {
                append(COMMA);
                append(SPACE);
                visit(context.tx(), errorCode);
            }
        }

        if (parent != null) {
            append(SPACE);
            append(CHAIN);
            append(SPACE);
            visit(context.tx(), parent);
        }

        return null;
    }

    public Object function(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String name = propertyString(context.tx(), node, Function.NAME_PROP_NAME);
        boolean implicit = propertyBoolean(context.tx(), node, Function.IMPLICIT_PROP_NAME);
        KomodoObject[] args = references(context.tx(), node, Function.ARGS_REF_NAME);
        Iterable<KomodoObject> argIterable = Arrays.asList(args);
        Iterator<KomodoObject> argIter = argIterable.iterator();

        if (implicit) {
            // Hide this function, which is implicit
            visit(context.tx(), argIter.next());

        } else if (name.equalsIgnoreCase(CONVERT) || name.equalsIgnoreCase(CAST)) {
            append(name);
            append(OPEN_BRACKET);

            if (argIter.hasNext()) {
                visit(context.tx(), argIter.next());

                if (name.equalsIgnoreCase(CONVERT)) {
                    append(COMMA + SPACE);
                } else {
                    append(SPACE);
                    append(AS);
                    append(SPACE);
                }

                KomodoObject args1 = null;
                if (argIter.hasNext())
                    args1 = argIter.next();

                if (args1 != null && instanceOf(context.tx(), args1, LexTokens.CONSTANT)) {
                    Property valueProp = args1.getProperty(context.tx(), Constant.VALUE_PROP_NAME);
                    append(toString(context.tx(), valueProp));
                } else {
                    append(undefined());
                }

            }

            append(CLOSE_BRACKET);

        } else if (name.equals(PLUS) || name.equals(MINUS) || name.equals(MULTIPLY) || name.equals(DIVIDE) || name.equals(LOGICAL_OR)) {
            append(OPEN_BRACKET);

            for(int i = 0; argIter.hasNext(); ++i) {
                if (i > 0) {
                    append(SPACE);
                    append(name);
                    append(SPACE);
                }

                visit(context.tx(), argIter.next());
            }

            append(CLOSE_BRACKET);

        } else if (name.equalsIgnoreCase(TIMESTAMPADD) || name.equalsIgnoreCase(TIMESTAMPDIFF)) {
            append(name);
            append(OPEN_BRACKET);

            if (argIter.hasNext()) {
                Property valueProp = argIter.next().getProperty(context.tx(), Constant.VALUE_PROP_NAME);
                append(toString(context.tx(), valueProp));

                iterate(context.tx(), args);
            }

            append(CLOSE_BRACKET);

        } else if (name.equalsIgnoreCase(XMLPI)) {
            append(name);
            append(OPEN_BRACKET + "NAME" + SPACE); //$NON-NLS-1$

            Property valueProp = argIter.next().getProperty(context.tx(), Constant.VALUE_PROP_NAME);
            appendDisplayName(toString(context.tx(), valueProp));
            iterate(context.tx(), args);

            append(CLOSE_BRACKET);

        } else if (name.equalsIgnoreCase(TRIM)) {
            append(name);
            append(OPEN_BRACKET);

            Property valueProp = argIter.next().getProperty(context.tx(), Constant.VALUE_PROP_NAME);
            String value = toString(context.tx(), valueProp);
            if (!value.equalsIgnoreCase(BOTH)) {
                append(value);
                append(SPACE);
            }

            visit(context.tx(), argIter.next());
            append(SPACE);
            append(FROM);
            append(SPACE);
            visit(context.tx(), argIter.next());
            append(CLOSE_BRACKET);
        } else {
            append(name);
            append(OPEN_BRACKET);
            iterate(context.tx(), args);
            append(CLOSE_BRACKET);
        }

        return null;
    }

    public Object aggregateSymbol(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String name = propertyString(context.tx(), node, AggregateSymbol.NAME_PROP_NAME);
        boolean distinct = propertyBoolean(context.tx(), node, AggregateSymbol.DISTINCT_PROP_NAME);

        String aggFunctionName = propertyString(context.tx(), node, AggregateSymbol.AGGREGATE_FUNCTION_PROP_NAME);
        AggregateFunctions aggregateFunction = AggregateFunctions.findAggregateFunction(aggFunctionName);

        append(name);
        append(OPEN_BRACKET);

        if (distinct) {
            append(DISTINCT);
            append(SPACE);
        } else if (aggregateFunction == AggregateFunctions.USER_DEFINED) {
            append(ALL);
            append(SPACE);
        }

        KomodoObject[] args = references(context.tx(), node, AggregateSymbol.ARGS_REF_NAME);
        if (args.length > 0) {
            iterate(context.tx(), args);
        } else {
            if (aggregateFunction == AggregateFunctions.COUNT) {
                append(ALL_COLS);
            }
        }

        KomodoObject orderBy = reference(context.tx(), node, AggregateSymbol.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            append(SPACE);
            visit(context.tx(), orderBy);
        }

        append(CLOSE_BRACKET);

        KomodoObject condition = reference(context.tx(), node, AggregateSymbol.CONDITION_REF_NAME);
        if (condition != null) {
            append(SPACE);
            append(FILTER);
            append(OPEN_BRACKET);
            append(WHERE);
            append(SPACE);
            visit(context.tx(), condition);
            append(CLOSE_BRACKET);
        }

        return null;
    }

    public Object aliasSymbol(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject symbol = reference(context.tx(), node, AliasSymbol.SYMBOL_REF_NAME);
        visit(context.tx(), symbol);
        append(SPACE);
        append(AS);
        append(SPACE);

        append(escapeSinglePart(outputName(context.tx(), node)));

        return null;
    }

    public Object elementSymbol(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean shortNameOnly = context.get(SHORT_NAME_ONLY_KEY) != null ? (Boolean) context.get(SHORT_NAME_ONLY_KEY) : false;

        String displayModeName = propertyString(context.tx(), node, ElementSymbol.DISPLAY_MODE_PROP_NAME);
        DisplayMode displayMode = DisplayMode.findDisplayMode(displayModeName);

        String outputName = outputName(context.tx(), node);
        
        if (DisplayMode.SHORT_OUTPUT_NAME.equals(displayMode) || shortNameOnly) {
            appendDisplayName(shortName(outputName));
            return null;
        }

        if (DisplayMode.FULLY_QUALIFIED.equals(displayMode)) {
            outputName = propertyString(context.tx(), node,  Symbol.NAME_PROP_NAME);
        }

        appendDisplayName(outputName);

        return null;
    }

    public Object expressionSymbol(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        KomodoObject expression = reference(context.tx(), node, ExpressionSymbol.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);
        return null;
    }

    public Object groupSymbol(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);
        String alias = null;
        String name = propertyString(context.tx(), node, org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon.Symbol.NAME_PROP_NAME);
        String defn = propertyString(context.tx(), node, GroupSymbol.DEFINITION_PROP_NAME);
        
        if (defn != null) {
            alias = name;
            name = defn;
        }

        appendDisplayName(name);

        if (alias != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            append(escapeSinglePart(alias));
        }

        return null;
    }

    public Object arraySymbol(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        boolean implicit = propertyBoolean(context.tx(), node, ArraySymbol.IMPLICIT_PROP_NAME);
        if (!implicit) {
            append(OPEN_BRACKET);
        }

        iterate(context.tx(), node, ArraySymbol.EXPRESSIONS_REF_NAME);
        if (!implicit) {

            if (size(context.tx(), node, ArraySymbol.EXPRESSIONS_REF_NAME) == 1) {
                append(COMMA);
            }

            append(CLOSE_BRACKET);
        }

        return null;
    }

    public Object caseExpression(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(CASE);
        append(SPACE);
        KomodoObject expression = reference(context.tx(), node, CaseExpression.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);
        append(SPACE);

        KomodoObject[] whens = references(context.tx(), node, CaseExpression.WHEN_REF_NAME);
        KomodoObject[] thens = references(context.tx(), node, CaseExpression.THEN_REF_NAME);
        for(int i = 0; i < whens.length && i < thens.length; ++i) {
            append(WHEN);
            append(SPACE);
            visit(context.tx(), whens[i]);
            append(SPACE);
            append(THEN);
            append(SPACE);
            visit(context.tx(), thens[i]);
            append(SPACE);
        }

        KomodoObject elseExpression = reference(context.tx(), node, CaseExpression.ELSE_EXPRESSION_REF_NAME);
        if (elseExpression != null) {
            append(ELSE);
            append(SPACE);
            visit(context.tx(), elseExpression);
            append(SPACE);
        }

        append(END);

        return null;
    }

    public Object constant(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        String typeName = propertyString(context.tx(), node, Expression.TYPE_CLASS_PROP_NAME);
        DataTypeName dataTypeName = DataTypeName.findDataTypeName(typeName);
        if (dataTypeName == null)
            dataTypeName = DataTypeName.OBJECT;

        Class<?> type = getDataTypeService().getDefaultDataClass(dataTypeName);
        boolean multiValued = propertyBoolean(context.tx(), node, Constant.MULTI_VALUED_PROP_NAME);
        Object value = propertyValue(context.tx(), node, Constant.VALUE_PROP_NAME, dataTypeName);
        appendLiteral(type, multiValued, value);

        return null;
    }

    public Object derivedColumn(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject expression = reference(context.tx(), node, DerivedColumn.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);

        String alias = propertyString(context.tx(), node, DerivedColumn.ALIAS_PROP_NAME);
        if (alias != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            appendDisplayName(alias);
        }

        return null;
    }

    public Object jsonObject(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(JSONOBJECT);
        append(OPEN_BRACKET);
        iterate(context.tx(), node, JSONObject.ARGS_REF_NAME);
        append(CLOSE_BRACKET);

        return null;
    }

    public Object multipleElementSymbol(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject groupKomodoObject = reference(context.tx(), node, MultipleElementSymbol.GROUP_REF_NAME);
        if (groupKomodoObject == null) {
            append(STAR);
        } else {
            visit(context.tx(), groupKomodoObject);
            append(DOT);
            append(STAR);
        }

        return null;
    }

    public Object queryString(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(QUERYSTRING);
        append(OPEN_BRACKET);

        KomodoObject path = reference(context.tx(), node, QueryString.PATH_REF_NAME);
        visit(context.tx(), path);

        KomodoObject[] args = references(context.tx(), node, QueryString.ARGS_REF_NAME);
        if (args.length > 0) {
            append(COMMA);
            append(SPACE);
            iterate(context.tx(), node, QueryString.ARGS_REF_NAME);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    public Object reference(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject expression = reference(context.tx(), node, Reference.EXPRESSION_REF_NAME);
        boolean positional = propertyBoolean(context.tx(), node, Reference.POSITIONAL_PROP_NAME);
        if (!positional && expression != null) {    
            visit(context.tx(), expression);
        } else {
            append(QUESTION_MARK);
        }

        return null;
    }

    public Object scalarSubquery(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        // operator and beginning of list
        append(OPEN_BRACKET);
        KomodoObject command = reference(context.tx(), node, SubqueryContainer.COMMAND_REF_NAME);
        visit(context.tx(), command);
        append(CLOSE_BRACKET);

        return null;
    }

    public Object searchedCaseExpression(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(CASE);

        KomodoObject[] whens = references(context.tx(), node, CaseExpression.WHEN_REF_NAME);
        KomodoObject[] thens = references(context.tx(), node, CaseExpression.THEN_REF_NAME);
        for(int i = 0; i < whens.length && i < thens.length; ++i) {
            append(SPACE);
            append(WHEN);
            append(SPACE);
            visit(context.tx(), whens[i]);
            append(SPACE);
            append(THEN);
            append(SPACE);
            visit(context.tx(), thens[i]);
        }

        append(SPACE);

        KomodoObject elseExpression = reference(context.tx(), node, CaseExpression.ELSE_EXPRESSION_REF_NAME);
        if (elseExpression != null) {
            append(ELSE);
            append(SPACE);
            visit(context.tx(), elseExpression);
            append(SPACE);
        }

        append(END);

        return null;
    }

    public Object textLine(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(FOR);
        append(SPACE);
        iterate(context.tx(), node, TextLine.EXPRESSIONS_REF_NAME);

        String delimiter = propertyString(context.tx(), node, TextLine.DELIMITER_PROP_NAME);
        if (delimiter != null) {
            append(SPACE);
            append(DELIMITER);
            append(SPACE);
            appendLiteral(String.class, false, delimiter);
        }

        String quote = propertyString(context.tx(), node, TextLine.QUOTE_PROP_NAME);
        if (quote != null) {
            append(SPACE);
            append(QUOTE);
            append(SPACE);
            appendLiteral(String.class, false, quote);
        }

        boolean includeHeader = propertyBoolean(context.tx(), node, TextLine.INCLUDE_HEADER_PROP_NAME);
        if (!includeHeader) {
            append(SPACE);
            append(HEADER);
        }

        String encoding = propertyString(context.tx(), node, TextLine.ENCODING_PROP_NAME);
        if (encoding != null) {
            append(SPACE);
            append(ENCODING);
            append(SPACE);
            appendDisplayName(encoding);
        }

        return null;
    }

    public Object windowFunction(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        KomodoObject windowFunction = reference(context.tx(), node, WindowFunction.FUNCTION_REF_NAME);
        visit(context.tx(), windowFunction);

        append(SPACE);
        append(OVER);
        append(SPACE);

        KomodoObject windowSpec = reference(context.tx(), node, WindowFunction.WINDOW_SPECIFICATION_REF_NAME);
        visit(context.tx(), windowSpec);

        return null;
    }

    public Object windowSpecification(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(OPEN_BRACKET);
        boolean needsSpace = false;
        
        KomodoObject[] partitions = references(context.tx(), node, WindowSpecification.PARTITION_REF_NAME);
        if (partitions.length > 0) {
            append(PARTITION);
            append(SPACE);
            append(BY);
            append(SPACE);
            iterate(context.tx(), node, WindowSpecification.PARTITION_REF_NAME);
            needsSpace = true;
        }

        KomodoObject orderBy = reference(context.tx(), node, WindowSpecification.ORDER_BY_REF_NAME);
        if (orderBy != null) {
            if (needsSpace) {
                append(SPACE);
            }

            visit(context.tx(), orderBy);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    public Object xmlAttributes(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(XMLATTRIBUTES);
        append(OPEN_BRACKET);
        iterate(context.tx(), node, XMLAttributes.ARGS_REF_NAME);
        append(CLOSE_BRACKET);

        return null;
    }

    public Object xmlElement(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(XMLELEMENT);
        append(OPEN_BRACKET + "NAME" + SPACE); //$NON-NLS-1$

        String name = propertyString(context.tx(), node, XMLElement.NAME_PROP_NAME);
        appendDisplayName(name);

        KomodoObject namespaces = reference(context.tx(), node, XMLElement.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            append(COMMA + SPACE);
            visit(context.tx(), namespaces);
        }

        KomodoObject attributes = reference(context.tx(), node, XMLElement.ATTRIBUTES_REF_NAME);
        if (attributes != null) {
            append(COMMA + SPACE);
            visit(context.tx(), attributes);
        }

        KomodoObject[] contents = references(context.tx(), node, XMLElement.CONTENT_REF_NAME);
        if (contents.length > 0) {
            append(COMMA + SPACE);
        }

        iterate(context.tx(), node, XMLElement.CONTENT_REF_NAME);

        append(CLOSE_BRACKET);

        return null;
    }

    public Object xmlForest(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(XMLFOREST);
        append(OPEN_BRACKET);

        KomodoObject namespaces = reference(context.tx(), node, XMLForest.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            visit(context.tx(), namespaces);
            append(COMMA + SPACE);
        }

        iterate(context.tx(), node, XMLForest.ARGUMENTS_REF_NAME);

        append(CLOSE_BRACKET);

        return null;
    }

    public Object xmlNamespaces(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(XMLNAMESPACES);
        append(OPEN_BRACKET);

        iterate(context.tx(), node, XMLNamespaces.NAMESPACE_ITEMS_REF_NAME);

        append(CLOSE_BRACKET);

        return null;
    }

    public Object xmlParse(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(XMLPARSE);
        append(OPEN_BRACKET);

        boolean document = propertyBoolean(context.tx(), node, XMLParse.DOCUMENT_PROP_NAME);
        if (document) {
            append(DOCUMENT);
        } else {
            append(CONTENT);
        }

        append(SPACE);
        
        KomodoObject expression = reference(context.tx(), node, XMLParse.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);

        boolean wellFormed = propertyBoolean(context.tx(), node, XMLParse.WELL_FORMED_PROP_NAME);
        if (wellFormed) {
            append(SPACE);
            append(WELLFORMED);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    public Object xmlQuery(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(XMLQUERY);
        append(OPEN_BRACKET);

        KomodoObject namespaces = reference(context.tx(), node, XMLQuery.NAMESPACES_REF_NAME);
        if (namespaces != null) {
            visit(context.tx(), namespaces);
            append(COMMA);
            append(SPACE);
        }

        String xquery = propertyString(context.tx(), node, XMLQuery.XQUERY_PROP_NAME);
        appendLiteral(String.class, false, xquery);

        KomodoObject[] passings = references(context.tx(), node, XMLQuery.PASSING_REF_NAME);
        if (passings.length > 0) {
            append(SPACE);
            append(PASSING);
            append(SPACE);
            iterate(context.tx(), node, XMLQuery.PASSING_REF_NAME);
        }

        
        if (node.hasProperty(context.tx(), XMLQuery.EMPTY_ON_EMPTY_PROP_NAME)) {
            append(SPACE);
            boolean emptyOnEmpty = propertyBoolean(context.tx(), node, XMLQuery.EMPTY_ON_EMPTY_PROP_NAME);
            if (emptyOnEmpty) {
                append(EMPTY);
            } else {
                append(NULL);
            }

            append(SPACE);
            append(ON);
            append(SPACE);
            append(EMPTY);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    public Object xmlSerialize(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(XMLSERIALIZE);
        append(OPEN_BRACKET);
        
        if (node.hasProperty(context.tx(), XMLSerialize.DOCUMENT_PROP_NAME)) {
            boolean document = propertyBoolean(context.tx(), node,  XMLSerialize.DOCUMENT_PROP_NAME);
            if (document) {
                append(DOCUMENT);
            } else {
                append(CONTENT);
            }

            append(SPACE);
        }

        KomodoObject expression = reference(context.tx(), node, XMLSerialize.EXPRESSION_REF_NAME);
        visit(context.tx(), expression);

        String typeString = propertyString(context.tx(), node,  XMLSerialize.TYPE_STRING_PROP_NAME);
        if (typeString != null) {
            append(SPACE);
            append(AS);
            append(SPACE);
            append(typeString);
        }

        String encoding = propertyString(context.tx(), node,  XMLSerialize.ENCODING_PROP_NAME);
        if (encoding != null) {
            append(SPACE);
            append(ENCODING);
            append(SPACE);
            append(escapeSinglePart(encoding));
        }

        String version = propertyString(context.tx(), node,  XMLSerialize.VERSION_PROP_NAME);
        if (version != null) {
            append(SPACE);
            append(VERSION);
            append(SPACE);
            appendLiteral(String.class, false, version);
        }

        if (node.hasProperty(context.tx(), XMLSerialize.DECLARATION_PROP_NAME)) {
            boolean declaration = propertyBoolean(context.tx(), node,  XMLSerialize.DECLARATION_PROP_NAME);
            append(SPACE);
            if (declaration) {
                append(INCLUDING);
            } else {
                append(EXCLUDING);
            }

            append(SPACE);
            append(XMLDECLARATION);
        }

        append(CLOSE_BRACKET);

        return null;
    }

    public Object cacheHint(TeiidSqlNodeContext context) throws Exception {
        KomodoObject node = (KomodoObject) context.get(NODE_KEY);

        append(BEGIN_HINT);
        append(SPACE);
        append("cache");

        boolean addParens = false;
        boolean prefersMemory = propertyBoolean(context.tx(), node,  CacheHint.PREFERS_MEMORY_PROP_NAME);
        if (prefersMemory) {
            append(OPEN_BRACKET);
            addParens = true;
            append("pref_mem");
        }

        String ttl = propertyString(context.tx(), node, CacheHint.TTL_PROP_NAME);
        if (ttl != null) {
            if (!addParens) {
                append(OPEN_BRACKET);
                addParens = true;
            } else {
                append(SPACE);
            }
            append("ttl:");
            append(ttl);
        }

        boolean updateable = propertyBoolean(context.tx(), node,  CacheHint.UPDATEABLE_PROP_NAME);
        if (updateable) {
            if (!addParens) {
                append(OPEN_BRACKET);
                addParens = true;
            } else {
                append(SPACE);
            }
            append("updatable");
        }

        String scope = propertyString(context.tx(), node, CacheHint.SCOPE_PROP_NAME);
        if (scope != null) {
            if (!addParens) {
                append(OPEN_BRACKET);
                addParens = true;
            } else {
                append(SPACE);
            }     
            append("scope:");
            append(scope);            
        }

        Long minRows = propertyLong(context.tx(), node, CacheHint.MIN_ROWS_PROP_NAME);
        if (minRows != null) {
            if (!addParens) {
                append(OPEN_BRACKET);
                addParens = true;
            } else {
                append(SPACE);
            }     
            append("min:");
            append(minRows.toString());
        }

        if (addParens) {
            append(CLOSE_BRACKET);
        }

        append(SPACE);
        append(END_HINT);
        beginClause(0);

        return null;
    }

    @Override
    public OperationType getRequestType() {
        return OperationType.READ_OPERATION;
    }
}
