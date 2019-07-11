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
package org.komodo.core.visitor;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.komodo.core.internal.repository.KObjectFactory;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.metadata.DataTypeService;
import org.komodo.metadata.DataTypeService.DataTypeName;
import org.komodo.metadata.MetadataNamespaces;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.TeiidSqlConstants;
import org.komodo.spi.TeiidSqlConstants.NonReserved;
import org.komodo.spi.TeiidSqlConstants.Reserved;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.OperationType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.utils.KeyInValueMap;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;

/**
 * Visitor that will walk a ddl kObject tree and convert it to
 * the a string.
 */
public class DdlNodeVisitor extends AbstractNodeVisitor
    implements Reserved, NonReserved, MetadataNamespaces {
	
	private static final String NOT_NULL = "NOT NULL";//$NON-NLS-1$
    private static final String UNDEFINED = "undefined"; //$NON-NLS-1$

    /**
     * Exclusions for what the visitor should avoid visiting in the given kObject.
     * By default, the visitor will visit all tables, procedures and functions.
     */
    public enum VisitorExclusions {
        /**
         * Exclude Tables
         */
        EXCLUDE_TABLES,

        /**
         * Exclude Table Constraints
         */
        EXCLUDE_TABLE_CONSTRAINTS,

        /**
         * Exclude Procedures
         */
        EXCLUDE_PROCEDURES,

        /**
         * Exclude Functions
         */
        EXCLUDE_FUNCTIONS
    }

    private StringBuilder ddlBuffer = new StringBuilder();

    private boolean includeTables = true;

    private boolean includeTableConstraints = true;

    private boolean includeProcedures = true;

    private boolean includeFunctions = true;

    private Set<DataTypeName> lengthDataTypes;

    private Set<DataTypeName> precisionDataTypes;

    private KeyInValueMap<String, URI> namespaceMap = new KeyInValueMap<String, URI>(new URIMapAdapter());

    private static Map<String, MixinTypeName> mixinTypeIndex = new HashMap<String, MixinTypeName>();

    private enum MixinTypeName {

        CREATE_TABLE(TeiidDdlLexicon.CreateTable.TABLE_STATEMENT),

        CREATE_VIEW(TeiidDdlLexicon.CreateTable.VIEW_STATEMENT),

        OPTION_NAMESPACE(TeiidDdlLexicon.OptionNamespace.STATEMENT),

        CREATE_PROCEDURE(TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT),

        CREATE_FUNCTION(TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT),

        UNKNOWN(UNDEFINED);

        private String kObjectTypeId;

        private MixinTypeName(String kObjectTypeId) {
            this.kObjectTypeId = kObjectTypeId;
            mixinTypeIndex.put(this.kObjectTypeId, this);
        }

        /**
         * Find the enum value related to the given kObject type
         *
         * @param kObjectType kObject type to attempt to find
         *
         * @return related mixin type name to the kObject type
         */
        public static MixinTypeName findName(String kObjectType) {
            if(kObjectType == null)
                return UNKNOWN;

            MixinTypeName mtName = mixinTypeIndex.get(kObjectType);
            if (mtName != null)
                return mtName;

            return UNKNOWN;
        }

    }

    private enum TableType {
        TABLE,
        VIEW,
        GLOBAL_TEMP_TABLE;
    }

    private class CreateObjectContext {

        private TableType tableType = TableType.TABLE;

        private boolean virtual = false;

        public boolean isPhysical() {
            return !virtual;
        }

        public void setPhysical(boolean physical) {
            this.virtual = !physical;
        }

        public boolean isVirtual() {
            return virtual;
        }

        public void setVirtual(boolean virtual) {
            this.virtual = virtual;
        }

        public TableType getTableType() {
            return tableType;
        }

        public void setTableType(TableType tableType) {
            this.tableType = tableType;
        }
    }

    private class ColumnContext {

        private boolean autoIncremented;

        private String nullType;

        private String dataTypeId;

        /**
         * @return auto incremented
         */
        public boolean isAutoIncremented() {
            return autoIncremented;
        }

        /**
         * @param autoIncremented auto incremented
         */
        public void setAutoIncremented(boolean autoIncremented) {
            this.autoIncremented = autoIncremented;
        }

        /**
         * @return is not null
         */
        public boolean isNotNull() {
            return NOT_NULL.equals(nullType);
        }

        /**
         * @param nullType null type
         */
        public void setNullType(String nullType) {
            this.nullType = nullType;
        }

        /**
         * @return data type name
         */
        public DataTypeName getDataTypeName() throws Exception {
            try {
                return getDataTypeService().getDataTypeName(dataTypeId);
            } catch (Exception ex) {
                throw new Exception(ex);
            }
        }

        /**
         * @return the dataType
         */
        public String getDataType() {
            return dataTypeId;
        }

        /**
         * @param dataType data type
         */
        public void setDataType(String dataType) {
            this.dataTypeId = dataType.toLowerCase();
        }
    }

    /**
     * @param version metadata version
     * @param dataTypeService the data type service
     * @param startOnNewLine prepend new line to start of ddl string
     * @param exclusions any items that should be excluded from visiting
     */
    public DdlNodeVisitor(DataTypeService dataTypeService, boolean startOnNewLine, VisitorExclusions... exclusions) {
        super(dataTypeService);

        if (exclusions != null) {
            for (VisitorExclusions exclusion : exclusions) {
                switch (exclusion) {
                    case EXCLUDE_TABLES:
                        this.includeTables = false;
                        break;
                    case EXCLUDE_TABLE_CONSTRAINTS:
                        this.includeTableConstraints = false;
                        break;
                    case EXCLUDE_PROCEDURES:
                        this.includeProcedures = false;
                        break;
                    case EXCLUDE_FUNCTIONS:
                        this.includeFunctions = false;
                        break;
                }
            }
        }
        if( startOnNewLine ) {
        	ddlBuffer.append(NEW_LINE);
        }
    }

    /**
     * @return the complete visited ddl string
     */
    public String getDdl() {
        String ddl = ddlBuffer.toString();

        if (ddl.trim().isEmpty())
            return EMPTY_STRING;

        return ddl;
    }

    @Override
    protected String undefined() {
        return UNDEFINED;
    }

    private DdlNodeVisitor append(Object o) {
        if (NEW_LINE.equals(o) && ddlBuffer.length() == 0) {
            // Ignore new line calls at the start of the whole text
            return this;
        }

        ddlBuffer.append(o);
        return this;
    }

    private Set<DataTypeName> getLengthDataTypes() {
        if (lengthDataTypes == null) {
            lengthDataTypes = new HashSet<DataTypeName>();
            lengthDataTypes.add(DataTypeName.CHAR);
            lengthDataTypes.add(DataTypeName.CLOB);
            lengthDataTypes.add(DataTypeName.BLOB);
            lengthDataTypes.add(DataTypeName.OBJECT);
            lengthDataTypes.add(DataTypeName.XML);
            lengthDataTypes.add(DataTypeName.STRING);
            lengthDataTypes.add(DataTypeName.VARBINARY);
            lengthDataTypes.add(DataTypeName.BIGINTEGER);
        }

        return lengthDataTypes;
    }

    private Set<DataTypeName> getPrecsionDataTypes() {
        if (precisionDataTypes == null) {
            precisionDataTypes = new HashSet<DataTypeName>();
            precisionDataTypes.add(DataTypeName.BIGDECIMAL);
        }

        return precisionDataTypes;
    }

    private ColumnContext createColumnContext(UnitOfWork transaction, KomodoObject columnKomodoObject) throws Exception {
        Property autoIncProp = property(transaction, columnKomodoObject, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT);
        Property nullProp = property(transaction, columnKomodoObject, StandardDdlLexicon.NULLABLE);
        Property dataTypeProp = property(transaction, columnKomodoObject, StandardDdlLexicon.DATATYPE_NAME);

        boolean autoIncremented = autoIncProp != null ? autoIncProp.getBooleanValue(transaction) : false;
        String nullType = toString(transaction, nullProp);
        String dataType = toString(transaction, dataTypeProp);

        ColumnContext columnContext = new ColumnContext();
        columnContext.setAutoIncremented(autoIncremented);
        columnContext.setNullType(nullType);
        columnContext.setDataType(dataType);

        return columnContext;
    }

    private String escapeStringValue(String str, String tick) {
        return StringUtils.replaceAll(str, tick, tick + tick);
    }

    protected String escapeSinglePart(String token) {
        if (TeiidSqlConstants.isReservedWord(token)) {
            return TeiidSqlConstants.Tokens.ID_ESCAPE_CHAR + token + TeiidSqlConstants.Tokens.ID_ESCAPE_CHAR;
        }
        boolean escape = true;
        char start = token.charAt(0);
        if (HASH.equals(Character.toString(start)) || AT.equals(Character.toString(start)) || StringUtils.isLetter(start)) {
            escape = false;
            for (int i = 1; !escape && i < token.length(); i++) {
                char c = token.charAt(i);
                escape = !StringUtils.isLetterOrDigit(c) && c != '_';
            }
        }
        if (escape) {
            return TeiidSqlConstants.Tokens.ID_ESCAPE_CHAR + escapeStringValue(token, SPEECH_MARK) + TeiidSqlConstants.Tokens.ID_ESCAPE_CHAR;
        }
        return token;
    }

    private void optionNamespace(UnitOfWork transaction, KomodoObject namespace) throws Exception {
        if (!hasMixinType(transaction, namespace, TeiidDdlLexicon.OptionNamespace.STATEMENT))
            return;

        String prefix = namespace.getName(transaction);
        String uriValue = undefined();
        Property uriProp = property(transaction, namespace, TeiidDdlLexicon.OptionNamespace.URI);
        if (uriProp != null)
            uriValue = toString(transaction, uriProp);

        if (namespaceMap.isEmpty()) {
            for (URI builtInUri : MetadataNamespaces.URI.map().values())
                namespaceMap.add(builtInUri);
        }

        URI uri = namespaceMap.get(prefix);
        if (uri == null) {
            uri = new URI(prefix, uriValue);
            namespaceMap.add(uri);
        }

        append(SET).append(SPACE).append(NAMESPACE).
        append(SPACE).append(QUOTE_MARK).
        append(StringUtils.replaceAll(uri.getUnbracedURI(), QUOTE_MARK, QUOTE_MARK + QUOTE_MARK)).
        append(QUOTE_MARK).
        append(SPACE).append(AS).append(SPACE).
        append(escapeSinglePart(uri.getPrefix())).
        append(SEMI_COLON);
    }

    private void columnDefault(UnitOfWork transaction, KomodoObject column, ColumnContext context) throws Exception {
        Property defaultProp = property(transaction, column, StandardDdlLexicon.DEFAULT_VALUE);
        if (defaultProp == null)
            return;

        String defaultValue = toString(transaction, defaultProp);
        append(SPACE).append(DEFAULT).append(SPACE).
        append(QUOTE_MARK).append(StringUtils.replaceAll(defaultValue, QUOTE_MARK, QUOTE_MARK + QUOTE_MARK)).
        append(QUOTE_MARK);
    }

    private void column(UnitOfWork transaction, KomodoObject column, ColumnContext context, boolean includeName, boolean includeType) throws Exception {
        if (includeName) {
            append(escapeSinglePart(column.getName(transaction)));
        }

        if (includeType) {
            if (includeName) {
                append(SPACE);
            }

            append(context.getDataType());

            Property colLengthProp = property(transaction, column, StandardDdlLexicon.DATATYPE_LENGTH);
            Property colPrecisionProp = property(transaction, column, StandardDdlLexicon.DATATYPE_PRECISION);
            Property colScaleProp = property(transaction, column, StandardDdlLexicon.DATATYPE_SCALE);
            Property colArrDimsProp = property(transaction, column, StandardDdlLexicon.DATATYPE_ARRAY_DIMENSIONS);

            long colLength = colLengthProp != null ? colLengthProp.getLongValue(transaction) : -1;
            long colPrecision = colPrecisionProp != null ? colPrecisionProp.getLongValue(transaction) : -1;
            long colScale = colScaleProp != null ? colScaleProp.getLongValue(transaction) : -1;
            long colArrDims = colArrDimsProp != null ? colArrDimsProp.getLongValue(transaction) : -1;

            if (getLengthDataTypes().contains(context.getDataTypeName())) {

                if (colLength > -1) {
                    append(OPEN_BRACKET).append(colLength).append(CLOSE_BRACKET);
                }

            } else if (getPrecsionDataTypes().contains(context.getDataTypeName()) &&
                            (colPrecision > -1 || colScale > -1)) {

                append(OPEN_BRACKET).append(colPrecision);
                if (colScale > -1) {
                    append(COMMA).append(colScale);
                }
                append(CLOSE_BRACKET);

            }

            for (long dims = colArrDims; dims > 0; dims--) {
                append(OPEN_SQUARE_BRACKET).append(CLOSE_SQUARE_BRACKET);
            }

            if (context.isNotNull()) {
                append(SPACE).append(NOT_NULL);
            }
        }
    }

    private URI findNamespace(String nsURI) {
        for (URI uri : namespaceMap.values()) {
            if (uri.getUri().equals(nsURI))
                return uri;

            if (uri.getUnbracedURI().equals(nsURI))
                return uri;
        }

        return null;
    }

    private void statementOption(UnitOfWork transaction, KomodoObject stmtOption) throws Exception {
        if (!hasMixinType(transaction, stmtOption, StandardDdlLexicon.TYPE_STATEMENT_OPTION))
            return;

        String key = stmtOption.getName(transaction);
        String value = undefined();

        Property property = property(transaction, stmtOption, StandardDdlLexicon.VALUE);
        if (property != null)
            value = toString(transaction, property);

        if (undefined().equals(value))
            value = NULL;

        String[] keyComponents = key.split(COLON);
        if (keyComponents.length > 1) {
            //
            // This key has a namespace preceding it
            //
            String prefix = keyComponents[0];

            //
            // The prefix represents a modeshape prefix so need the original uri
            // from the modeshape namespace registry
            //
            KObjectFactory objectFactory = RepositoryImpl.getRepository(transaction).getObjectFactory();
            String mURI = objectFactory.getNamespaceURI(transaction, prefix);
            URI uri = null;
            if (mURI != null) {
                //
                // Need to find the ddl prefix for this namespace URI
                // Either a built-in namespace or custom namespace created with a SET NAMESPACE call
                //
                uri = findNamespace(mURI);
            } else {
                uri = namespaceMap.get(prefix);
            }

            if (uri != null)
                key = uri.getPrefix() + COLON + keyComponents[1];

            //
            // If uri == null then the namespace has not been set so
            // the colon is simply part of the name so leave the key intact
            //
        }

        append(escapeOptionKey(key)).append(SPACE);

        // Default to a string value which should be placed in quotes
        append(QUOTE_MARK + value + QUOTE_MARK);
    }

	protected String escapeOptionKey(String key) {
		StringBuilder result = new StringBuilder();

		if (key.length()>1 &&  key.charAt(0) == StringConstants.SPEECH_MARK.charAt(0)
				&& key.charAt(key.length() - 1) == StringConstants.SPEECH_MARK.charAt(0)) {
			result.append(StringConstants.SPEECH_MARK)
				  .append(escapeStringValue(key.substring(1, key.length() - 1), StringConstants.SPEECH_MARK))
			      .append(StringConstants.SPEECH_MARK);
			return result.toString();
		}

		String[] segmentsFromDots = key.split("\\.");
		for (String segment : segmentsFromDots) {
			if (segment.length() == 0) {
				continue;
			}
			result.append(TeiidSqlConstants.Tokens.ID_ESCAPE_CHAR)
				  .append(escapeStringValue(segment, StringConstants.SPEECH_MARK))
				  .append(TeiidSqlConstants.Tokens.ID_ESCAPE_CHAR).append(DOT_CHAR);
		}

		result.setLength(result.length() - 1);
		return result.toString();
	}

    private void statementOptions(UnitOfWork transaction, KomodoObject kObject, String prefix) throws Exception {
        Collection<KomodoObject> options = getChildren(transaction, kObject, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        Iterator<KomodoObject> iterator = options.iterator();

        boolean hasOptions = iterator.hasNext();
        if (! hasOptions)
            return;

        append(prefix).append(OPTIONS).append(SPACE).append(OPEN_BRACKET);

        while(iterator.hasNext()) {
            KomodoObject option = iterator.next();
            statementOption(transaction, option);

            if (iterator.hasNext())
                append(COMMA).append(SPACE);
        }

        append(CLOSE_BRACKET);
    }

    private void tableElement(UnitOfWork transaction, KomodoObject tableElement, CreateObjectContext context) throws Exception {
        if (!hasMixinType(transaction, tableElement, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT))
            return;

        append(NEW_LINE).append(TAB);

        ColumnContext columnContext = createColumnContext(transaction, tableElement);

        if (TableType.GLOBAL_TEMP_TABLE == context.getTableType() && columnContext.isAutoIncremented() &&
            columnContext.isNotNull() &&
            DataTypeName.INTEGER.equals(columnContext.getDataTypeName())) {
            append(escapeSinglePart(tableElement.getName(transaction)));
            append(SPACE);
            append(SERIAL);
        } else {
            column(transaction, tableElement, columnContext, true, true);

            if (columnContext.isAutoIncremented()) {
                append(SPACE).append(AUTO_INCREMENT);
            }
        }

        columnDefault(transaction, tableElement, columnContext);

        // options
        statementOptions(transaction, tableElement, SPACE);
    }

    private KomodoObject referenceByUuid(UnitOfWork transaction, Repository repository, String reference) throws Exception {
        KomodoObject refObject = repository.getUsingId(transaction, reference);
        return refObject;
    }

    private void constraint(UnitOfWork transaction, KomodoObject constraint, String expectedType) throws Exception {
        if (!hasMixinType(transaction, constraint, expectedType))
            return;

        Repository repository = RepositoryImpl.getRepository(transaction);

        append(COMMA).append(NEW_LINE).append(TAB);

        Property typeProp = property(transaction, constraint, TeiidDdlLexicon.Constraint.TYPE);
        append(toString(transaction, typeProp));

        Property refProp = property(transaction, constraint, TeiidDdlLexicon.Constraint.REFERENCES);
        if (refProp != null) {
            List<Object> values = multiPropertyValues(transaction, refProp);

            append(OPEN_BRACKET);

            Iterator<Object> valIter = values.iterator();
            while(valIter.hasNext()) {
                Object refValue = valIter.next();

                KomodoObject keyColumn = referenceByUuid(transaction, repository, refValue.toString());
                append(escapeSinglePart(keyColumn.getName(transaction)));

                if (valIter.hasNext())
                    append(COMMA).append(SPACE);
            }

            append(CLOSE_BRACKET);
        }

        if (TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT.equals(expectedType)) {

            append(SPACE).append(REFERENCES);

            Property tableRefProp = property(transaction, constraint, TeiidDdlLexicon.Constraint.TABLE_REFERENCE);
            KomodoObject tableReference = null;

            if (tableRefProp != null) {
                tableReference = referenceByUuid(transaction, repository, tableRefProp.getStringValue(transaction));
                append(SPACE).append(tableReference == null ? undefined() : tableReference.getName(transaction));
            }

            Property tableRefRefsProp = property(transaction, constraint, TeiidDdlLexicon.Constraint.TABLE_REFERENCE_REFERENCES);
            if (tableRefRefsProp != null) {
                append(SPACE);

                List<Object> tableRefs = multiPropertyValues(transaction, tableRefRefsProp);
                append(OPEN_BRACKET);

                Iterator<Object> valIter = tableRefs.iterator();
                while(valIter.hasNext()) {
                    Object refValue = valIter.next();
                    KomodoObject refColumn = referenceByUuid(transaction, repository, refValue.toString());
                    append(refColumn == null ? undefined() : escapeSinglePart(refColumn.getName(transaction)));

                    if (valIter.hasNext())
                        append(COMMA).append(SPACE);
                }

                append(CLOSE_BRACKET);
            }
        }

        // options
        statementOptions(transaction, constraint, SPACE);
    }

    private void constraints(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        Collection<KomodoObject> teConstraints = getChildren(transaction, kObject, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);
        for (KomodoObject teConstraint : teConstraints) {
            constraint(transaction, teConstraint, TeiidDdlLexicon.Constraint.TABLE_ELEMENT);
        }

        Collection<KomodoObject> indexConstraints = getChildren(transaction, kObject, TeiidDdlLexicon.Constraint.INDEX_CONSTRAINT);
        for (KomodoObject indexConstraint : indexConstraints) {
            constraint(transaction, indexConstraint, TeiidDdlLexicon.Constraint.INDEX_CONSTRAINT);
        }

        Collection<KomodoObject> fkConstraints = getChildren(transaction, kObject, TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT);
        for (KomodoObject fkConstraint : fkConstraints) {
            constraint(transaction, fkConstraint, TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT);
        }
    }

    private void addTableBody(UnitOfWork transaction, KomodoObject kObject, CreateObjectContext context) throws Exception {
        String name = escapeSinglePart(kObject.getName(transaction));
        append(name);

        Collection<KomodoObject> tableElements = getChildren(transaction, kObject, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        if (! tableElements.isEmpty()) {
            append(SPACE);
            append(OPEN_BRACKET);

            Iterator<KomodoObject> iterator = tableElements.iterator();
            while(iterator.hasNext()) {
                KomodoObject tableElement = iterator.next();
                tableElement(transaction, tableElement, context);

                if (iterator.hasNext())
                    append(COMMA);
            }

            if(includeTableConstraints) {
                constraints(transaction, kObject);
            }
            append(NEW_LINE);
            append(CLOSE_BRACKET);
        }

        // options
        statementOptions(transaction, kObject, SPACE);
    }

    private String schemaElementType(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        Property property = property(transaction, kObject, TeiidDdlLexicon.SchemaElement.TYPE);
        if (property == null)
            return null;

        return toString(transaction, property);
    }

    private void tabulation(UnitOfWork transaction, KomodoObject tabulation, CreateObjectContext context) throws Exception {
        append(SPACE);

        addTableBody(transaction, tabulation, context);

        if (TableType.GLOBAL_TEMP_TABLE != context.getTableType()) {
            if (context.isVirtual()) {
            	Property p = tabulation.getProperty(transaction, TeiidDdlLexicon.CreateTable.QUERY_EXPRESSION);
                String teiidSql = p.getStringValue(transaction); 
                append(NEW_LINE).append(AS).append(NEW_LINE).append(teiidSql);
            }
            append(SEMI_COLON);
        }
    }

    private void table(UnitOfWork transaction, KomodoObject table) throws Exception {
        if (! includeTables)
            return;

        if (!hasMixinType(transaction, table, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT))
            return;

        append(NEW_LINE);

        CreateObjectContext context = new CreateObjectContext();
        context.setPhysical(FOREIGN.equals(schemaElementType(transaction, table)));

        append(CREATE).append(SPACE);

        if (context.isPhysical()) {
            context.setTableType(TableType.TABLE);
            append(FOREIGN).append(SPACE).append(TABLE);
        }
        else {
            context.setTableType(TableType.GLOBAL_TEMP_TABLE);
            append(GLOBAL).append(SPACE).append(TEMPORARY).append(SPACE).append(TABLE);
        }

        tabulation(transaction, table, context);
    }

    private void view(UnitOfWork transaction, KomodoObject view) throws Exception {
        if (! includeTables)
            return;

        if (!hasMixinType(transaction, view, TeiidDdlLexicon.CreateTable.VIEW_STATEMENT))
            return;

        append(NEW_LINE);

        CreateObjectContext context = new CreateObjectContext();
        context.setVirtual(true);
        context.setTableType(TableType.VIEW);

        append(CREATE).append(SPACE).append(VIEW);

        tabulation(transaction, view, context);
    }

    private void procedureParameter(UnitOfWork transaction, KomodoObject parameter) throws Exception {
        if (!hasMixinType(transaction, parameter, TeiidDdlLexicon.CreateProcedure.PARAMETER))
            return;

        Property typeProp = property(transaction, parameter, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE);
        String paramType = toString(transaction, typeProp);
        append(paramType).append(SPACE);

        ColumnContext columnContext = createColumnContext(transaction, parameter);
        column(transaction, parameter, columnContext, true, true);

        Property returnProp = property(transaction, parameter, TeiidDdlLexicon.CreateProcedure.PARAMETER_RESULT_FLAG);
        boolean returnFlag = returnProp == null ? false : returnProp.getBooleanValue(transaction);
        if (returnFlag) {
            append(SPACE).append(NonReserved.RESULT);
        }

        columnDefault(transaction, parameter, columnContext);

        // Options
        statementOptions(transaction, parameter, SPACE);
    }

    private void procedureParameters(UnitOfWork transaction, KomodoObject procedure) throws Exception {
        Collection<KomodoObject> parameters = getChildren(transaction, procedure, TeiidDdlLexicon.CreateProcedure.PARAMETER);
        Iterator<KomodoObject> paramIter = parameters.iterator();
        while(paramIter.hasNext()) {
            KomodoObject parameter = paramIter.next();

            procedureParameter(transaction, parameter);

            if (paramIter.hasNext())
                append(COMMA).append(SPACE);
        }
    }

    private void procedure(UnitOfWork transaction, KomodoObject procedure) throws Exception {
        if (! includeProcedures)
            return;

        if (!hasMixinType(transaction, procedure, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT))
            return;

        append(NEW_LINE);

        CreateObjectContext context = new CreateObjectContext();
        context.setPhysical(FOREIGN.equals(schemaElementType(transaction, procedure)));

        append(CREATE).append(SPACE);

        if (context.isVirtual())
            append(VIRTUAL);
        else
            append(FOREIGN);

        append(SPACE).append(PROCEDURE).append(SPACE).append(escapeSinglePart(procedure.getName(transaction)));
        append(OPEN_BRACKET);
        procedureParameters(transaction, procedure);
        append(CLOSE_BRACKET);

        boolean hasResultSet = procedure.hasChild(transaction, TeiidDdlLexicon.CreateProcedure.RESULT_SET);
        if(hasResultSet) {
            KomodoObject resultSet = procedure.getChild(transaction, TeiidDdlLexicon.CreateProcedure.RESULT_SET);
            // Handle DataType result
            if (hasMixinType(transaction, resultSet, TeiidDdlLexicon.CreateProcedure.RESULT_DATA_TYPE)) {
                ColumnContext columnContext = createColumnContext(transaction, resultSet);
                append(SPACE).append(RETURNS).append(SPACE).
                append(columnContext.getDataType());
            } else {
                append(SPACE).append(RETURNS).append(SPACE).append(TABLE).append(SPACE);

                append(OPEN_BRACKET);
                Collection<KomodoObject> resultColumns = getChildren(transaction, resultSet, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);
                Iterator<KomodoObject> iterator = resultColumns.iterator();
                while (iterator.hasNext()) {
                    KomodoObject resultColumn = iterator.next();
                    ColumnContext columnContext = createColumnContext(transaction, resultColumn);
                    column(transaction, resultColumn, columnContext, true, true);

                    if (iterator.hasNext())
                        append(COMMA).append(SPACE);
                }
                append(CLOSE_BRACKET);
            }
        }

        //options
        statementOptions(transaction, procedure, NEW_LINE);

        //block
        if (context.isVirtual()) {
            append(NEW_LINE).append(AS).append(NEW_LINE);
            Property p = procedure.getProperty(transaction, TeiidDdlLexicon.CreateProcedure.STATEMENT);
            String teiidSql = p.getStringValue(transaction);
            append(teiidSql);
            append(SEMI_COLON);
        }
    }

    private void functionParameter(UnitOfWork transaction, KomodoObject functionParameter) throws Exception {
        Property typeProp = property(transaction, functionParameter, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE);
        String paramType = toString(transaction, typeProp);

        if (VARIADIC.equals(paramType))
            append(VARIADIC).append(SPACE);

        ColumnContext columnContext = createColumnContext(transaction, functionParameter);
        column(transaction, functionParameter, columnContext, true, true);
    }

    private void functionParameters(UnitOfWork transaction, KomodoObject function) throws Exception {
        Collection<KomodoObject> parameters = getChildren(transaction, function, TeiidDdlLexicon.CreateProcedure.PARAMETER);
        Iterator<KomodoObject> paramIter = parameters.iterator();
        while(paramIter.hasNext()) {
            KomodoObject parameter = paramIter.next();

            functionParameter(transaction, parameter);

            if (paramIter.hasNext())
                append(COMMA).append(SPACE);
        }
    }

    private void function(UnitOfWork transaction, KomodoObject function) throws Exception {
        if (! includeFunctions)
            return;

        if (!hasMixinType(transaction, function, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT))
            return;

        append(CREATE).append(SPACE);

        CreateObjectContext context = new CreateObjectContext();
        context.setPhysical(FOREIGN.equals(schemaElementType(transaction, function)));

        if (context.isPhysical())
            append(FOREIGN);
        else
            append(VIRTUAL);

        append(SPACE).append(FUNCTION).append(SPACE).append(escapeSinglePart(function.getName(transaction)));

        append(OPEN_BRACKET);
        functionParameters(transaction, function);
        append(CLOSE_BRACKET);

        KomodoObject resultSet = function.getChild(transaction, TeiidDdlLexicon.CreateProcedure.RESULT_SET);
        if (resultSet != null) {
            ColumnContext columnContext = createColumnContext(transaction, resultSet);
            append(SPACE).append(RETURNS).append(SPACE).
            append(columnContext.getDataType());
        }

        //options
        statementOptions(transaction, function, NEW_LINE);
        append(SEMI_COLON);
    }

    @Override
    public Object visit(UnitOfWork transaction, KomodoObject kObject) throws KException {
        if (kObject == null)
            return null;

        try {
            //
            // Teiid DDL KomodoObjects
            //
            String tddlMixinType = findMixinTypeByNamespace(transaction, kObject, TeiidDdlLexicon.Namespace.PREFIX);
            MixinTypeName typeName = MixinTypeName.findName(tddlMixinType);
            switch (typeName) {
                case CREATE_TABLE:
                    table(transaction, kObject);
                    append(NEW_LINE);
                    break;
                case CREATE_VIEW:
                    view(transaction, kObject);
                    append(NEW_LINE);
                    break;
                case OPTION_NAMESPACE:
                    optionNamespace(transaction, kObject);
                    append(NEW_LINE);
                    break;
                case CREATE_PROCEDURE:
                    procedure(transaction, kObject);
                    append(NEW_LINE);
                    break;
                case CREATE_FUNCTION:
                    function(transaction, kObject);
                    append(NEW_LINE);
                    break;
                case UNKNOWN:
                default:
                    // Not a kObject we are interested in but may contain such kObjects
                    visitChildren(transaction, kObject);
            }

            return null;
        } catch (Exception ex) {
            throw new KException(ex);
        }
    }

    @Override
    public OperationType getRequestType() {
        return OperationType.READ_OPERATION;
    }
}
