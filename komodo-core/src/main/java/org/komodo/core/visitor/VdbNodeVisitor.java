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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.lexicon.LexiconConstants.CoreLexicon;
import org.komodo.spi.lexicon.LexiconConstants.JcrLexicon;
import org.komodo.spi.lexicon.LexiconConstants.ModeshapeLexicon;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KObjectFactory;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.OperationType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.MetadataVersion;
import org.komodo.spi.type.DataTypeService;

/**
 * Visitor that will walk a vdb kObject tree and convert it to
 * the dynamic vdb xml syntax.
 */
public class VdbNodeVisitor extends AbstractNodeVisitor implements StringConstants {

    /**
     * Companion XML tag for permission condition
     */
    public static String DATA_ROLE_PERMISSION_CONDITION_XML = "condition"; //$NON-NLS-1$

    private static String UNDEFINED = null;

    private static Map<String, NodeTypeName> kObjectNameIndex = new HashMap<String, NodeTypeName>();

    private enum NodeTypeName {

        VIRTUAL_DATABASE(VdbLexicon.Vdb.VIRTUAL_DATABASE, VdbLexicon.ManifestIds.VDB),

        DESCRIPTION(VdbLexicon.Vdb.DESCRIPTION, VdbLexicon.ManifestIds.DESCRIPTION),

        IMPORT_VDBS(VdbLexicon.Vdb.IMPORT_VDBS),

        IMPORT_VDB(VdbLexicon.ImportVdb.IMPORT_VDB, VdbLexicon.ManifestIds.IMPORT_VDB),

        MODEL(VdbLexicon.Vdb.DECLARATIVE_MODEL, VdbLexicon.ManifestIds.MODEL),

        SOURCES(VdbLexicon.Vdb.SOURCES),

        SOURCE(VdbLexicon.Source.SOURCE, VdbLexicon.ManifestIds.SOURCE),

        TRANSLATORS(VdbLexicon.Vdb.TRANSLATORS),

        TRANSLATOR(VdbLexicon.Translator.TRANSLATOR, VdbLexicon.ManifestIds.TRANSLATOR),

        DATA_ROLES(VdbLexicon.Vdb.DATA_ROLES),

        DATA_ROLE(VdbLexicon.DataRole.DATA_ROLE, VdbLexicon.ManifestIds.DATA_ROLE),

        PERMISSIONS(VdbLexicon.DataRole.PERMISSIONS),

        PERMISSION(VdbLexicon.DataRole.Permission.PERMISSION, VdbLexicon.ManifestIds.PERMISSION),

        CONDITIONS(VdbLexicon.DataRole.Permission.CONDITIONS),

        CONDITION(VdbLexicon.DataRole.Permission.Condition.CONDITION, VdbLexicon.ManifestIds.CONDITION),

        MASKS(VdbLexicon.DataRole.Permission.MASKS),

        MASK(VdbLexicon.DataRole.Permission.Mask.MASK, VdbLexicon.ManifestIds.MASK),

        UNKNOWN(UNDEFINED);

        private String id;

        private String tag;

        private NodeTypeName(String id, String tag) {
            this.id = id;
            this.tag = tag;
            kObjectNameIndex.put(this.id, this);
        }

        private NodeTypeName(String name) {
            this(name, UNDEFINED);
        }

        /**
         * @return the name
         */
        public String getId() {
            return this.id;
        }

        /**
         * @return the tag
         */
        public String getTag() {
            return this.tag;
        }

        /**
         * @param name name value
         * @return the name enum for the given name
         */
        public static NodeTypeName findName(String name) {
            NodeTypeName ntName = kObjectNameIndex.get(name);
            if (ntName != null)
                return ntName;

            return UNKNOWN;
        }
    }

    @SuppressWarnings( "unused" )
    private interface ElementTabValue {
        int VIRTUAL_DATABASE = 0;
        int VDB_PROPERTY = 1;
        int DESCRIPTION = 1;
        int CONNECTION_TYPE = 1;
        int IMPORT_VDB = 1;

        int MODEL = 1;
        int MODEL_PROPERTY = 2;
        int MODEL_DESCRIPTION = 2;
        int MODEL_METADATA = 2;
        int MODEL_VALIDATION = 2;
        int MODEL_SOURCE = 2;

        int TRANSLATOR = 1;
        int TRANSLATOR_PROPERTY = 2;

        int DATA_ROLE = 1;
        int DATA_ROLE_DESCRIPTION = 2;
        int PERMISSION = 2;
        int MAPPED_ROLE_NAME = 2;
        int RESOURCE_NAME = 3;
        int PERMISSION_ALLOW = 3;
        int CONDITION = 3;
        int MASK = 3;

        int ENTRY = 1;
        int ENTRY_PROPERTY = 2;
        int ENTRY_DESCRIPTION = 2;
    }

    private XMLStreamWriter writer;

    private boolean showTabs;

    /**
     * Create new visitor that writes to the given xml stream writer
     *
     * @param version teiid version
     * @param dataTypeService the data type service
     * @param writer output for the xml
     */
    public VdbNodeVisitor(MetadataVersion version, DataTypeService dataTypeService, XMLStreamWriter writer) {
        super(version, dataTypeService);
        this.writer = writer;
    }

    @Override
    protected String undefined() {
        return UNDEFINED;
    }

    /**
     * Set to true to show tabs
     *
     * @param showTabs showTabs flags
     */
    public void setShowTabs(boolean showTabs) {
        this.showTabs = showTabs;
    }

    private void writeNewLine(int total) throws XMLStreamException {
        for (int i = 0; i < total; ++i)
            writer.writeCharacters(NEW_LINE);
    }

    private void writeNewLine() throws XMLStreamException {
        writeNewLine(1);
    }

    private void writeTab(int total) throws XMLStreamException {
        if (showTabs) {
            for (int i = 0; i < total; ++i)
                writer.writeCharacters(TAB);
        }
    }

    private void writeStartDocument() throws XMLStreamException {
        writer.writeStartDocument("UTF-8", "1.0"); //$NON-NLS-1$ //$NON-NLS-2$
        writeNewLine();
    }

    private void writeStartElement(String tag) throws XMLStreamException {
        writer.writeStartElement(tag);
    }

    private void writeAttribute(String name, String value) throws XMLStreamException {
        if (value != null)
            writer.writeAttribute(name, value);
    }

    private void writeCData(String data) throws XMLStreamException {
        writer.writeCData(data);
    }

    private void writeCharacters(String characters) throws XMLStreamException {
        writer.writeCharacters(characters);
    }

    private void writeEndElement() throws XMLStreamException {
        writer.writeEndElement();
        writeNewLine();
    }

    private void writeElementWithText(String name, String text) throws XMLStreamException {
        writeStartElement(name);
        writeCharacters(text);
        writeEndElement();
    }

    private void writeEndDocument() throws XMLStreamException {
        writer.writeEndDocument();
        writer.close();
    }

    private boolean isPrimaryNodeType(UnitOfWork transaction, KomodoObject kObject, NodeTypeName kObjectTypeName)
        throws Exception {
        Descriptor kObjectType = kObject.getPrimaryType(transaction);
        return kObjectTypeName.getId().equals(kObjectType.getName());
    }

    private void properties(UnitOfWork transaction, KomodoObject kObject, int numTabs, Properties exportableProps)
        throws XMLStreamException {

        for (Object key : exportableProps.keySet()) {
            String name = (String)key;
            String value = exportableProps.getProperty(name);

            writeTab(numTabs);
            writeStartElement(VdbLexicon.ManifestIds.PROPERTY);
            writeAttribute(VdbLexicon.ManifestIds.NAME, name);
            writeAttribute(VdbLexicon.ManifestIds.VALUE, value);
            writeEndElement();
        }
    }

    private void mask(UnitOfWork transaction, KomodoObject kObject) throws XMLStreamException, Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.MASK))
            return;

        writeTab(ElementTabValue.MASK);
        // Condition element
        writeStartElement(NodeTypeName.MASK.getTag());

        Property property = property(transaction, kObject, VdbLexicon.DataRole.Permission.Mask.ORDER);
        writeAttribute(VdbLexicon.ManifestIds.ORDER, toString(transaction, property));

        writeCharacters(kObject.getName(transaction));
        writeEndElement();
    }

    private void condition(UnitOfWork transaction, KomodoObject kObject) throws XMLStreamException, Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.CONDITION))
            return;

        // Condition element
        writeTab(ElementTabValue.CONDITION);
        writeStartElement(NodeTypeName.CONDITION.getTag());

        Property property = property(transaction, kObject, VdbLexicon.DataRole.Permission.Condition.CONSTRAINT);
        writeAttribute(VdbLexicon.ManifestIds.CONSTRAINT, toString(transaction, property));

        writeCharacters(kObject.getName(transaction));
        writeEndElement();
    }

    private void permission(UnitOfWork transaction, KomodoObject kObject) throws XMLStreamException, Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.PERMISSION))
            return;

        // Permission element
        //        writeNewLine();
        writeTab(ElementTabValue.PERMISSION);
        writeStartElement(NodeTypeName.PERMISSION.getTag());

        // Resource name element
        writeNewLine();
        writeTab(ElementTabValue.RESOURCE_NAME);
        writeElementWithText(VdbLexicon.ManifestIds.RESOURCE_NAME, kObject.getName(transaction));

        String[][] permTags = {
            {VdbLexicon.DataRole.Permission.ALLOW_CREATE, VdbLexicon.ManifestIds.ALLOW_CREATE},
            {VdbLexicon.DataRole.Permission.ALLOW_READ, VdbLexicon.ManifestIds.ALLOW_READ},
            {VdbLexicon.DataRole.Permission.ALLOW_UPDATE, VdbLexicon.ManifestIds.ALLOW_UPDATE},
            {VdbLexicon.DataRole.Permission.ALLOW_DELETE, VdbLexicon.ManifestIds.ALLOW_DELETE},
            {VdbLexicon.DataRole.Permission.ALLOW_EXECUTE, VdbLexicon.ManifestIds.ALLOW_EXECUTE},
            {VdbLexicon.DataRole.Permission.ALLOW_ALTER, VdbLexicon.ManifestIds.ALLOW_ALTER},
            {VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE, VdbLexicon.ManifestIds.ALLOW_LANGUAGE}};

        for (int i = 0; i < permTags.length; ++i) {
            Property permProp = property(transaction, kObject, permTags[i][0]);

            // Don't include allow-language if not present or set to false as when this was present queries were not working
            // in the default read-only data role. Might need a 3-state value (true, false, not set) for this property.
            // See TEIIDTOOLS-224
            if (VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE.equals(permTags[i][0])
                && ((permProp == null) || !permProp.getBooleanValue(transaction))) {
                continue;
            }

            Boolean value = permProp == null ? false : permProp.getBooleanValue(transaction);
            writeTab(ElementTabValue.PERMISSION_ALLOW);
            writeElementWithText(permTags[i][1], value.toString());
        }

        // Conditions
        visitChild(transaction, kObject, NodeTypeName.CONDITIONS.getId());

        // Masks
        visitChild(transaction, kObject, NodeTypeName.MASKS.getId());

        // End Permission
        writeTab(ElementTabValue.PERMISSION);
        writeEndElement();
    }

    private void dataRole(UnitOfWork transaction, KomodoObject kObject) throws XMLStreamException, Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.DATA_ROLE))
            return;

        // Data Role element
        writeTab(ElementTabValue.DATA_ROLE);
        writeStartElement(NodeTypeName.DATA_ROLE.getTag());

        // Process data role attributes
        String nameProp = kObject.getName(transaction);
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property authProp = property(transaction, kObject, VdbLexicon.DataRole.ANY_AUTHENTICATED);
        writeAttribute(VdbLexicon.ManifestIds.ANY_AUTHENTICATED, toString(transaction, authProp));
        Property tempTablesProp = property(transaction, kObject, VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES);
        writeAttribute(VdbLexicon.ManifestIds.ALLOW_CREATE_TEMP_TABLES, toString(transaction, tempTablesProp));
        Property grantAllProp = property(transaction, kObject, VdbLexicon.DataRole.GRANT_ALL);
        writeAttribute(VdbLexicon.ManifestIds.GRANT_ALL, toString(transaction, grantAllProp));

        writeNewLine();

        description(transaction, kObject, ElementTabValue.DATA_ROLE_DESCRIPTION);

        // Permissions
        visitChild(transaction, kObject, NodeTypeName.PERMISSIONS.getId());

        // Mapped Role Names
        if (kObject.hasProperty(transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES)) {
            Property property = kObject.getProperty(transaction, VdbLexicon.DataRole.MAPPED_ROLE_NAMES);
            Object[] mappedRoleValues = property.getValues(transaction);
            for (Object value : mappedRoleValues) {
                writeTab(ElementTabValue.MAPPED_ROLE_NAME);
                writeElementWithText(VdbLexicon.ManifestIds.MAPPED_ROLE_NAME, value.toString());
            }
        }

        writeTab(ElementTabValue.DATA_ROLE);
        writeEndElement();
    }

    private void translator(UnitOfWork transaction, KomodoObject kObject) throws XMLStreamException, Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.TRANSLATOR))
            return;

        // Translator element
        writeTab(ElementTabValue.TRANSLATOR);
        writeStartElement(NodeTypeName.TRANSLATOR.getTag());

        // Process translator attributes
        String nameProp = kObject.getName(transaction);
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property typeProp = property(transaction, kObject, VdbLexicon.Translator.TYPE);
        writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(transaction, typeProp));
        Property descProp = property(transaction, kObject, VdbLexicon.Translator.DESCRIPTION);
        writeAttribute(VdbLexicon.ManifestIds.DESCRIPTION, toString(transaction, descProp));

        writeNewLine();

        // Process property attributes
        Properties exportableProps = filterExportableProperties(transaction,
                                                                kObject,
                                                                VdbLexicon.Translator.TYPE,
                                                                VdbLexicon.Translator.DESCRIPTION);
        properties(transaction, kObject, ElementTabValue.TRANSLATOR_PROPERTY, exportableProps);

        writeTab(ElementTabValue.TRANSLATOR);
        writeEndElement();
    }

    private void source(UnitOfWork transaction, KomodoObject kObject) throws XMLStreamException, Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.SOURCE))
            return;

        // Translator element
        writeTab(ElementTabValue.MODEL_SOURCE);
        writeStartElement(NodeTypeName.SOURCE.getTag());

        // Process source attributes
        String nameProp = kObject.getName(transaction);
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property translatorProp = property(transaction, kObject, VdbLexicon.Source.TRANSLATOR);
        writeAttribute(VdbLexicon.ManifestIds.TRANSLATOR_NAME, toString(transaction, translatorProp));
        Property jndiProp = property(transaction, kObject, VdbLexicon.Source.JNDI_NAME);
        writeAttribute(VdbLexicon.ManifestIds.JNDI_NAME, toString(transaction, jndiProp));

        writeEndElement();
    }

    /**
     * Due to properties not being allowed in the sources element, sources' association connections
     * properties must be added as properties to their parent model.
     *
     * Since the xml writer runs in order this must be added prior to the visiting of the sources and
     * does not conform to the visitor pattern. Assuming that properties can be added to the source
     * element in the future this can revert to being in the source method.
     *
     * @param transaction
     * @param model
     * @return
     * @throws Exception
     */
    private Properties sourceConnections(UnitOfWork transaction, KomodoObject model) throws Exception {
        Properties props = new Properties();
    
        if (! model.hasChild(transaction, NodeTypeName.SOURCES.getId()))
            return props;

        KomodoObject sources = model.getChild(transaction, NodeTypeName.SOURCES.getId());
        for (KomodoObject source : sources.getChildrenOfType(transaction, NodeTypeName.SOURCE.getId())) {
            if (! source.hasProperty(transaction, VdbLexicon.Source.ORIGIN_CONNECTION))
                continue;

            Property connProperty = source.getProperty(transaction, VdbLexicon.Source.ORIGIN_CONNECTION);
            String connName = connProperty.getStringValue(transaction);
            if (connName == null)
                continue;

            /*
             * Add a property to the model that specifies the association
             * between the named source and the named connection
             */
            props.setProperty(VdbLexicon.ManifestIds.ORIGIN_SRC_CONNECTION + "-" + source.getName(transaction), connName);
        }

        return props;
    }

    private void model(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.MODEL))
            return;

        writeTab(ElementTabValue.MODEL);
        writeStartElement(NodeTypeName.MODEL.getTag());

        writeAttribute(VdbLexicon.ManifestIds.NAME, kObject.getName(transaction));

        Property typeProp = property(transaction, kObject, CoreLexicon.MODEL_TYPE);
        writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(transaction, typeProp));

        Property pathProp = property(transaction, kObject, VdbLexicon.Model.PATH_IN_VDB);
        if (pathProp != null)
            writeAttribute(VdbLexicon.ManifestIds.PATH, toString(transaction, pathProp));

        Property visibleProp = property(transaction, kObject, VdbLexicon.Model.VISIBLE);
        if (visibleProp != null) {
            String value = toString(transaction, visibleProp);
            if (!Boolean.parseBoolean(value)) // True is the default value so no need to include if true
                writeAttribute(VdbLexicon.ManifestIds.VISIBLE, value);
        }

        writeNewLine();
        description(transaction, kObject, ElementTabValue.MODEL_DESCRIPTION);

        Properties exportableProps = filterExportableProperties(transaction, kObject, CoreLexicon.MODEL_TYPE);

        // Find sources associated connections and add them to the model's exportable properties
        Properties assocConnProps = sourceConnections(transaction, kObject);
        exportableProps.putAll(assocConnProps);

        properties(transaction, kObject, ElementTabValue.MODEL_PROPERTY, exportableProps);

        // Sources
        visitFilteredChildren(transaction, kObject, NodeTypeName.SOURCES.getId());

        DdlNodeVisitor visitor = new DdlNodeVisitor(getVersion(), getDataTypeService(), showTabs);
        visitor.visit(transaction, kObject);

        if (!visitor.getDdl().isEmpty()) {
            writeTab(ElementTabValue.MODEL_METADATA);
            writeStartElement(VdbLexicon.ManifestIds.METADATA);
            Property metaTypeProp = property(transaction, kObject, VdbLexicon.Model.METADATA_TYPE);
            writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(transaction, metaTypeProp));

            writeNewLine();
            writeTab(ElementTabValue.MODEL_METADATA + 1);
            writeCData(visitor.getDdl());

            // end metadata tag
            writeNewLine();
            writeTab(ElementTabValue.MODEL_METADATA + 1);
            writeNewLine();
            writeTab(ElementTabValue.MODEL_METADATA);
            writeEndElement();
        }

        // End model tag
        writeTab(ElementTabValue.MODEL);
        writeEndElement();
    }

    private void importVdb(UnitOfWork transaction, KomodoObject kObject) throws XMLStreamException, Exception {
        if (!isPrimaryNodeType(transaction, kObject, NodeTypeName.IMPORT_VDB))
            return;

        // Import-vdb element
        writeTab(ElementTabValue.IMPORT_VDB);
        writeStartElement(NodeTypeName.IMPORT_VDB.getTag());

        // Process import-vdb attributes
        String nameProp = kObject.getName(transaction);
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property versionProp = property(transaction, kObject, VdbLexicon.ImportVdb.VERSION);
        writeAttribute(VdbLexicon.ManifestIds.VERSION, toString(transaction, versionProp));
        Property dataPoliciesProp = property(transaction, kObject, VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);
        if (dataPoliciesProp != null)
            writeAttribute(VdbLexicon.ManifestIds.IMPORT_DATA_POLICIES, toString(transaction, dataPoliciesProp));

        writeTab(ElementTabValue.IMPORT_VDB);
        writeEndElement();
    }

    private void description(UnitOfWork transaction, KomodoObject kObject, int numTabs) throws Exception {
        Property property;
        property = property(transaction, kObject, NodeTypeName.DESCRIPTION.getId());
        if (property == null)
            return;

        writeTab(numTabs);
        writeElementWithText(NodeTypeName.DESCRIPTION.getTag(), toString(transaction, property));
    }

    private Properties filterExportableProperties(UnitOfWork transaction, KomodoObject kObject, String... propertiesToIgnore)
        throws Exception {
        Properties exportableProps = new Properties();
        List<String> propsToIgnore = Arrays.asList(propertiesToIgnore);

        String[] propertyNames = kObject.getRawPropertyNames(transaction);
        List<Property> properties = new ArrayList<>();
        for (String propName : propertyNames) {
            if (! kObject.hasRawProperty(transaction, propName))
                continue;

            Property property = kObject.getRawProperty(transaction, propName);
            properties.add(property);
        }

        for (Property property : properties) {
            String name = property.getName(transaction);
            if (name == null)
                continue;

            if (propsToIgnore.contains(name))
                continue;

            // Ignore jcr properties since these are internal to modeshape
            if (name.startsWith(JcrLexicon.Namespace.PREFIX))
                continue;

            String value = toString(transaction, property);

            //
            // Ignore modeshape vdb properties as <property> type properties will
            // not have a vdb prefix but simply be the property name on its own, eg.
            // UseConnectedMetadata or vdb-property1.
            //
            if (name.startsWith(VdbLexicon.Namespace.PREFIX + COLON)) {
                //
                // Preview is actually converted into a vdb property so need to special-case
                // turn it back into a simple property name but we only care if the property
                // is actually true.
                //
                if (name.equals(VdbLexicon.Vdb.PREVIEW) && Boolean.parseBoolean(value)) {
                    name = VdbLexicon.ManifestIds.PREVIEW;
                } else {
                    continue;
                }
            }

            name = convertNamePrefixToUri(transaction, property);
            exportableProps.put(name, value);
        }

        return exportableProps;
    }

    private String convertNamePrefixToUri(UnitOfWork transaction, Property property) throws Exception {
        String name = property.getName(transaction);
        int index = name.indexOf( COLON );

        // if JCR expanded name or just a local name just return the name
        if ( index == -1 ) {
            return name;
        }

        // convert JCR qualified name to expanded name
        String prefix = name.substring(0, index);

        KObjectFactory objectFactory = property.getRepository().getObjectFactory();
        String uri = objectFactory.getNamespaceURI(transaction, prefix) ;
        QName expanded = new QName( uri, name.substring( index + 1 ) );

        return expanded.toString();
    }
    private void virtualDatabase(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        // Start new document
        writeStartDocument();

        // Vdb element
        writeTab(ElementTabValue.VIRTUAL_DATABASE);
        writeStartElement(NodeTypeName.VIRTUAL_DATABASE.getTag());

        // Name attribute
        Property property = property(transaction, kObject, VdbLexicon.Vdb.NAME);
        writeAttribute(VdbLexicon.ManifestIds.NAME, toString(transaction, property));

        // Version attribute
        property = property(transaction, kObject, VdbLexicon.Vdb.VERSION);
        writeAttribute(VdbLexicon.ManifestIds.VERSION, toString(transaction, property));

        writeNewLine(2);

        // Description element
        description(transaction, kObject, ElementTabValue.DESCRIPTION);

        // Connection Type element
        property = property(transaction, kObject, VdbLexicon.Vdb.CONNECTION_TYPE);
        if (property != null) {
            writeTab(ElementTabValue.CONNECTION_TYPE);
            writeElementWithText(VdbLexicon.ManifestIds.CONNECTION_TYPE, toString(transaction, property));
        }

        // Properties elements
        Properties exportableProps = filterExportableProperties(transaction,
                                                                kObject,
                                                                VdbLexicon.Vdb.NAME,
                                                                VdbLexicon.Vdb.VERSION,
                                                                NodeTypeName.DESCRIPTION.getId(),
                                                                VdbLexicon.Vdb.CONNECTION_TYPE,
                                                                ModeshapeLexicon.MODE_SHA1);

        properties(transaction, kObject, ElementTabValue.VDB_PROPERTY, exportableProps);

        writeNewLine();

        //
        // Visit vdb children by name since the xsd demands them in a specific order
        //

        // Import Vdbs
        visitFilteredChildren(transaction, kObject, NodeTypeName.IMPORT_VDBS.getId());

        // Models
        visitFilteredChildren(transaction, kObject, NodeTypeName.MODEL.getId());

        // Translators
        visitFilteredChildren(transaction, kObject, NodeTypeName.TRANSLATORS.getId());

        // Data Roles
        visitFilteredChildren(transaction, kObject, NodeTypeName.DATA_ROLES.getId());

        writeNewLine();

        // Close out the xml document
        writeTab(ElementTabValue.VIRTUAL_DATABASE);
        writeEndElement();
        writeEndDocument();
    }

    @Override
    public OperationType getRequestType() {
        return OperationType.READ_OPERATION;
    }

    @Override
    public Object visit(UnitOfWork transaction, KomodoObject kObject) throws KException {
        if (kObject == null)
            return null;

        Descriptor primaryType = kObject.getPrimaryType(transaction);
        String kObjectTypeName = primaryType.getName();
        NodeTypeName ntName = NodeTypeName.findName(kObjectTypeName);
        try {
            switch (ntName) {
                case VIRTUAL_DATABASE:
                    virtualDatabase(transaction, kObject);
                    break;
                case IMPORT_VDBS:
                case TRANSLATORS:
                case SOURCES:
                case DATA_ROLES:
                case PERMISSIONS:
                case CONDITIONS:
                case MASKS:
                    visitChildren(transaction, kObject);
                    break;
                case MODEL:
                    model(transaction, kObject);
                    break;
                case IMPORT_VDB:
                    importVdb(transaction, kObject);
                    break;
                case TRANSLATOR:
                    translator(transaction, kObject);
                    break;
                case SOURCE:
                    source(transaction, kObject);
                    break;
                case DATA_ROLE:
                    dataRole(transaction, kObject);
                    break;
                case PERMISSION:
                    permission(transaction, kObject);
                    break;
                case CONDITION:
                    condition(transaction, kObject);
                    break;
                case MASK:
                    mask(transaction, kObject);
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
    protected void visitChild(UnitOfWork transaction, KomodoObject kObject, String relNodePath) throws Exception {
        try {
            if (kObject.hasRawChild(transaction, relNodePath)) {
                // write tab value based on kObject path/type
                writeTab(getTabValue(relNodePath));

                KomodoObject child = kObject.getChild(transaction, relNodePath);
                child.accept(transaction, this);
            }
        } catch (XMLStreamException ex) {
            throw new Exception(ex);
        }
    }

    private int getTabValue(String relNodePath) {
        if (relNodePath.equals(NodeTypeName.IMPORT_VDB.getId())) {
            return ElementTabValue.IMPORT_VDB;
        } else if (relNodePath.equals(NodeTypeName.CONDITION.getId())) {
            return ElementTabValue.CONDITION;
        } else if (relNodePath.equals(NodeTypeName.DATA_ROLE.getId())) {
            return ElementTabValue.DATA_ROLE;
        } else if (relNodePath.equals(NodeTypeName.DESCRIPTION.getId())) {
            return ElementTabValue.DESCRIPTION;
        } else if (relNodePath.equals(NodeTypeName.MASK.getId())) {
            return ElementTabValue.MASK;
        } else if (relNodePath.equals(NodeTypeName.MODEL.getId())) {
            return ElementTabValue.MODEL;
        } else if (relNodePath.equals(NodeTypeName.PERMISSION.getId())) {
            return ElementTabValue.PERMISSION;
        } else if (relNodePath.equals(NodeTypeName.SOURCE.getId())) {
            return ElementTabValue.MODEL_SOURCE;
        } else if (relNodePath.equals(NodeTypeName.TRANSLATOR.getId())) {
            return ElementTabValue.TRANSLATOR;
        } else if (relNodePath.equals(NodeTypeName.VIRTUAL_DATABASE.getId())) {
            return ElementTabValue.VIRTUAL_DATABASE;
        }

        return 0;
    }
}
