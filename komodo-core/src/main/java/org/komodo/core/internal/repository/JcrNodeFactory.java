/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.transactiont file distributed with this work for information
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
package org.komodo.core.internal.repository;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.PropertyIterator;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.nodetype.NodeType;
import javax.jcr.nodetype.NodeTypeManager;
import javax.jcr.nodetype.PropertyDefinition;
import javax.jcr.query.Query;
import javax.jcr.query.QueryResult;
import org.komodo.core.repository.DescriptorImpl;
import org.komodo.core.repository.Messages;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyDescriptorImpl;
import org.komodo.core.repository.PropertyImpl;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.LexiconConstants.JcrLexicon;
import org.komodo.spi.lexicon.LexiconConstants.NTLexicon;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KObjectFactory;
import org.komodo.spi.repository.KPropertyFactory;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.api.JcrTools;

public class JcrNodeFactory extends AbstractJcrFactory implements KObjectFactory {

    JcrPropertyFactory propertyFactory;

    JcrNodeFactory() {
        this.propertyFactory = new JcrPropertyFactory(this);
    }

    @Override
    public KPropertyFactory getPropertyFactory() {
        return propertyFactory;
    }

    @Override
    public Property getId(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
    
        try {
            return getProperty(transaction, kObject, JcrLexicon.JCR_UUID);
        } catch (KException ex) {
            throw handleError(ex);
        }
    }

    @Override
    public String getName(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");

        try {
            String result = node(transaction, kObject).getName();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject getParent(UnitOfWork transaction, KomodoObject child) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(child, "child");

        Node childNode = node(transaction, child);
        try {
            return new ObjectImpl(child.getRepository(), childNode.getParent().getPath(), 0);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Descriptor getType(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");

        try {
            Node node = node(transaction, kObject);
            String type = node.getPrimaryNodeType().getName();
            return new DescriptorImpl(kObject.getRepository(), type);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void setType(UnitOfWork transaction, KomodoObject kObject, String typeName) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
    
        try {
            String type = (StringUtils.isBlank(typeName) ? NTLexicon.NT_UNSTRUCTURED : typeName);
            node(transaction, kObject).setPrimaryType(type);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public boolean hasNode(UnitOfWork transaction, String absolutePath) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotEmpty(absolutePath, "absolutePath");

        try {
            return getSession(transaction).nodeExists(absolutePath);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject getNode(UnitOfWork transaction, Repository repository, String absolutePath) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(repository, "repository");
        ArgCheck.isNotEmpty(absolutePath, "absolutePath");

        try {
            Session session = getSession(transaction);
            if (!session.nodeExists(absolutePath))
                return null;

            Node node = node(transaction, absolutePath);
            return new ObjectImpl(repository, node.getPath(), 0);

        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject getNodeById(UnitOfWork transaction, Repository repository, String id) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(id, "id");
    
        String sql = "SELECT * FROM [nt:base] WHERE [jcr:uuid] = '" + id + "'"; //$NON-NLS-1$ //$NON-NLS-2$
    
        try {
            Query query = getSession(transaction).getWorkspace().getQueryManager().createQuery(sql, Query.JCR_SQL2);
            QueryResult result = query.execute();
            NodeIterator itr = result.getNodes();
    
            if (itr.getSize() == 0) {
                return null;
            }
    
            if (itr.getSize() == 1) {
                Node node = itr.nextNode();
                return new ObjectImpl(repository, node.getPath(), node.getIndex());
            }
    
            throw new KException(Messages.getString(Messages.Komodo.DUPLICATE_OBJECT_ERROR, id));
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject create(UnitOfWork transaction, Repository repository, String absolutePath, String nodeType) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotEmpty(absolutePath, "absolutePath");
        ArgCheck.isNotEmpty(nodeType, "nodeType");
    
        Session session = getSession(transaction);
    
        try {
            Node node;
            if (nodeType == null)
                node = new JcrTools().findOrCreateNode(session, absolutePath);
            else
                node = new JcrTools().findOrCreateNode(session, absolutePath, nodeType);
    
            KomodoObject result = new ObjectImpl(repository, node.getPath(), node.getIndex());
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public boolean hasChild(UnitOfWork transaction, KomodoObject parent, String childName) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(parent, "parent");
        ArgCheck.isNotNull(childName, "childName");

        Node parentNode = node(transaction, parent);
        try {
            return parentNode.hasNode(childName);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject getChild(UnitOfWork transaction, KomodoObject parent, String childName) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(parent, "parentNode");
        ArgCheck.isNotNull(childName, "childName");

        Node parentNode = node(transaction, parent);
        try {
            Node childNode = parentNode.getNode(childName);
            return new ObjectImpl(parent.getRepository(), childNode.getPath(), 0);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public boolean hasChildren(UnitOfWork transaction, KomodoObject parent) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(parent, "parent");

        try {
            Node parentNode = node(transaction, parent);
            return parentNode.hasNodes();
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<KomodoObject> getChildren(UnitOfWork transaction, KomodoObject parent, String... namePatterns)throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(parent, "parentNode");
        ArgCheck.isNotNull(namePatterns, "namePatterns");

        Node parentNode = node(transaction, parent);
        try {
            Collection<KomodoObject> children = new ArrayList<>();
            NodeIterator childNodes = null;
            
            if ((namePatterns == null) || (namePatterns.length == 0)) {
                childNodes = parentNode.getNodes();
            } else {
                childNodes = parentNode.getNodes(namePatterns);
            }

            while (childNodes.hasNext()) {
                Node childNode = childNodes.nextNode();
                children.add(new ObjectImpl(parent.getRepository(), childNode.getPath(), 0));
            }

            return children;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject addChild(UnitOfWork transaction, KomodoObject parent, String nodeName, String type) throws KException {
        ArgCheck.isNotNull(parent, "parent");
        ArgCheck.isNotNull(nodeName, "nodeName");
        ArgCheck.isNotNull(type, "type");

        try {
            Node parentNode = node(transaction, parent);
            Node childNode = parentNode.addNode(nodeName, type);
            KomodoObject result = new ObjectImpl(parent.getRepository(), childNode.getPath(), 0);
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject addChild(UnitOfWork transaction, KomodoObject parent, String nodeName) throws KException {
        return addChild(transaction, parent, nodeName, NTLexicon.NT_UNSTRUCTURED);
    }

    @Override
    public Collection<String> getPropertyNames(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");

        try {
            List<String> names = new ArrayList<>();

            for (PropertyIterator iter = node(transaction, kObject).getProperties(); iter.hasNext();) {
                String name = iter.nextProperty().getName();
                names.add(name);
            }

            return names;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public boolean hasProperty(UnitOfWork transaction, KomodoObject kObject, String propertyName) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
        ArgCheck.isNotEmpty(propertyName, "propertyName");

        try {
            Node node = node(transaction, kObject);
            return node.hasProperty(propertyName);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Property getProperty(UnitOfWork transaction, KomodoObject kObject, String propertyName) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
        ArgCheck.isNotEmpty(propertyName, "propertName");

        try {
            Node node = node(transaction, kObject);
            Property result = null;

            if (node.hasProperty(propertyName)) {
                javax.jcr.Property jcrProperty = node.getProperty(propertyName);
                result = new PropertyImpl(kObject.getRepository(), jcrProperty.getPath());
            }

            return result;
        } catch (Exception e) {
            throw handleError(e);
        }
    }

    @Override
    public void setProperty(UnitOfWork transaction, KomodoObject kObject, String name, Object... values) throws KException {
        propertyFactory.setProperty(transaction, kObject, name, values);
    }

    @Override
    public Descriptor getDescriptor(UnitOfWork transaction, KomodoObject kObject, String typeName) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
        ArgCheck.isNotEmpty(typeName, "name");

        try {
            Node node = node(transaction, kObject);
            NodeType[] nodeTypes = node.getMixinNodeTypes();
            Descriptor result = null;

            for ( NodeType nodeType : nodeTypes ) {
                if ( typeName.equals( nodeType.getName() ) ) {
                    result = new DescriptorImpl(kObject.getRepository(), nodeType.getName());
                    break;
                }
            }

            if ( result == null ) {
                throw new KException( Messages.getString( Messages.Komodo.DESCRIPTOR_NOT_FOUND, typeName, kObject.getAbsolutePath() ) );
            }

            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<Descriptor> getDescriptors(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");

        try {
            Node node = node(transaction, kObject);
            NodeType[] nodeTypes = node.getMixinNodeTypes();
            List<Descriptor> result = new ArrayList<>(nodeTypes.length);

            for (NodeType nodeType : nodeTypes) {
                result.add(new DescriptorImpl(kObject.getRepository(), nodeType.getName()));
            }

            return result;
        } catch ( Exception e ) {
            throw handleError( e );
        }
    }

    @Override
    public void addDescriptor(UnitOfWork transaction, KomodoObject kObject, String... descriptorNames) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
        ArgCheck.isNotNull(descriptorNames, "descriptorNames");

        try {
            Node node = node(transaction, kObject);
            for (String descriptorName : descriptorNames) {
                ArgCheck.isNotEmpty(descriptorName, "mixin"); //$NON-NLS-1$
                node.addMixin(descriptorName);
            }
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void removeDescriptor(UnitOfWork transaction, KomodoObject kObject, String... descriptorNames) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
        ArgCheck.isNotEmpty(descriptorNames, "descriptorNames");
    
        try {
            Node node = node(transaction, kObject);
            for (String descriptorName : descriptorNames) {
                ArgCheck.isNotEmpty(descriptorName, "mixin"); //$NON-NLS-1$
                node.removeMixin(descriptorName);
            }
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Descriptor[] getParentDescriptors(UnitOfWork transaction, Repository repository, Descriptor descriptor) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(repository, "repository");
        ArgCheck.isNotNull(descriptor, "descriptor");

        try {
            NodeTypeManager manager = nodeTypeManager(transaction);
            NodeType nodeType = manager.getNodeType(descriptor.getName());
            if (nodeType == null)
                return new Descriptor[0];

            List<Descriptor> descriptors = new ArrayList<>();
            NodeType[] superTypes = nodeType.getSupertypes();
            if (superTypes.length > 0) {
                for (NodeType superType : superTypes) {
                    descriptors.add(new DescriptorImpl(repository, superType.getName()));
                }
            }

            return descriptors.toArray(new Descriptor[0]);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    protected Collection<Descriptor> getAllDescriptors(Repository repository, Node node) throws Exception {
        List<Descriptor> descriptors = new ArrayList<>();
    
        NodeType[] mixins = node.getMixinNodeTypes();
        if (mixins != null) {
            for (NodeType mixin : mixins) {
                descriptors.add(new DescriptorImpl(repository, mixin.getName()));
            }
        }
    
        descriptors.add(new DescriptorImpl(repository, node.getPrimaryNodeType().getName()));
        return descriptors;
    }

    @Override
    public PropertyDescriptor getPropertyDescriptor(UnitOfWork transaction, KomodoObject kObject, String propName) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotEmpty(propName, "propName"); //$NON-NLS-1$

        try {
            Node node = node(transaction, kObject);
            for (Descriptor typeDescriptor : getAllDescriptors(kObject.getRepository(), node)) {
                for (PropertyDescriptor propDescriptor : typeDescriptor.getPropertyDescriptors(transaction)) {
                    if ((propDescriptor != null) && propName.equals(propDescriptor.getName())) {
                        return propDescriptor;
                    }
                }
            }

            return null;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Collection<PropertyDescriptor> getPropertyDescriptors(UnitOfWork transaction, Descriptor nodeDescriptor) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(nodeDescriptor, "nodeDescriptor");

        try {
            NodeTypeManager nodeTypeMgr = nodeTypeManager(transaction);
            PropertyDefinition[] propDefns = nodeTypeMgr.getNodeType(nodeDescriptor.getName()).getPropertyDefinitions();
            List<PropertyDescriptor> propDescriptors = new ArrayList<>(propDefns.length);

            for (PropertyDefinition propDefn : propDefns) {
                PropertyValueType type = propertyFactory.convert(propDefn.getRequiredType());

                Value[] values = propDefn.getDefaultValues();
                Object[] defaultValues = PropertyDescriptor.NO_VALUES;

                if ((values != null)) {
                    defaultValues = new Object[values.length];
                    int j = 0;
                    for (Value value : values) {
                        defaultValues[j++] = propertyFactory.convert(value, propDefn.getRequiredType());
                    }
                }

                propDescriptors.add(new PropertyDescriptorImpl(propDefn.isMandatory(), propDefn.isProtected(),
                                                               propDefn.isMultiple(), propDefn.getName(), type, defaultValues,
                                                               propertyFactory));
            }

            return propDescriptors;
        } catch (Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    @Override
    public void move(UnitOfWork transaction, KomodoObject kObject, String newPath) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
        ArgCheck.isNotEmpty(newPath, "newPath");

        try {
            getSession(transaction).move(kObject.getAbsolutePath(), newPath);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public void remove(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");

        try {
            Node node = node(transaction, kObject);
            node.remove();
        } catch (Exception e) {
            throw handleError(e);
        }
    }

    @Override
    public void print(UnitOfWork transaction, KomodoObject kObject) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");

        try {
            JcrTools tools = new JcrTools(true);
            tools.printSubgraph(node(transaction, kObject));
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public String getNamespaceURI(UnitOfWork transaction, String prefix) throws KException {
        checkTransaction(transaction);
        try {
            return getSession(transaction).getNamespaceURI(prefix);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject exportTeiidVdb(UnitOfWork transaction, KomodoObject parent, TeiidVdb teiidVdb) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(teiidVdb, "teiidVdb");
        ArgCheck.isTrue(transaction.isRollbackOnly(), "transaction should be rollback only");

        try {
            String vdbContent = teiidVdb.export();
            Session session = getSession(transaction);

            if (!(session instanceof org.modeshape.jcr.api.Session))
                throw new UnsupportedOperationException(); // Very unlikely to happen ... ... hopefully!!

            org.modeshape.jcr.api.Session mSession = (org.modeshape.jcr.api.Session)session;

            KomodoObject vdb = parent.addChild(transaction, teiidVdb.getName(), VdbLexicon.Vdb.VIRTUAL_DATABASE);
            KomodoObject fileNode = vdb.addChild(transaction, JcrLexicon.JCR_CONTENT, null);
            fileNode.setProperty(transaction, JcrLexicon.JCR_DATA, vdbContent);

            Property dataProperty = fileNode.getProperty(transaction, JcrLexicon.JCR_DATA);

            javax.jcr.Property inputProperty = session.getProperty(dataProperty.getAbsolutePath());
            Node outputNode = session.getNode(vdb.getAbsolutePath());
            mSession.sequence("VDB Dynamic Sequencer", inputProperty, outputNode);
            return vdb;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }
}
