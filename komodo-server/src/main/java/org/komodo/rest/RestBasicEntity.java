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
package org.komodo.rest;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * Indicates the objects has a JSON representation.
 */
public class RestBasicEntity extends AbstractKEntity {

    /**
     * A {@link RestBasicEntity} that indicates the resource was not found.
     */
    public static class ResourceNotFound extends RestBasicEntity {

        private final String operationName;
        private final String resourceName;

        /**
         * @param resourceName
         *        the name of the resource that was not found (cannot be empty)
         * @param operationName
         *        the operation that was executed (cannot be empty)
         */
        public ResourceNotFound(final String resourceName, final String operationName) {
            super();
            ArgCheck.isNotEmpty(resourceName, "resourceName"); //$NON-NLS-1$
            ArgCheck.isNotEmpty(operationName, "operationName"); //$NON-NLS-1$

            this.resourceName = resourceName;
            this.operationName = operationName;
        }

        /**
         * @return the operation name (never empty)
         */
        public String getOperationName() {
            return this.operationName;
        }

        /**
         * @return the resource name (never empty)
         */
        public String getResourceName() {
            return this.resourceName;
        }

    }

    /**
     * Indicates no content is being returned.
     */
    public static final RestBasicEntity NO_CONTENT = new RestBasicEntity() {
        // nothing to do
    };

    /**
     * Used for NO_CONTENT and ResourceNotFound
     */
    public RestBasicEntity() {
        super();
    }

    /**
     * @param baseUri the base uri of the REST request
     * @throws KException if error occurs
     */
    public RestBasicEntity(URI baseUri) throws KException {
        super(baseUri);
    }

    /**
     * @param baseUri the base uri of the REST request
     * @param kObject the kObject
     * @param uow the transaction
     * @param createCommonLinks should the self and parent links be created
     * @throws KException if error occurs
     */
    protected RestBasicEntity(URI baseUri, KomodoObject kObject, UnitOfWork uow, boolean createCommonLinks) throws KException {
        this(baseUri);

        ArgCheck.isNotNull(kObject, "kObject"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(kObject.getName(uow));
        setDataPath(kObject.getAbsolutePath());
        setkType(kObject.getTypeIdentifier(uow));
        setHasChildren(kObject.hasChildren(uow));

        if (createCommonLinks) {
            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(SEARCH_PATH_PARAMETER, getDataPath());
            addLink(new RestLink(LinkType.SELF, getUriBuilder().searchUri(properties)));

            KomodoObject parent = kObject.getParent(uow);
            ArgCheck.isNotNull(parent);
            properties = new KomodoProperties();
            properties.addProperty(SEARCH_PATH_PARAMETER, parent.getAbsolutePath());
            addLink(new RestLink(LinkType.PARENT, getUriBuilder().searchUri(properties)));

            createChildLink();
        }
    }

    protected void createChildLink() {
        KomodoProperties properties;
        properties = new KomodoProperties();
        properties.addProperty(SEARCH_PARENT_PARAMETER, getDataPath());
        addLink(new RestLink(LinkType.CHILDREN, getUriBuilder().searchUri(properties)));
    }

    /**
     * @param baseUri the base uri of the REST request
     * @param kObject the kObject
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestBasicEntity(URI baseUri, KomodoObject kObject, UnitOfWork uow) throws KException {
        this(baseUri, kObject, uow, true);
    }

    /**
     * @param kObject the object
     * @param parentClass the class of the desired parent
     * @return the ancestor of the object with the given class
     */
    protected <T extends KomodoObject> T ancestor(KomodoObject kObject, Class<T> parentClass, UnitOfWork uow) throws KException {
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(kObject.getRepository(), uow);
        KomodoObject parent = kObject.getParent(uow);
        while (parent != null) {
            T resolvedParent = wsMgr.resolve(uow, parent, parentClass);
            if (resolvedParent != null)
                return resolvedParent;

            parent = parent.getParent(uow);
        }

        return null;
    }

    /**
     * @return the id
     */
    public String getId() {
        Object id = tuples.get(ID);
        return id != null ? id.toString() : null;
    }

    /**
     * @param id the id to set
     */
    public void setId(String id) {
        tuples.put(ID, id);
    }

    /**
     * @return the dataPath
     */
    public String getDataPath() {
        Object path = tuples.get(DATA_PATH);
        return path != null ? path.toString() : null;
    }

    /**
     * @param dataPath the dataPath to set
     */
    public void setDataPath(String dataPath) {
        tuples.put(DATA_PATH, dataPath);
    }

    /**
     * @return the kType
     */
    public KomodoType getkType() {
        Object ktype = tuples.get(KTYPE);
        return ktype != null ? KomodoType.getKomodoType(ktype.toString()) : null;
    }

    /**
     * @param kType the kType to set
     */
    public void setkType(KomodoType kType) {
        tuples.put(KTYPE, kType);
    }

    /**
     * @return the hasChildren
     */
    public boolean hasChildren() {
        Object value = tuples.get(HAS_CHILDREN);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param hasChildren the hasChildren to set
     */
    public void setHasChildren(boolean hasChildren) {
        tuples.put(HAS_CHILDREN, hasChildren);
    }

    protected boolean hasPrefix(String name) {
        return name.matches(PREFIX_PATTERN);
    }

    /**
     * Derives execution properties from the given {@link KomodoObject}
     * and adds them to this entity
     *
     * @param uow transaction required for fetching the properties from the {@link KomodoObject}
     * @param kObject the source {@link KomodoObject}
     * @throws KException if error occurs
     */
    public void addExecutionProperties(UnitOfWork uow, KomodoObject kObject) throws KException {
        final List<String> propNames = new ArrayList<>(Arrays.asList(kObject.getPropertyNames(uow))); // props with values
        final PropertyDescriptor[] descriptors = kObject.getPropertyDescriptors(uow);

        if (descriptors.length != 0) {
            for (PropertyDescriptor descriptor : descriptors) {
                String name = descriptor.getName();
                if (!propNames.contains(name)) {
                    propNames.add(name);
                }
            }
        }

        //
        // Execution properties are stored in komodo object without a prefix
        //
        Iterator<String> propIter = propNames.iterator();
        while(propIter.hasNext()) {
            String propName = propIter.next();

            if (hasPrefix(propName))
                continue;

            Property attribute = kObject.getProperty(uow, propName);
            if (attribute == null)
                continue;

            if (attribute.isMultiple(uow)) {
                Object[] values = attribute.getValues(uow);
                addProperty(propName, values);
            } else {
                Object value = attribute.getValue(uow);
                addProperty(propName, value);
            }
        }
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestBasicEntity [tuples=" + this.tuples + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
