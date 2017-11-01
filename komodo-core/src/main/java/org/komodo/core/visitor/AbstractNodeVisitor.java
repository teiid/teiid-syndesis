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
import java.util.List;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoObjectVisitor;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.MetadataVersion;
import org.komodo.spi.type.DataTypeService;
import org.komodo.utils.ArgCheck;

/**
 *
 */
public abstract class AbstractNodeVisitor implements KomodoObjectVisitor {

    private MetadataVersion version;

    private DataTypeService dataTypeService;

    /**
     * @param version teiid version
     */
    public AbstractNodeVisitor(MetadataVersion version, DataTypeService dataTypeService) {
        ArgCheck.isNotNull(version, "version");
        ArgCheck.isNotNull(dataTypeService, "dataTypeService");
        this.version = version;
        this.dataTypeService = dataTypeService;
    }

    /**
     * @return teiid version
     */
    public MetadataVersion getVersion() {
        return version;
    }

    /**
     * @return data type manager service
     */
    public DataTypeService getDataTypeService() throws Exception {
        return dataTypeService;
    }

    protected abstract String undefined();

    protected String findMixinTypeByNamespace(UnitOfWork transaction, KomodoObject kObject, String nspacePrefix) throws Exception {
        Descriptor[] mixinTypes = kObject.getDescriptors(transaction);
        if (mixinTypes.length == 0)
            return null;

        if (nspacePrefix == null)
            return null;

        if (! nspacePrefix.endsWith(StringConstants.COLON))
            nspacePrefix = nspacePrefix + StringConstants.COLON;

        for (Descriptor mixinType : mixinTypes) {

            if (mixinType.getName().startsWith(nspacePrefix))
                return mixinType.getName();
        }

        return null;
    }

    protected String findMixinTypeById(UnitOfWork transaction, KomodoObject kObject, String mixinTypeId) throws Exception {
        Descriptor[] mixins = kObject.getDescriptors(transaction);
        if (mixins.length == 0)
            return null;

        if (mixinTypeId == null)
            return null;

        for (Descriptor mixin : mixins) {
            if (mixin.getName().equals(mixinTypeId))
                return mixin.getName();
        }

        return null;
    }

    protected boolean hasMixinType(UnitOfWork transaction, KomodoObject kObject, String mixinTypeId) throws Exception {
        if (kObject == null || mixinTypeId == null)
            return false;

        String[] components = mixinTypeId.split(StringConstants.COLON);
        if (components == null)
            return false;

        String mixinType = findMixinTypeById(transaction, kObject, mixinTypeId);
        return mixinType != null ? mixinType.equals(mixinTypeId) : false;
    }

    protected void visitChild(UnitOfWork transaction, KomodoObject kObject, String relKomodoObjectPath) throws Exception {
        
        if (kObject.hasChild(transaction, relKomodoObjectPath)) {
            KomodoObject child = kObject.getChild(transaction, relKomodoObjectPath);
            child.accept(transaction, this);
        }
    }

    protected Collection<KomodoObject> getChildren(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        if (kObject == null)
            return Collections.emptyList();

       return Arrays.asList(kObject.getChildren(transaction));
    }

    protected Collection<KomodoObject> getChildren(UnitOfWork transaction, KomodoObject kObject, String mixinTypeId) throws Exception {
        if (kObject == null)
            return Collections.emptyList();
        
        KomodoObject[] childrenOfType = kObject.getChildrenOfType(transaction, mixinTypeId);
        return Arrays.asList(childrenOfType);
    }

    protected void visitFilteredChildren(UnitOfWork transaction, KomodoObject kObject, String typeName) throws Exception, Exception {
        KomodoObject[] childrenOfType = kObject.getChildrenOfType(transaction, typeName);
        for (KomodoObject child : childrenOfType) {
            child.accept(transaction, this);
        }
    }

    protected void visitChildren(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        KomodoObject[] children = kObject.getRawChildren(transaction);
        if (children == null)
            return;

        for (KomodoObject child : children) {
            child.accept(transaction, this);
        }
    }

    protected Property property(UnitOfWork transaction, KomodoObject kObject, String propName) throws Exception {
        if (kObject == null || propName == null)
            return null;
    
        if (! kObject.hasRawProperty(transaction, propName))
            return null;
    
        Property property = kObject.getRawProperty(transaction, propName);
        return property;
    }

    protected List<Object> multiPropertyValues(UnitOfWork transaction, Property refProp) throws Exception {
        List<Object> values = null;
        if (! refProp.isMultiple(transaction))
            values = Collections.singletonList(refProp.getValue(transaction));
        else
            values = Arrays.asList(refProp.getValues(transaction));
        return values;
    }

    protected String toString(UnitOfWork transaction, Property property) throws Exception {
        if (property == null)
            return undefined();

        Object value = property.isMultiple(transaction) ? property.getValues(transaction)[0] : property.getValue(transaction);
        return value == null ? undefined() : value.toString();
    }

}
