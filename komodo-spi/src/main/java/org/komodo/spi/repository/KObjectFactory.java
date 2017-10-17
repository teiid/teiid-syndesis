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
package org.komodo.spi.repository;

import java.util.Collection;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidVdb;

public interface KObjectFactory extends StringConstants, RepositoryConstants {

    KPropertyFactory getPropertyFactory();

    /**
     * @param transaction
     * @param absolutePath
     * @return true if there is a node at the given path
     * @throws KException 
     */
    boolean hasNode(UnitOfWork transaction, String absolutePath) throws KException;

    /**
     * @param transaction
     * @param repository
     * @param absolutePath
     * @return the node at the given path
     * @throws KException 
     */
    KomodoObject getNode(UnitOfWork transaction, Repository repository, String absolutePath) throws KException;

    /**
     * @param transaction
     * @param repository
     * @param id
     * @throws KException 
     */
    KomodoObject getNodeById(UnitOfWork transaction, Repository repository, String id) throws KException;

    /**
     * @param transaction
     * @param repository
     * @param absolutePath
     * @param nodeType
     * @return a new node of the given type at the given path. If already exists then that will be returned.
     * @throws KException 
     */
    KomodoObject create(UnitOfWork transaction, Repository repository, String absolutePath, String nodeType) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @return the id of the given node
     * @throws KException 
     */
    Property getId(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @return the name of the given node
     * @throws KException 
     */
    String getName(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @return the parent of this node
     * @throws KException 
     */
    KomodoObject getParent(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @return get the type of this node
     * @throws KException 
     */
    Descriptor getType(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @param type
     * @throws KException 
     */
    void setType(UnitOfWork transaction, KomodoObject kObject, String type) throws KException;

    /**
     * @param transaction
     * @param kObject the node
     * @return the collection of descriptors of this node
     * @throws KException 
     */
    Collection<Descriptor> getDescriptors(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @param name
     * @return the named descriptor or null
     * @throws KException 
     */
    Descriptor getDescriptor(UnitOfWork transaction, KomodoObject kObject, String name) throws KException;

    /**
     * @param transaction
     * @param repository
     * @param descriptor
     * @return the parent descriptors of the given {@link Descriptor}
     * @throws KException 
     */
    Descriptor[] getParentDescriptors(UnitOfWork transaction, Repository repository, Descriptor descriptor) throws KException;

    /**
     * Add the given descriptors to the node
     *
     * @param transaction
     * @param kObject
     * @param descriptorNames
     * @throws KException 
     */
    void addDescriptor(UnitOfWork transaction, KomodoObject kObject, String... descriptorNames) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @param descriptorNames
     * @throws KException 
     */
    void removeDescriptor(UnitOfWork transaction, KomodoObject kObject, String... descriptorNames) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @param propName
     * @return the {@link PropertyDescriptor} with the given name
     * @throws KException
     */
    PropertyDescriptor getPropertyDescriptor(UnitOfWork transaction, KomodoObject kObject, String propName) throws KException;

    /**
     * @param transaction
     * @param nodeDescriptor the node descriptor
     * @return the collection of property descriptors belonging to the given node descriptor
     * @throws KException 
     */
    Collection<PropertyDescriptor> getPropertyDescriptors(UnitOfWork transaction, Descriptor nodeDescriptor) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @return this node has any children
     * @throws KException 
     */
    boolean hasChildren(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param parent
     * @param namePatterns 
     * @return the parent node's immediate children
     * @throws KException 
     */
    Collection<KomodoObject> getChildren(UnitOfWork transaction, KomodoObject parent, String... namePatterns) throws KException;

    /**
     * @param transaction
     * @param parent
     * @param childName
     * @return 
     * @throws KException 
     */
    boolean hasChild(UnitOfWork transaction, KomodoObject parent, String childName) throws KException;

    /**
     * @param transaction
     * @param name
     * @return the child of parent with name
     * @throws KException 
     */
    KomodoObject getChild(UnitOfWork transaction, KomodoObject parent, String name) throws KException;

    /**
     * Adds a new Node with the given node as a parent
     *
     * @param transaction
     * @param parent
     * @param kObjectName
     * @param type
     *
     * @return the new node
     * @throws KException
     */
    KomodoObject addChild(UnitOfWork transaction, KomodoObject parent, String nodeName, String type) throws KException;

    /**
     * Adds a new Node with the given node as a parent
     *
     * @param transaction
     * @param parent
     * @param kObjectName
     *
     * @return the new node
     * @throws KException
     */
    KomodoObject addChild(UnitOfWork transaction, KomodoObject parent, String nodeName) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @param newPath the new path. Can be relative or absolute but parent must be in the repository
     * @throws KException 
     */
    void move(UnitOfWork transaction, KomodoObject kObject, String newPath) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @throws KException 
     */
    void remove(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @return a collection of the names of all the properties of this node
     * @throws KException 
     */
    Collection<String> getPropertyNames(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @param propertyName
     * @return whether node has named property
     * @throws KException 
     */
    boolean hasProperty(UnitOfWork transaction, KomodoObject kObject, String propertyName) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @param propertyName
     * @throws KException
     * @return the named property from the node
     */
    Property getProperty(UnitOfWork transaction, KomodoObject kObject, String propertyName) throws KException;

    /**
     * Set the property of this node to the given value(s)
     *
     * @param transaction
     * @param kObject
     * @param name
     * @param values
     * @throws KException 
     */
    void setProperty(UnitOfWork transaction, KomodoObject kObject, String propertyName, Object... values) throws KException;

    /**
     * @param transaction
     * @param kObject
     * @throws KException 
     */
    void print(UnitOfWork transaction, KomodoObject kObject) throws KException;

    /**
     * @param transaction
     * @param prefix
     * @return the namespace URI represented by this prefix
     * @throws KException 
     */
    String getNamespaceURI(UnitOfWork transaction, String prefix) throws KException;

    /**
     * @param uow a transaction that expects to be rolled back since {@link KomodoObject}s created
     *                  by this method are meant to be transient and thrown away once properties have been
     *                  extracted from them.
     * @param teiidVdb
     * @param repository
     * @return a transient Vdb {@link KomodoObject}
     * @throws KException 
     */
    KomodoObject convertTeiidVdb(UnitOfWork uow, Repository repository, TeiidVdb teiidVdb) throws KException;
}
