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
package org.komodo.spi.utils;

import java.beans.PropertyChangeListener;

/**
 * Provides properties and notifies registered listeners of property changes.
 */
public interface PropertyProvider extends PropertyChangeListener {

    /**
     * @param listener
     *        the listener being added (cannot be <code>null</code>)
     * @return <code>true</code> if the listener was added
     */
    boolean addPropertyChangeListener( final PropertyChangeListener listener );

    /**
     * @param propertyName
     *        the name of the property whose value is being requested (can be empty)
     * @return the property value (can be <code>null</code> if the property name is empty, if the value is <code>null</code>, or
     *         if the property does not exist)
     */
    Object getProperty( final String propertyName );

    /**
     * @param propertyName
     *        the name of the property whose existence is being checked (can be empty)
     * @return <code>true</code> if the property name is not empty, the property exists, and the property has a non-
     *         <code>null</code> value
     */
    boolean hasProperty( final String propertyName );

    /**
     * @param listener
     *        the listener being removed (can be <code>null</code>)
     * @return <code>true</code> if the listener was removed
     */
    boolean removePropertyChangeListener( final PropertyChangeListener listener );

}
