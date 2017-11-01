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
package org.komodo.spi.repository;

import java.util.Comparator;

/**
 * A {@link KomodoObject Komodo object's} {@link Property property} descriptor.
 */
public interface PropertyDescriptor {

    /**
     * Sorts {@link PropertyDescriptor}s by name.
     */
    Comparator< PropertyDescriptor > NAME_SORTER = new Comparator< PropertyDescriptor >() {

        /**
         * {@inheritDoc}
         *
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare( final PropertyDescriptor thisDescriptor,
                            final PropertyDescriptor thatDescriptor ) {
            return thisDescriptor.getName().compareTo( thatDescriptor.getName() );
        }

    };

    /**
     * An empty array of property descriptors.
     */
    PropertyDescriptor[] NO_DESCRIPTORS = {};

    /**
     * An empty array of objects.
     */
    Object[] NO_VALUES = {};

    /**
     * If the property is multi-valued, there can be more than one default value. For a single-valued property there can be at
     * most one default value.
     *
     * @return the default values or an empty array if no default values exist.
     */
    Object[] getDefaultValues();

    /**
     * @return the property name (never <code>null</code> or empty)
     */
    String getName();

    /**
     * @return the type of the property (never <code>null</code>)
     */
    PropertyValueType getType();

    /**
     * @return <code>true</code> if property is required
     */
    boolean isMandatory();

    /**
     * @return <code>true</code> if the property value is modifiable
     */
    boolean isModifiable();

    /**
     * @return <code>true</code> if this property is multi-valued
     */
    boolean isMultiple();

}
