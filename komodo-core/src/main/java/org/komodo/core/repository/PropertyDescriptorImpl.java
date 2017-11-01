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
package org.komodo.core.repository;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KPropertyFactory;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.utils.ArgCheck;

/**
 * A {@link PropertyDescriptor property descriptor} implementation.
 */
public class PropertyDescriptorImpl implements PropertyDescriptor {

    private final Object[] defaultValues;
    private final boolean mandatory;
    private final boolean modifiable;
    private final boolean multiple;
    private final String name;
    private final PropertyValueType type;
    private final KPropertyFactory factory;

    /**
     * @param mandatory
     *        <code>true</code> if the property is mandatory
     * @param modifiable
     *        <code>true</code> if the property is modifiable
     * @param multiple
     *        <code>true</code> if the property is multi-valued
     * @param name
     *        the property name (cannot be empty)
     * @param type
     *        the property type (cannot be <code>null</code>)
     * @param values
     *        the default values (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    public PropertyDescriptorImpl(
                                   final boolean mandatory,
                                   final boolean modifiable,
                                   final boolean multiple,
                                   final String name,
                                   final PropertyValueType type,
                                   final Object[] values,
                                   final KPropertyFactory factory) throws KException {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotNull( type, "type" ); //$NON-NLS-1$

        this.mandatory = mandatory;
        this.modifiable = modifiable;
        this.multiple = multiple;
        this.name = name;
        this.type = type;
        this.factory = factory;

        if ( ( values == null ) || ( values.length == 0 ) ) {
            this.defaultValues = NO_VALUES;
        } else {
            this.defaultValues = new Object[ values.length ];
            int i = 0;

            
            for ( final Object value : values ) {
                this.defaultValues[i++] = getFactory().convert(value, this.type);
            }
        }
    }

    protected KPropertyFactory getFactory() {
        return factory;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#getDefaultValues()
     */
    @Override
    public Object[] getDefaultValues() {
        return this.defaultValues;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#getType()
     */
    @Override
    public PropertyValueType getType() {
        return this.type;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#isMandatory()
     */
    @Override
    public boolean isMandatory() {
        return this.mandatory;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#isModifiable()
     */
    @Override
    public boolean isModifiable() {
        return this.modifiable;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.PropertyDescriptor#isMultiple()
     */
    @Override
    public boolean isMultiple() {
        return this.multiple;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return getName();
    }

}
