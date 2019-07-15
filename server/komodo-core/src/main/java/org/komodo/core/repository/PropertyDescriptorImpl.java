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
package org.komodo.core.repository;

import org.komodo.spi.KException;
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
     * @see org.komodo.core.repository.PropertyDescriptor#getDefaultValues()
     */
    @Override
    public Object[] getDefaultValues() {
        return this.defaultValues;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.PropertyDescriptor#getName()
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.PropertyDescriptor#getType()
     */
    @Override
    public PropertyValueType getType() {
        return this.type;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.PropertyDescriptor#isMandatory()
     */
    @Override
    public boolean isMandatory() {
        return this.mandatory;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.PropertyDescriptor#isModifiable()
     */
    @Override
    public boolean isModifiable() {
        return this.modifiable;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.PropertyDescriptor#isMultiple()
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
