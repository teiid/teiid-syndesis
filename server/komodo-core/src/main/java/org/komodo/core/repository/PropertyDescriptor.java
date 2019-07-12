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
