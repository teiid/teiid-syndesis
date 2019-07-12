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

import java.io.InputStream;
import java.util.Calendar;
import java.util.Date;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.RepositoryConstants;
import org.komodo.spi.repository.UnitOfWork;

public interface KPropertyFactory extends StringConstants, RepositoryConstants {

    String DELIMITER = AT;

    /**
     * Values will be converted to a string if a conversion cannot be done. Only boolean, long, decimal, double, and string are
     * supported. A string conversion is done for all unsupported types.
     *
     * @param value
     *        the value (cannot be <code>null</code>)
     * @param type
     *        the required type of the property
     * @return the value converted to the required type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Object convert(Object value, PropertyValueType type) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the name of the property
     * @throws KException
     */
    String getName(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the parent of the property
     * @throws KException
     */
    KomodoObject getParent(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return the property descriptor
     * @throws KException 
     */
    PropertyDescriptor getPropertyDescriptor(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return the property type
     * @throws KException 
     */
    PropertyValueType getType(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return true if this property contains multiple values
     * @throws KException 
     */
    boolean isMultiple(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return the value of the property according to the property's own type
     * @throws Exception
     */
    Object getValue(UnitOfWork transaction, Property property) throws Exception;

    /**
     * @param transaction
     * @param property
     * @return the values of the property according to the property's own type
     * @throws Exception
     */
    Object[] getValues(UnitOfWork transaction, Property property) throws Exception;

    /**
     * @param transaction
     * @param property
     * @return get the binary value of the property
     * @throws KException 
     */
    InputStream getBinary(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the binary streams of the property
     * @throws KException 
     */
    InputStream[] getBinaryValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the boolean value of the property
     * @throws KException 
     */
    Boolean getBoolean(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the boolean values of the property
     * @throws KException 
     */
    Boolean[] getBooleanValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link String} value of the property
     * @throws KException 
     */
    String getString(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link String}[] values of the property
     * @throws KException 
     */
    String[] getStringValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Long} value of the property
     * @throws KException 
     */
    Long getLong(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Long} values of the property
     * @throws KException 
     */
    Long[] getLongValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Integer} value of the property
     * @throws KException 
     */
    Integer getInteger(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Integer} values of the property
     * @throws KException 
     */
    Integer[] getIntegerValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Double} value of the property
     * @throws KException 
     */
    Double getDouble(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Double} values of the property
     * @throws KException 
     */
    Double[] getDoubleValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Date} value of the property
     * @throws KException 
     */
    Calendar getDate(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the {@link Date} values of the property
     * @throws KException
     */
    Calendar[] getDateValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the reference value of the property
     * @throws KException 
     */
    String getReference(UnitOfWork transaction, Property property) throws KException;

    /**
     * @param transaction
     * @param property
     * @return get the reference values of the property
     * @throws KException 
     */
    String[] getReferenceValues(UnitOfWork transaction, Property property) throws KException;

    /**
     * Set the value of the property
     *
     * @param transaction
     * @param property
     * @param values
     * @throws KException 
     */
    void setValue(UnitOfWork transaction, Property property, Object... values) throws KException;

}
