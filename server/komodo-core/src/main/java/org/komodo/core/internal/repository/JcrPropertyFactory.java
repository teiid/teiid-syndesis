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
package org.komodo.core.internal.repository;

import java.io.InputStream;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.jcr.Binary;
import javax.jcr.Node;
import javax.jcr.PropertyType;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.nodetype.PropertyDefinition;

import org.komodo.core.repository.KPropertyFactory;
import org.komodo.core.repository.Messages;
import org.komodo.core.repository.Messages.Komodo;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.PropertyDescriptorImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.teiid.core.util.ArgCheck;

public class JcrPropertyFactory extends AbstractJcrFactory implements KPropertyFactory {

    private KObjectFactory nodeFactory;
    private Repository repository;

    public JcrPropertyFactory(KObjectFactory nodeFactory, Repository repository) {
        this.nodeFactory = nodeFactory;
        this.repository = repository;
    }

    /**
     * @param type
     *        the {@link PropertyType JCR property type} being converted
     * @return the object property type or {@link org.komodo.spi.repository.PropertyDescriptor.Type#UNDEFINED} if type cannot be
     *         determined
     */
    PropertyValueType convert(int type) {
        if (type == PropertyType.BINARY)
            return PropertyValueType.BINARY;
        if (type == PropertyType.BOOLEAN)
            return PropertyValueType.BOOLEAN;
        if (type == PropertyType.DATE)
            return PropertyValueType.DATE;
        if (type == PropertyType.DECIMAL)
            return PropertyValueType.DOUBLE;
        if (type == PropertyType.DOUBLE)
            return PropertyValueType.DOUBLE;
        if (type == PropertyType.LONG)
            return PropertyValueType.LONG;
        if (type == PropertyType.NAME)
            return PropertyValueType.STRING;
        if (type == PropertyType.PATH)
            return PropertyValueType.STRING;
        if (type == PropertyType.REFERENCE)
            return PropertyValueType.REFERENCE;
        if (type == PropertyType.STRING)
            return PropertyValueType.STRING;
        if (type == PropertyType.URI)
            return PropertyValueType.STRING;
        if (type == PropertyType.WEAKREFERENCE)
            return PropertyValueType.REFERENCE;

        return PropertyValueType.UNDEFINED;
    }

    /**
     * @param value
     *        the JCR value holder (cannot be <code>null</code>)
     * @param propertyType
     *        the required type of the property
     * @return the <code>Object</code> representation of the JCR value (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Object convert(Value value, int propertyType) throws KException {
        try {
            switch (propertyType) {
                case PropertyType.BOOLEAN:
                    return value.getBoolean();
                case PropertyType.LONG:
                    return value.getLong();
                case PropertyType.DOUBLE:
                    return value.getDouble();
                case PropertyType.DATE:
                    return value.getDate();
                case PropertyType.DECIMAL:
                    return value.getDecimal();
                default:
                    return value.getString();
            }
        } catch (final Exception e) {
            throw new KException(Messages.getString(Komodo.UNABLE_TO_CONVERT_VALUE, propertyType), e);
        }
    }

    /**
     * @param type
     *        the {@link KomodoObject Komodo object} {@link Property property}
     *        {@link org.komodo.spi.repository.PropertyDescriptor.Type type} being converted (cannot be <code>null</code>)
     * @return the {@link PropertyType JCR property type}
     */
    int convert(PropertyValueType type) {
        ArgCheck.isNotNull(type, "type"); //$NON-NLS-1$

        if (type == PropertyValueType.BINARY)
            return PropertyType.BINARY;
        if (type == PropertyValueType.BOOLEAN)
            return PropertyType.BOOLEAN;
        if (type == PropertyValueType.DATE)
            return PropertyType.DATE;
        if (type == PropertyValueType.DOUBLE)
            return PropertyType.DOUBLE;
        if (type == PropertyValueType.LONG)
            return PropertyType.LONG;
        if (type == PropertyValueType.REFERENCE)
            return PropertyType.REFERENCE;
        if (type == PropertyValueType.STRING)
            return PropertyType.STRING;

        return PropertyType.UNDEFINED;
    }

    /**
     * Values will be converted to a string if a conversion cannot be done. Only boolean, long, decimal, double, and string are
     * supported. A string conversion is done for all unsupported types.
     *
     * @param value
     *        the value (cannot be <code>null</code>)
     * @param propertyType
     *        the required type of the property
     * @return the value converted to the required type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Object convert(Object value, int propertyType) throws KException {
        if (value instanceof Value) {
            return convert((Value)value, propertyType);
        }

        try {
            switch (propertyType) {
                case PropertyType.BOOLEAN:
                    return Boolean.parseBoolean(value.toString());
                case PropertyType.LONG:
                    return Long.parseLong(value.toString());
                case PropertyType.DOUBLE:
                case PropertyType.DECIMAL:
                    return Double.parseDouble(value.toString());
                default:
                    return value.toString();
            }
        } catch (Exception e) {
            throw new KException(Messages.getString(Komodo.UNABLE_TO_CONVERT_VALUE, propertyType), e);
        }
    }

    /**
     * @param factory
     *        the factory used to perform the conversion (cannot be <code>null</code>)
     * @param value
     *        the value being converted to a JCR value holder (can be <code>null</code>)
     * @return the JCR value holder (can be <code>null</code> if input is <code>null</code>)
     * @throws Exception if an error occurs
     */
    Value createValue(ValueFactory factory, Object value) throws Exception {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$

        if (value == null) {
            return null;
        }

        if (value instanceof Value) {
            return (Value)value;
        }

        if (value instanceof Boolean) {
            return factory.createValue(Boolean.class.cast(value));
        }

        if (value instanceof Long) {
            return factory.createValue(Long.class.cast(value));
        }

        if (value instanceof Double) {
            return factory.createValue(Double.class.cast(value));
        }

        if (value instanceof Calendar) {
            return factory.createValue(Calendar.class.cast(value));
        }

        if (value instanceof BigDecimal) {
            return factory.createValue(BigDecimal.class.cast(value));
        }

        if (value instanceof InputStream) {
            Binary binary = factory.createBinary((InputStream)value);
            return factory.createValue(binary);
        }

        return factory.createValue(value.toString());
    }

    /**
     * @param factory
     *        the factory used to perform the conversion (cannot be <code>null</code>)
     * @param value
     *        the value being converted to a JCR value holder (cannot be <code>null</code>)
     * @param jcrPropType
     *        the JCR {@link PropertyType property type}
     * @return the JCR value holder (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    Value createValue(ValueFactory factory, Object value, int jcrPropType) throws Exception {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$
        ArgCheck.isNotNull(value, "value"); //$NON-NLS-1$

        if (PropertyType.UNDEFINED == jcrPropType) {
            return createValue(factory, value);
        }

        if (PropertyType.BINARY == jcrPropType) {
            if (value instanceof InputStream) {
                InputStream stream = (InputStream)value;
                Binary binary = factory.createBinary(stream);
                return factory.createValue(binary);
            }

            throw new Exception("A Binary property value must be in the form of an InputStream");
        }

        if (PropertyType.BOOLEAN == jcrPropType) {
            if (value instanceof Boolean) {
                return factory.createValue((Boolean)value);
            }

            return factory.createValue(Boolean.parseBoolean(value.toString()));
        }

        if (PropertyType.LONG == jcrPropType) {
            if (value instanceof Long) {
                return factory.createValue((Long)value);
            }

            return factory.createValue(Long.parseLong(value.toString()));
        }

        if (PropertyType.DOUBLE == jcrPropType) {
            if (value instanceof Double) {
                return factory.createValue((Double)value);
            }

            return factory.createValue(Double.parseDouble(value.toString()));
        }

        if (PropertyType.DATE == jcrPropType) {
            if (value instanceof Calendar) {
                return factory.createValue((Calendar)value);
            }

            Calendar calendar = Calendar.getInstance();
            Date date = DateFormat.getDateInstance().parse(value.toString());
            calendar.setTime(date);

            return factory.createValue(calendar);
        }

        if (PropertyType.DECIMAL == jcrPropType) {
            if (value instanceof BigDecimal) {
                return factory.createValue((BigDecimal)value);
            }

            return factory.createValue(new BigDecimal(value.toString()));
        }

        return factory.createValue(value.toString());
    }

    /**
     * @param factory
     *        the factory used to perform the conversion (cannot be <code>null</code>)
     * @param values
     *        the values being converted to a JCR value holders (cannot be <code>null</code>)
     * @param jcrPropType
     *        the JCR {@link PropertyType property type}
     * @return the JCR value holders (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    Value[] createValues(ValueFactory factory, Object[] values, int jcrPropType) throws Exception {
        ArgCheck.isNotNull(factory, "factory"); //$NON-NLS-1$
        ArgCheck.isNotNull(values, "values"); //$NON-NLS-1$

        List<Value> result = new ArrayList<Value>();

        if ((values == null) || (values.length == 0)) {
            return new Value[0];
        }

        for (Object value : values) {
            result.add(createValue(factory, value, jcrPropType));
        }

        return result.toArray(new Value[result.size()]);
    }

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
    public Object convert(Object value, PropertyValueType type) throws KException {
        if (value == null)
            return null;

        if (value.getClass().isArray()) {
            Object[] values = (Object[])value;
            List<Object> converted = new ArrayList<>();
            for (Object compValue : values) {
                converted.add(convert(compValue, type));
            }
            return converted.toArray(new Object[0]);
        }

        try {
            switch (type) {
                case BINARY:
                case DATE:
                case REFERENCE:
                case BOOLEAN:
                    return Boolean.parseBoolean(value.toString());
                case INTEGER:
                    return Integer.parseInt(value.toString());
                case LONG:
                    return Long.parseLong(value.toString());
                case DOUBLE:
                    return Double.parseDouble(value.toString());
                default:
                    return value.toString();
            }
        } catch (Exception e) {
            throw new KException(Messages.getString(Komodo.UNABLE_TO_CONVERT_VALUE, type), e);
        }
    }

    @Override
    public String getName(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            String result = getSession(transaction).getProperty(property.getAbsolutePath()).getName();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public KomodoObject getParent(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            Node parent = getSession(transaction).getProperty(property.getAbsolutePath()).getParent();
            String parentPath = parent.getPath();

            if (!parentPath.endsWith(FORWARD_SLASH)) { //$NON-NLS-1$
                parentPath += FORWARD_SLASH; //$NON-NLS-1$
            }

            return new ObjectImpl(repository, parent.getPath(), 0);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public PropertyValueType getType(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        PropertyValueType propertyValueType = PropertyValueType.UNDEFINED;

        try {
            javax.jcr.Property result = getSession(transaction).getProperty(property.getAbsolutePath());

            int requiredType = result.getDefinition().getRequiredType();
            switch (requiredType) {
                case PropertyType.NAME:
                case PropertyType.STRING:
                case PropertyType.PATH:
                case PropertyType.REFERENCE:
                case PropertyType.URI:
                case PropertyType.WEAKREFERENCE:
                    propertyValueType = PropertyValueType.STRING;
                    break;
                case PropertyType.LONG:
                    propertyValueType = PropertyValueType.LONG;
                    break;
                case PropertyType.DOUBLE:
                case PropertyType.DECIMAL:
                    propertyValueType = PropertyValueType.DOUBLE;
                    break;
                case PropertyType.DATE:
                    propertyValueType = PropertyValueType.DATE;
                    break;
                case PropertyType.BOOLEAN:
                    propertyValueType = PropertyValueType.BOOLEAN;
                    break;
                case PropertyType.BINARY:
                case PropertyType.UNDEFINED:
                default:
                    propertyValueType = PropertyValueType.UNDEFINED;
            }

            return propertyValueType;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public boolean isMultiple(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            Session session = getSession(transaction);
            boolean result = session.getProperty(property.getAbsolutePath()).isMultiple();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public PropertyDescriptor getPropertyDescriptor(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            PropertyDefinition propDefn = jcrProperty.getDefinition();
            PropertyValueType type = convert(propDefn.getRequiredType());

            Value[] values = propDefn.getDefaultValues();
            Object[] defaultValues = PropertyDescriptor.NO_VALUES;

            if ((values != null)) {
                defaultValues = new Object[values.length];
                int j = 0;
                for (Value value : values) {
                    defaultValues[j++] = convert(value, propDefn.getRequiredType());
                }
            }

            PropertyDescriptor result = new PropertyDescriptorImpl(propDefn.isMandatory(), propDefn.isProtected(),
                                                                   propDefn.isMultiple(), propDefn.getName(), type, defaultValues,
                                                                   this);

            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Object getValue(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value value = jcrProperty.getValue();
            int propType = jcrProperty.getType();
            Object result = convert(value, propType);

            return result;
        } catch (Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    @Override
    public Object[] getValues(UnitOfWork transaction, Property property) throws Exception {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            int propType = jcrProperty.getType();
            Value[] values = jcrProperty.getValues();
            Object[] objectValues = new Object[values.length];
            int i = 0;

            for (Value value : values) {
                objectValues[i++] = convert(value, propType);
            }

            return objectValues;
        } catch (Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    @Override
    public Boolean getBoolean(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            boolean result = getSession(transaction).getProperty(property.getAbsolutePath()).getBoolean();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Boolean[] getBooleanValues(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value[] values = jcrProperty.getValues();
            Boolean[] booleanValues = new Boolean[values.length];
            int i = 0;

            for (Value value : values) {
                booleanValues[i++] = value.getBoolean();
            }

            return booleanValues;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public String getString(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            String result = getSession(transaction).getProperty(property.getAbsolutePath()).getString();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public String[] getStringValues(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value[] values = jcrProperty.getValues();
            String[] stringValues = new String[values.length];
            int i = 0;

            for (Value value : values) {
                stringValues[i++] = value.getString();
            }

            return stringValues;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public String getReference(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            String result = getSession(transaction).getProperty(property.getAbsolutePath()).getString();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public String[] getReferenceValues(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value[] values = jcrProperty.getValues();
            String[] stringValues = new String[values.length];
            int i = 0;

            for (Value value : values) {
                stringValues[i++] = value.getString();
            }

            return stringValues;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Long getLong(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            long result = getSession(transaction).getProperty(property.getAbsolutePath()).getLong();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Long[] getLongValues(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value[] values = jcrProperty.getValues();
            Long[] longValues = new Long[values.length];
            int i = 0;

            for (Value value : values) {
                longValues[i++] = value.getLong();
            }

            return longValues;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Double getDouble(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            double result = getSession(transaction).getProperty(property.getAbsolutePath()).getDouble();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Double[] getDoubleValues(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value[] values = jcrProperty.getValues();
            Double[] doubleValues = new Double[values.length];
            int i = 0;

            for (Value value : values) {
                doubleValues[i++] = value.getDouble();
            }

            return doubleValues;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Integer getInteger(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            double result = getSession(transaction).getProperty(property.getAbsolutePath()).getDouble();
            return (int)Math.round(result);
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Integer[] getIntegerValues(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value[] values = jcrProperty.getValues();
            Integer[] intValues = new Integer[values.length];
            int i = 0;

            for (Value value : values) {
                double doubleValue = value.getDouble();
                int intValue = (int)Math.round(doubleValue);
                intValues[i++] = intValue;
            }

            return intValues;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Calendar getDate(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            Calendar result = getSession(transaction).getProperty(property.getAbsolutePath()).getDate();
            return result;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public Calendar[] getDateValues(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            javax.jcr.Property jcrProperty = getSession(transaction).getProperty(property.getAbsolutePath());
            Value[] values = jcrProperty.getValues();
            Calendar[] dateValues = new Calendar[values.length];
            int i = 0;

            for (Value value : values) {
                dateValues[i++] = value.getDate();
            }

            return dateValues;
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public InputStream getBinary(UnitOfWork transaction, Property property) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            Binary result = getSession(transaction).getProperty(property.getAbsolutePath()).getBinary();
            if (result == null)
                return null;

            return result.getStream();
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    @Override
    public InputStream[] getBinaryValues(UnitOfWork transaction, Property property) throws KException {
        //
        // Not supported at this time
        //
        throw new UnsupportedOperationException();
    }

    @Override
    public void setValue(UnitOfWork transaction, Property property, Object... values) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(property, "property");

        try {
            Session session = getSession(transaction);
            javax.jcr.Property jcrProperty = session.getProperty(property.getAbsolutePath());

            if (values == null) {
                jcrProperty.remove();
            } else {
                int count = values.length;
                boolean multiple = jcrProperty.isMultiple();
                int type = jcrProperty.getType();

                if (count == 0) {
                    // remove if property is multi-valued
                    if (multiple) {
                        jcrProperty.remove();
                    } else {
                        // single-valued property
                        throw new KException(Messages.getString(Komodo.UNABLE_TO_REMOVE_SINGLE_VALUE_PROPERTY_WITH_EMPTY_ARRAY,
                                                                property.getAbsolutePath(),
                                                                getParent(transaction, property)));
                    }
                } else if (count > 1) {
                    if (multiple) {
                        jcrProperty.setValue(createValues(session.getValueFactory(), values, type));
                    } else {
                        throw new KException(Messages.getString(Komodo.UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
                                                                jcrProperty.getName(),
                                                                getParent(transaction, property)));
                    }
                } else {
                    // only one value so set property
                    if (multiple) {
                        jcrProperty.setValue(createValues(session.getValueFactory(), values, type));
                    } else {
                        jcrProperty.setValue(createValue(session.getValueFactory(), values[0]));
                    }
                }
            }
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }

    private void setSingleValuedProperty(Node node, ValueFactory factory, String name, Object propValue, int propertyType)
        throws Exception {

        // Allow property to be created if it does not exist
        boolean hasProperty = node.hasProperty(name);
        if (!hasProperty) {
            node.setProperty(name, createValue(factory, propValue, propertyType));
            return;
        }

        javax.jcr.Property property = node.getProperty(name);

        Value oldValue = property.getValue();
        Value newValue = createValue(factory, propValue, propertyType);

        if (!oldValue.equals(newValue)) {
            boolean doIt = true;

            // for Value references the toString and getString are different. so for current values we must use getString to compare
            if (PropertyType.REFERENCE == propertyType || PropertyType.WEAKREFERENCE == propertyType) {
                doIt = !oldValue.getString().equals(newValue.toString());
            }

            if (doIt) {
                node.setProperty(name, newValue);
            }
        }
    }

    private void setMultiValuedProperty(Session session, Node node, ValueFactory factory, String name, Object[] propValues,
                                        int propertyType) throws Exception {

        Value[] values = new Value[propValues.length];
        int ndx = 0;

        for (Object val : propValues) {
            values[ndx++] = createValue(factory, val, propertyType);
        }

        node.setProperty(name, values);
    }

    public void setProperty(UnitOfWork transaction, KomodoObject kObject, String name, Object[] values) throws KException {
        checkTransaction(transaction);
        ArgCheck.isNotNull(kObject, "kObject");
        ArgCheck.isNotEmpty(name, "name");

        try {
            Session session = getSession(transaction);
            Node node = session.getNode(kObject.getAbsolutePath());
            ValueFactory factory = session.getValueFactory();
            boolean exists = node.hasProperty(name);

            // remove property
            if (values == null) {
                if (exists) {
                    node.getProperty(name).remove();
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_PROPERTY_THAT_DOES_NOT_EXIST,
                                                            name,
                                                            kObject.getAbsolutePath()));
                }
            } else {
                // must be an array at this point
                int count = values.length;

                if (exists) {
                    javax.jcr.Property property = node.getProperty(name);
                    int type = property.getType();
                    boolean multiple = property.isMultiple();

                    if (count == 0) {
                        if (multiple) {
                            property.remove();
                        } else {
                            throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_SINGLE_VALUE_PROPERTY_WITH_EMPTY_ARRAY,
                                                                    name,
                                                                    kObject.getAbsolutePath()));
                        }
                    } else if (count > 1) {
                        if (multiple) {
                            setMultiValuedProperty(session, node, factory, name, values, type);
                        } else {
                            throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
                                                                    name,
                                                                    kObject.getAbsolutePath()));
                        }
                    } else {
                        // remove if value is null or empty string
                        if ((values[0] == null) || ((values[0] instanceof String) && StringUtils.isBlank((String)values[0]))) {
                            node.getProperty(name).remove();
                        } else {
                            if (multiple) {
                                setMultiValuedProperty(session, node, factory, name, values, type);
                            } else {
                                setSingleValuedProperty(node, factory, name, values[0], type);
                            }
                        }
                    }
                } else {
                    // property does not exist and no values being set
                    if (count == 0) {
                        throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_PROPERTY_THAT_DOES_NOT_EXIST,
                                                                name,
                                                                kObject.getAbsolutePath()));
                    }

                    // try and find property descriptor
                    PropertyDescriptor propDescriptor = nodeFactory.getPropertyDescriptor(transaction, kObject, name);

                    if (propDescriptor == null) {
                        // could not find a property descriptor so assume if more than one value it is multi-valued
                        if (count > 1) {
                            setMultiValuedProperty(session, node, factory, name, values, PropertyType.UNDEFINED);
                        } else {
                            if ((values[0] != null)
                                && ((!(values[0] instanceof String)) || !StringUtils.isBlank((String)values[0]))) {
                                setSingleValuedProperty(node, factory, name, values[0], PropertyType.UNDEFINED);
                            }
                        }
                    } else if (propDescriptor.isMultiple()) {
                        int propType = convert(propDescriptor.getType());
                        setMultiValuedProperty(session, node, factory, name, values, propType);
                    } else {
                        int propType = convert(propDescriptor.getType());
                        if ((values[0] != null)
                            && ((!(values[0] instanceof String)) || !StringUtils.isBlank((String)values[0]))) {
                            setSingleValuedProperty(node, factory, name, values[0], propType);
                        }
                    }
                }
            }
        } catch (Exception ex) {
            throw handleError(ex);
        }
    }
}
