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

import org.komodo.core.internal.repository.Repository;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * The base class for a {@link Property Komodo object property}.
 */
public class PropertyImpl extends AbstractKNode implements Property {

    /**
     * An empty array of values.
     */
    public static final Object[] NO_VALUES = new Object[0];

    /**
     * @param propertyRepository
     *        the repository where this property is located (cannot be <code>null</code>)
     * @param propertyPath
     *        the property path (cannot be empty)
     * @throws KException
     *         if there is an error constructing the property
     */
    public PropertyImpl( final UnitOfWork transaction,
    					 final Repository propertyRepository,
                         final String propertyPath ) throws KException {
    	super(propertyRepository, propertyPath);
    }

    public KPropertyFactory getPropertyFactory() {
        return this.repository.getPropertyFactory();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getAbsolutePath()
     */
    @Override
    public String getAbsolutePath() {
        return this.path;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getBinaryValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public InputStream getBinaryValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            InputStream stream = getPropertyFactory().getBinary(transaction, this);
            if (stream == null)
                return null;

            return stream;

        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getBooleanValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Boolean getBooleanValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getBoolean(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getBooleanValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Boolean[] getBooleanValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getBooleanValues(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getDateValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Calendar getDateValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getDate(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getDateValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Calendar[] getDateValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getDateValues(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getDoubleValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Double getDoubleValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getDouble(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getDoubleValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Double[] getDoubleValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getDoubleValues(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getIntegerValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Integer getIntegerValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getInteger(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getIntegerValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Integer[] getIntegerValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getIntegerValues(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getLongValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Long getLongValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getLong(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getLongValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Long[] getLongValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getLongValues(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KNode#getName()
     */
    @Override
    public String getName( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getName(getTransaction(), this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KNode#getParent()
     */
    @Override
    public KomodoObject getParent( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getParent(getTransaction(), this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KNode#getRepository()
     */
    public Repository getRepository() {
        return this.repository;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getStringValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getStringValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getString(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getStringValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getStringValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getStringValues(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getValue(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Object getValue( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getValue(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getValues(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Object[] getValues( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().getValues(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getDescriptor(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyDescriptor getDescriptor( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
    
        try {
            return getPropertyFactory().getPropertyDescriptor(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }
    
            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#getValueType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public PropertyValueType getValueType( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        return getPropertyFactory().getType(transaction, this);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#isMultiple(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isMultiple( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            return getPropertyFactory().isMultiple(transaction, this);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Property#set(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.Object[])
     */
    @Override
    public void set( final UnitOfWork transaction,
                     final Object... values ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            getPropertyFactory().setValue(transaction, this, values);
        } catch (final Exception e) {
            if (e instanceof KException) {
                throw (KException)e;
            }

            throw new KException(e);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.path;
    }

}
