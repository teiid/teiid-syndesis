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

import java.util.Collection;

import org.komodo.core.internal.repository.KObjectFactory;
import org.komodo.core.internal.repository.Repository;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a {@link KomodoObject Komodo object} {@link Descriptor type definition}.
 */
public class DescriptorImpl implements Descriptor {

    final String name;
    final Repository repository;
    final UnitOfWork transaction;

    /**
     * @param descriptorRepository
     *        the repository where the descriptor is located (cannot be <code>null</code>)
     * @param descriptorName
     *        the descriptor name (cannot be empty)
     */
    public DescriptorImpl( final UnitOfWork transaction,
    					   final Repository descriptorRepository,
                           final String descriptorName ) {
        ArgCheck.isNotNull( descriptorRepository, "descriptorRepository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$

        this.repository = descriptorRepository;
        this.name = descriptorName;
        this.transaction = transaction;
    }

    public KObjectFactory getNodeFactory() {
        return this.repository.getObjectFactory();
    }

    public KPropertyFactory getPropertyFactory() {
        return this.repository.getPropertyFactory();
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj ) {
        if ( ( obj == null ) || !Descriptor.class.isInstance( obj ) ) {
            return false;
        }

        final Descriptor that = ( Descriptor )obj;
        return this.name.equals( that.getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Descriptor#getName()
     */
    @Override
    public String getName() {
        return this.name;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.Descriptor#getPropertyDescriptors()
     */
    @Override
    public PropertyDescriptor[] getPropertyDescriptors( ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        try {
            Collection<PropertyDescriptor> propDescriptors = getNodeFactory().getPropertyDescriptors(transaction, this);
            return propDescriptors.toArray(new PropertyDescriptor[0]);
        } catch ( final Exception e ) {
            if ( e instanceof KException ) {
                throw ( KException )e;
            }

            throw new KException( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return this.name.hashCode();
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.name;
    }

}
