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
package org.komodo.relational.internal;

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;

/**
 * A base implementation of a relational object that does not permit children.
 */
public abstract class RelationalChildRestrictedObject extends RelationalObjectImpl {

    protected RelationalChildRestrictedObject( final UnitOfWork uow,
                                               final Repository repository,
                                               final String path ) throws KException {
        super( uow, repository, path );
    }

    protected RelationalChildRestrictedObject( final UnitOfWork uow,
                                               final Repository repository,
                                               final String path,
                                               final int index ) throws KException {
        super( uow, repository, path, index );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public final KomodoType[] getChildTypes() {
        return KomodoType.NO_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#addChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     * @throws UnsupportedOperationException
     *         if this method is called
     */
    @Override
    public final KomodoObject addChild( final UnitOfWork uow,
                                        final String name,
                                        final String primaryType ) {
        throw new UnsupportedOperationException( "Children cannot be added to objects of type " + getClass().getName() ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public final KomodoObject[] getChildren( final UnitOfWork uow,
                                             final String... namePatterns ) {
        return KomodoObject.EMPTY_ARRAY;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String[])
     */
    @Override
    public final KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                                   final String type,
                                                   final String... namePatterns ) {
        return KomodoObject.EMPTY_ARRAY;
    }

    /**
     * {@inheritDoc}
     *
     * @return <code>false</code>
     * @see org.komodo.core.repository.ObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public final boolean hasChild( final UnitOfWork uow,
                                   final String name ) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @return <code>false</code>
     * @see org.komodo.core.repository.ObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final boolean hasChildren( final UnitOfWork uow ) {
        return false;
    }
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#isChildRestricted()
     */
    @Override
    public final boolean isChildRestricted() {
        return true;
    }

}
