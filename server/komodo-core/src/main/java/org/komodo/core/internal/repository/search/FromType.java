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
package org.komodo.core.internal.repository.search;

import org.komodo.core.KomodoLexicon.Search;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * A from type with its requisite alias
 */
class FromType {

    private final String alias;

    private final String type;

    /**
     * Constructor
     * @param type the type
     * @param alias the alias
     */
    public FromType(String type, String alias) {
        ArgCheck.isNotEmpty(type);
        if (alias == null)
            alias = StringConstants.EMPTY_STRING;

        this.type = type;
        this.alias = alias;
    }

    /**
     * @return the alias
     */
    public String getAlias() {
        return this.alias;
    }

    /**
     * @return the type
     */
    public String getType() {
        return this.type;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.alias == null) ? 0 : this.alias.hashCode());
        result = prime * result + ((this.type == null) ? 0 : this.type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        FromType other = (FromType)obj;
        if (this.alias == null) {
            if (other.alias != null)
                return false;
        } else if (!this.alias.equals(other.alias))
            return false;
        if (this.type == null) {
            if (other.type != null)
                return false;
        } else if (!this.type.equals(other.type))
            return false;
        return true;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param searchObject the parent searchObject
     * @throws KException
     *         if an error occurs
     */
    void write(UnitOfWork uow, KomodoObject searchObject) throws KException {
        ArgCheck.isNotNull(uow, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((uow.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotNull(searchObject, "searchObject"); //$NON-NLS-1$

        Repository repository = searchObject.getRepository();
        KomodoObject ftObject = repository.add(uow, searchObject.getAbsolutePath(), Search.FromType.NODE_TYPE, Search.FromType.NODE_TYPE);
        ftObject.setProperty(uow, Search.FromType.TYPE, getType());
        ftObject.setProperty(uow, Search.FromType.ALIAS, getAlias());
    }
}