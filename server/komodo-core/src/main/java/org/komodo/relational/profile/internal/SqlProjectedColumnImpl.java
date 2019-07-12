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
package org.komodo.relational.profile.internal;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of a SQL projected column
 */
public class SqlProjectedColumnImpl  extends RelationalObjectImpl implements SqlProjectedColumn {
	
    /**
     * The resolver of a {@link SqlProjectedColumn}.
     */
    public static final TypeResolver<SqlProjectedColumnImpl> RESOLVER = new TypeResolver<SqlProjectedColumnImpl>() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class<SqlProjectedColumnImpl> owningClass() {
            return SqlProjectedColumnImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(transaction, kobject, KomodoLexicon.SqlComposition.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public SqlProjectedColumnImpl resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == SqlProjectedColumn.TYPE_ID) {
                return (SqlProjectedColumnImpl)kobject;
            }

            return new SqlProjectedColumnImpl(transaction, RepositoryImpl.getRepository(transaction), kobject.getAbsolutePath());
        }

    };

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { SqlProjectedColumn.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param path
     *        the path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a model
     */
	public SqlProjectedColumnImpl(UnitOfWork uow, Repository repository, String path) throws KException {
		super(uow, repository, path);
	}

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return SqlProjectedColumn.IDENTIFIER;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ViewDefinitionImpl getParent(final UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state must be NOT_STARTED"); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent(transaction);
        final ViewDefinitionImpl result = ViewDefinitionImpl.RESOLVER.resolve(transaction, grouping.getParent(transaction));
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

	@Override
	public void setName(UnitOfWork transaction, String name) throws KException {
		setObjectProperty(transaction, "setName", KomodoLexicon.SqlProjectedColumn.NAME, name); //$NON-NLS-1$
	}

	@Override
	public String getName(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		                                      "getName", //$NON-NLS-1$
                                                      KomodoLexicon.SqlProjectedColumn.NAME );
		return value;
	}

	@Override
	public void setType(UnitOfWork transaction, String type) throws KException {
		setObjectProperty(transaction, "setType", KomodoLexicon.SqlProjectedColumn.TYPE, type); //$NON-NLS-1$
	}

	@Override
	public String getType(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		                                      "getType", //$NON-NLS-1$
                                                      KomodoLexicon.SqlProjectedColumn.TYPE );
		return value;
	}
	
    @Override
    public void setSelected(UnitOfWork transaction, boolean complete) throws KException {
        setObjectProperty( transaction, "setComplete", KomodoLexicon.SqlProjectedColumn.IS_SELECTED, complete ); //$NON-NLS-1$
    }

    @Override
    public boolean isSelected(UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        
        final Boolean value = getObjectProperty(transaction, PropertyValueType.BOOLEAN, 
                                                             "isSelected", //$NON-NLS-1$
                                                             KomodoLexicon.SqlProjectedColumn.IS_SELECTED );
        return value;
    }

}
