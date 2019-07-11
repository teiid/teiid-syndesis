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
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.ViewDefinition;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of an view definition
 */
public class SqlCompositionImpl  extends RelationalObjectImpl implements SqlComposition {
	
    /**
     * The resolver of a {@link SqlComposition}.
     */
    public static final TypeResolver<SqlComposition> RESOLVER = new TypeResolver<SqlComposition>() {

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
        public Class<SqlCompositionImpl> owningClass() {
            return SqlCompositionImpl.class;
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
        public SqlComposition resolve(final UnitOfWork transaction, final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == SqlComposition.TYPE_ID) {
                return (SqlComposition)kobject;
            }

            return new SqlCompositionImpl(transaction, RepositoryImpl.getRepository(transaction), kobject.getAbsolutePath());
        }

    };

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { SqlComposition.IDENTIFIER };

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
	public SqlCompositionImpl(UnitOfWork uow, Repository repository, String path) throws KException {
		super(uow, repository, path);
	}

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return SqlComposition.IDENTIFIER;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ViewDefinition getParent(final UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state must be NOT_STARTED"); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent(transaction);
        final ViewDefinition result = ViewDefinitionImpl.RESOLVER.resolve(transaction, grouping.getParent(transaction));
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
	public void setDescription(UnitOfWork transaction, String description) throws KException {
		setObjectProperty(transaction, "setDescription", KomodoLexicon.SqlComposition.DESCRIPTION, description); //$NON-NLS-1$
	}

	@Override
	public String getDescription(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		"getDescription", //$NON-NLS-1$
                KomodoLexicon.SqlComposition.DESCRIPTION );
		return value;
	}

	@Override
	public void setLeftSourcePath(UnitOfWork transaction, String leftSourcePath) throws KException {
		setObjectProperty(transaction, "setLeftSourcePath", KomodoLexicon.SqlComposition.LEFT_SOURCE_PATH, leftSourcePath); //$NON-NLS-1$
	}

	@Override
	public String getLeftSourcePath(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		"getLeftSourcePath", //$NON-NLS-1$
                KomodoLexicon.SqlComposition.LEFT_SOURCE_PATH );
		return value;
	}

	@Override
	public void setRightSourcePath(UnitOfWork transaction, String rightSourcePath) throws KException {
		setObjectProperty(transaction, "setRightSourcePath", KomodoLexicon.SqlComposition.RIGHT_SOURCE_PATH, rightSourcePath); //$NON-NLS-1$
	}

	@Override
	public String getRightSourcePath(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		"getRightSourcePath", //$NON-NLS-1$
                KomodoLexicon.SqlComposition.RIGHT_SOURCE_PATH );
		return value;
	}

	@Override
	public void setType(UnitOfWork transaction, String type) throws KException {
		setObjectProperty(transaction, "setType", KomodoLexicon.SqlComposition.TYPE, type); //$NON-NLS-1$
	}

	@Override
	public String getType(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		"getType", //$NON-NLS-1$
                KomodoLexicon.SqlComposition.TYPE );
		return value;
	}

	@Override
	public void setOperator(UnitOfWork transaction, String operator) throws KException {
		setObjectProperty(transaction, "setOperator", KomodoLexicon.SqlComposition.OPERATOR, operator); //$NON-NLS-1$
	}

	@Override
	public String getOperator(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		"getOperator", //$NON-NLS-1$
                KomodoLexicon.SqlComposition.OPERATOR );
		return value;
	}

	@Override
	public void setLeftCriteriaColumn(UnitOfWork transaction, String leftCriteriaColumn) throws KException {
		setObjectProperty(transaction, "setLeftCriteriaColumn", KomodoLexicon.SqlComposition.LEFT_CRITERIA_COLUMN, leftCriteriaColumn); //$NON-NLS-1$
	}

	@Override
	public String getLeftCriteriaColumn(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		"getLeftCriteriaColumn", //$NON-NLS-1$
                KomodoLexicon.SqlComposition.LEFT_CRITERIA_COLUMN );
		return value;
	}

	@Override
	public void setRightCriteriaColumn(UnitOfWork transaction, String rightCriteriaColumn) throws KException {
		setObjectProperty(transaction, "setRightCriteriaColumn", KomodoLexicon.SqlComposition.RIGHT_CRITERIA_COLUMN, rightCriteriaColumn); //$NON-NLS-1$
	}

	@Override
	public String getRightCriteriaColumn(UnitOfWork transaction) throws KException {
        String value = getObjectProperty(transaction, PropertyValueType.STRING, 
        		"getRightCriteriaColumn", //$NON-NLS-1$
                KomodoLexicon.SqlComposition.RIGHT_CRITERIA_COLUMN );
		return value;
	}

}
