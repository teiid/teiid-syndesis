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
package org.komodo.relational.model.internal;

import org.komodo.relational.model.Index;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.spi.lexicon.ddl.teiid.TeiidDdlLexicon.Constraint;

/**
 * An implementation of a relational model index.
 */
public final class IndexImpl extends TableConstraintImpl implements Index {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not an index
     */
    public IndexImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Index.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getConstraintType()
     */
    @Override
    public ConstraintType getConstraintType() {
        return ConstraintType.INDEX;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Index#getExpression(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getExpression( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getExpression", Constraint.EXPRESSION); //$NON-NLS-1$
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
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Table getParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( transaction );
        final Table result = Table.RESOLVER.resolve( transaction, parent );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Index#setExpression(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setExpression( final UnitOfWork uow,
                               final String newExpression ) throws KException {
        setObjectProperty(uow, "setExpression", Constraint.EXPRESSION, newExpression); //$NON-NLS-1$
    }

}
