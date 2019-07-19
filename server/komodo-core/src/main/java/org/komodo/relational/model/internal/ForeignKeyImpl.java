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

import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.Property;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;

/**
 * An implementation of a relational model foreign key.
 */
public final class ForeignKeyImpl extends TableConstraintImpl implements ForeignKey {
	
    /**
     * The resolver of a {@link ForeignKey}.
     */
    public static final TypeResolver< ForeignKeyImpl > RESOLVER = new TypeResolver< ForeignKeyImpl >() {

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
        public Class< ForeignKeyImpl > owningClass() {
            return ForeignKeyImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final KomodoObject kobject ) throws KException {
        	UnitOfWork transaction = kobject.getTransaction();
            return ObjectImpl.validateType( kobject, Constraint.FOREIGN_KEY_CONSTRAINT )
                   && ObjectImpl.validatePropertyValue( transaction,
                		   kobject.getRepository(),
                                                        kobject,
                                                        Constraint.TYPE,
                                                        CONSTRAINT_TYPE.toValue() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public ForeignKeyImpl resolve( final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == ForeignKey.TYPE_ID ) {
                return ( ForeignKeyImpl )kobject;
            }
            UnitOfWork transaction = kobject.getTransaction();
            return new ForeignKeyImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace relative path (cannot be empty)
     * @throws KException
     *         if an error occurs or if node at specified path is not a foreign key
     */
    public ForeignKeyImpl( final UnitOfWork uow,
                           final Repository repository,
                           final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier() {
        return ForeignKey.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#addReferencesColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void addReferencesColumn(
                                     final Column newReferencesColumn ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( newReferencesColumn, "newReferencesColumn" ); //$NON-NLS-1$

        String[] newValue = null;
        final Property property = getProperty( Constraint.TABLE_REFERENCE_REFERENCES );
        final String columnId = getObjectFactory().getId(getTransaction(), newReferencesColumn).getStringValue( getTransaction() );

        if ( property == null ) {
            newValue = new String[ 1 ];
            newValue[0] = columnId;
        } else {
            final String[] columnRefs = property.getStringValues( getTransaction() );
            newValue = new String[ columnRefs.length + 1 ];
            System.arraycopy( columnRefs, 0, newValue, 0, columnRefs.length );
            newValue[columnRefs.length] = columnId;
        }

        setProperty( Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )newValue );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getConstraintType()
     */
    @Override
    public ConstraintType getConstraintType() {
        return ConstraintType.FOREIGN_KEY;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#getReferencesColumns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Column[] getReferencesColumns() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Repository repository = getRepository();
        Column[] result = null;

        final Property property = getProperty( Constraint.TABLE_REFERENCE_REFERENCES );

        if ( property == null ) {
            result = new Column[ 0 ];
        } else {
            final String[] columnRefs = property.getStringValues( getTransaction() );
            result = new Column[ columnRefs.length ];
            int i = 0;

            for ( final String columnId : columnRefs ) {
                final KomodoObject kobject = repository.getUsingId( getTransaction(), columnId );

                if ( kobject == null ) {
                    throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
                }

                result[i] = new ColumnImpl( getTransaction(), repository, kobject.getAbsolutePath() );
                ++i;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#getReferencesTable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public TableImpl getReferencesTable() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        TableImpl result = null;
        final Property property = getProperty( Constraint.TABLE_REFERENCE );

        if ( property != null ) {
            final String tableId = property.getStringValue( getTransaction() );
            final KomodoObject kobject = getRepository().getUsingId( getTransaction(), tableId );

            if ( kobject == null ) {
                throw new KException( Messages.getString( Relational.REFERENCED_TABLE_NOT_FOUND, tableId ) );
            }

            result = new TableImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent()
     */
    @Override
    public TableImpl getParent() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject parent = super.getParent( );
        final TableImpl result = TableImpl.RESOLVER.resolve( parent );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#removeReferencesColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void removeReferencesColumn(
                                        final Column removeReferencesColumn ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( removeReferencesColumn, "removeReferencesColumn" ); //$NON-NLS-1$

        final String columnId = getObjectFactory().getId(getTransaction(), removeReferencesColumn).getStringValue( getTransaction() );
        final Column[] current = getReferencesColumns( );

        if ( current.length == 0 ) {
            throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
        }

        setProperty( Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )null );

        boolean found = false;
//        final Column[] updated = new Column[ current.length - 1 ];
//        int i = 0;

        for ( final Column column : current ) {
            if ( column.equals( removeReferencesColumn ) ) {
                found = true;
            } else {
                addReferencesColumn( column );
//                updated[i] = column;
//                ++i;
            }
        }

        if ( found ) {
//            setProperty( transaction, Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )updated );
        } else {
            throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.ForeignKey#setReferencesTable(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Table)
     */
    @Override
    public void setReferencesTable(
                                    final Table newReferencesTable ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( newReferencesTable, "newReferencesTable" ); //$NON-NLS-1$

        // only set if different
        final TableImpl current = getReferencesTable( );

        if ( !newReferencesTable.equals( current ) ) {
            final String tableId = getObjectFactory().getId(getTransaction(), newReferencesTable).getStringValue( getTransaction() );
            setProperty( Constraint.TABLE_REFERENCE, tableId );

            if ( ( current != null ) && hasProperty( Constraint.TABLE_REFERENCE_REFERENCES ) ) {
                // remove reference columns because they pertained to previous referenced table
                setProperty( Constraint.TABLE_REFERENCE_REFERENCES, ( Object[] )null );
            }
        }
    }
    
    @Override
    public TableImpl getRelationalParent() throws KException {
    	return getParent();
    }

}
