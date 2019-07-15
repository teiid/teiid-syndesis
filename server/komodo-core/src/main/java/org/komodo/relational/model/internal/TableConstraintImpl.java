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
import org.komodo.core.repository.Property;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.ExcludeQNamesFilter;
import org.komodo.relational.internal.RelationalChildRestrictedObject;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;

/**
 * A base implementation of a relational model table constraint.
 */
abstract class TableConstraintImpl extends RelationalChildRestrictedObject implements TableConstraint {

    /**
     * A filter to exclude specific, readonly properties.
     */
    private static final Filter PROPS_FILTER = new ExcludeQNamesFilter( TeiidDdlLexicon.Constraint.TYPE );

    protected TableConstraintImpl( final UnitOfWork uow,
                                   final Repository repository,
                                   final String path ) throws KException {
        super(uow, repository, path);

        // add in filter to hide the constraint type
        final Filter[] updatedFilters = new Filter[ DEFAULT_FILTERS.length + 1 ];
        System.arraycopy( DEFAULT_FILTERS, 0, updatedFilters, 0, DEFAULT_FILTERS.length );
        updatedFilters[ DEFAULT_FILTERS.length ] = PROPS_FILTER;
        setFilters( updatedFilters );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#addColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void addColumn( final UnitOfWork transaction,
                           final Column columnToAdd ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( columnToAdd, "columnToAdd" ); //$NON-NLS-1$

        String[] newRefs = null;
        final Property property = getProperty( transaction, Constraint.REFERENCES );
        final String columnId = getObjectFactory().getId(transaction, columnToAdd).getStringValue( transaction );

        if ( property == null ) {
            newRefs = new String[] { columnId };
        } else {
            // add to existing multi-valued property
            final String[] columnRefs = property.getStringValues( transaction );
            newRefs = new String[ columnRefs.length + 1 ];
            System.arraycopy( columnRefs, 0, newRefs, 0, columnRefs.length );
            newRefs[columnRefs.length] = columnId;
        }

        setProperty( transaction, Constraint.REFERENCES, ( Object[] )newRefs );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getColumns(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Column[] getColumns( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Repository repository = getRepository();
        Column[] result = null;
        final Property property = getProperty( transaction, Constraint.REFERENCES );

        if ( property == null ) {
            result = new Column[ 0 ];
        } else {
            final String[] columnRefs = property.getStringValues( transaction );
            result = new Column[ columnRefs.length ];
            int i = 0;

            for ( final String columnId : columnRefs ) {
                final KomodoObject kobject = repository.getUsingId( transaction, columnId );

                if ( kobject == null ) {
                    throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
                }

                result[i] = new ColumnImpl( transaction, repository, kobject.getAbsolutePath() );
                ++i;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#getTable(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public TableImpl getTable( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        TableImpl result = null;
        final KomodoObject parent = getParent( transaction );

        if ( parent.hasDescriptor( transaction, CreateTable.TABLE_STATEMENT ) ) {
            result = new TableImpl( transaction, getRepository(), parent.getAbsolutePath() );
        } else if ( parent.hasDescriptor( transaction, CreateTable.VIEW_STATEMENT ) ) {
            result = new ViewImpl( transaction, getRepository(), parent.getAbsolutePath() );
        } else {
            throw new KException( Messages.getString( Relational.UNEXPECTED_TABLE_TYPE ) );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.TableConstraint#removeColumn(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.model.Column)
     */
    @Override
    public void removeColumn( final UnitOfWork transaction,
                              final Column columnToRemove ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( columnToRemove, "columnToRemove" ); //$NON-NLS-1$

        final String columnId = getObjectFactory().getId(transaction, columnToRemove).getStringValue( transaction );
        final Column[] current = getColumns( transaction );

        if ( current.length == 0 ) {
            throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
        }

        boolean found = false;
        final String[] updatedRefs = new String[ current.length - 1 ];
        int i = 0;

        for ( final Column column : current ) {
            if ( column.equals( columnToRemove ) ) {
                found = true;
            } else {
                updatedRefs[i] = getObjectFactory().getId(transaction, column).getStringValue( transaction );
                ++i;
            }
        }

        if ( found ) {
            setProperty( transaction, Constraint.REFERENCES, ( Object[] )updatedRefs );
        } else {
            throw new KException( Messages.getString( Relational.REFERENCED_COLUMN_NOT_FOUND, columnId ) );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#setProperty(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.Object[])
     */
    @Override
    public void setProperty( final UnitOfWork transaction,
                             final String propertyName,
                             final Object... values ) throws KException {
        if ( PROPS_FILTER.rejectProperty( propertyName ) ) {
            throw new UnsupportedOperationException( Messages.getString( Relational.PROPERTY_NOT_MODIFIABLE, propertyName ) );
        }

        super.setProperty( transaction, propertyName, values );
    }

}
