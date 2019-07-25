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

import java.util.ArrayList;
import java.util.List;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.Property;
import org.komodo.core.repository.PropertyValueType;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.dataservice.SqlComposition;
import org.komodo.relational.dataservice.SqlProjectedColumn;
import org.komodo.relational.dataservice.ViewDefinition;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of an view definition
 */
public class ViewDefinitionImpl extends RelationalObjectImpl implements ViewDefinition {
	
    /**
     * An empty array of sql compositions.
     */
    static final SqlCompositionImpl[] NO_SQL_COMPOSITIONS = new SqlCompositionImpl[0];
	
    /**
     * An empty array of sql compositions.
     */
    final static SqlProjectedColumnImpl[] NO_SQL_PROJECTED_COLUMNS = new SqlProjectedColumnImpl[0];
	
    /**
     * The resolver of a {@link ViewDefinition}.
     */
    public static final TypeResolver<ViewDefinitionImpl> RESOLVER = new TypeResolver<ViewDefinitionImpl>() {

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
        public Class<ViewDefinitionImpl> owningClass() {
            return ViewDefinitionImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(kobject, KomodoLexicon.ViewDefinition.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public ViewDefinitionImpl resolve(final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == ViewDefinition.TYPE_ID) {
                return (ViewDefinitionImpl)kobject;
            }

            return new ViewDefinitionImpl(kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath());
        }

    };
    
    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { SqlComposition.IDENTIFIER};

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
	public ViewDefinitionImpl(UnitOfWork uow, Repository repository, String path) throws KException {
		super(uow, repository, path);
	}

    @Override
    public KomodoType getTypeIdentifier() {
        return ViewDefinition.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent()
     */
    @Override
    public ViewEditorStateImpl getParent() throws KException {
        ArgCheck.isNotNull(getTransaction(), "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((getTransaction().getState() == State.NOT_STARTED), "transaction state must be NOT_STARTED"); //$NON-NLS-1$

        final KomodoObject grouping = super.getParent();
        final ViewEditorStateImpl result = ViewEditorStateImpl.RESOLVER.resolve(grouping.getParent());
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
     * @see org.komodo.core.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }
    
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ViewDefinition#addSqlComposition(UnitOfWork, String)
     */
    @Override
    public SqlComposition addSqlComposition(String compositionName) throws KException {
        return RelationalModelFactory.createSqlComposition( getTransaction(), getRepository(), this, compositionName);
    }

    private KomodoObject getSqlCompositionsGroupingNode() {
        try {
            final KomodoObject[] groupings = getRawChildren( getTransaction(), KomodoLexicon.ViewDefinition.SQL_COMPOSITIONS );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    private KomodoObject getSqlProjectedColumnsGroupingNode() {
        try {
            final KomodoObject[] groupings = getRawChildren( getTransaction(), KomodoLexicon.ViewDefinition.SQL_PROJECTED_COLUMNS );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ViewDefinition#getSqlCompositions(UnitOfWork, String...)
     */
    @Override
    public SqlCompositionImpl[] getSqlCompositions(final String... namePatterns) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getSqlCompositionsGroupingNode();

        if ( grouping != null ) {
            final List< SqlCompositionImpl > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( namePatterns ) ) {
                final SqlCompositionImpl gitRepo = new SqlCompositionImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
                temp.add( gitRepo );
            }

            return temp.toArray( new SqlCompositionImpl[ temp.size() ] );
        }

        return NO_SQL_COMPOSITIONS;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ViewDefinition#getSqlCompositions(UnitOfWork, String...)
     */
    @Override
    public void removeSqlComposition(String sqlCompositionToRemove) throws KException {
        ArgCheck.isNotNull(getTransaction(), "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((getTransaction().getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(sqlCompositionToRemove, "sqlCompositionToRemove"); //$NON-NLS-1$

        final SqlCompositionImpl[] sqlCompositions = getSqlCompositions(sqlCompositionToRemove);

        if (sqlCompositions.length == 0) {
            throw new KException(Messages.getString(Relational.SQL_COMPOSITION_NOT_FOUND_TO_REMOVE, sqlCompositionToRemove));
        }

        // remove first occurrence
        sqlCompositions[0].remove(getTransaction());
    }

	@Override
	public String getDescription() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        
        String desc = getObjectProperty(getTransaction(), PropertyValueType.STRING, "getDescription", //$NON-NLS-1$
                KomodoLexicon.ViewDefinition.DESCRIPTION );
		return desc;
	}

	@Override
	public String getDdl() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        
        String desc = getObjectProperty(getTransaction(), PropertyValueType.STRING, "getDdl", //$NON-NLS-1$
                KomodoLexicon.ViewDefinition.DDL );
		return desc;
	}

	@Override
	public String[] getSourcePaths(final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Property property = getProperty( KomodoLexicon.ViewDefinition.SOURCE_PATHS );

        if ( property == null ) {
            return StringConstants.EMPTY_ARRAY;
        }

        final boolean matchPattern = ( ( namePatterns != null ) && ( namePatterns.length != 0 ) );
        final List< String > paths = new ArrayList< >();

        for ( final String value : property.getStringValues( getTransaction() ) ) {
            if ( matchPattern ) {
                for ( final String pattern : namePatterns ) {
                    // convert pattern to a regex
                    final String regex = pattern.replace( "*", ".*" ); //$NON-NLS-1$ //$NON-NLS-2$

                    if ( value.matches( regex ) ) {
                    	paths.add( value );
                    }
                }
            } else {
            	paths.add( value );
            }
        }

        return paths.toArray( new String[ paths.size() ] );
	}

	@Override
	public String getViewName() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        
        String name = getObjectProperty(getTransaction(), PropertyValueType.STRING, "getViewName", //$NON-NLS-1$
                KomodoLexicon.ViewDefinition.VIEW_NAME );
		return name;
	}
	
	@Override
	public void setViewName(String name) throws KException {
		setObjectProperty( getTransaction(), "setViewName", KomodoLexicon.ViewDefinition.VIEW_NAME, name ); //$NON-NLS-1$
	}

	@Override
	public void setDescription(String description) throws KException {
		setObjectProperty( getTransaction(), "setDescription", KomodoLexicon.ViewDefinition.DESCRIPTION, description ); //$NON-NLS-1$
	}

	@Override
	public void setDdl(String ddl) throws KException {
		setObjectProperty( getTransaction(), "setDdl", KomodoLexicon.ViewDefinition.DDL, ddl ); //$NON-NLS-1$
	}

	@Override
	public String[] removeSourcePath(String sourcePathToRemove) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( sourcePathToRemove, "roleNameToRemove" ); //$NON-NLS-1$

        final String[] current = getSourcePaths( );

        if ( current.length == 0 ) {
            throw new KException( Messages.getString( Relational.MAPPED_ROLE_NOT_FOUND_TO_REMOVE, sourcePathToRemove ) );
        }

        final String[] result = new String[ current.length - 1 ];
        boolean found = false;
        int i = 0;

        for ( final String sourcePath : current ) {
            if ( sourcePath.equals( sourcePathToRemove ) ) {
                found = true;
            } else {
                result[i++] = sourcePath;
            }
        }

        if ( !found ) {
            throw new KException( Messages.getString( Relational.SOURCE_PATH_NOT_FOUND_TO_REMOVE, sourcePathToRemove ) );
        }

        final Object[] newValue = ( ( result.length == 0 ) ? null : result );
        setProperty( KomodoLexicon.ViewDefinition.SOURCE_PATHS, newValue );

        return result;
		
	}

	@Override
	public String[] addSourcePath(String sourcePathToAdd) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( sourcePathToAdd, "sourcePathToAdd" ); //$NON-NLS-1$

        String[] result = null;
        final String[] current = getSourcePaths( );
        int i = 0;

        if ( current.length == 0 ) {
            // this is first mapped role name
            result = new String[ 1 ];
        } else {
            // add to existing (make sure it doesn't already exist)
            result = new String[ current.length + 1 ];

            for ( final String sourcePath : current ) {
                if ( sourcePath.equals( sourcePathToAdd ) ) {
                    throw new KException( Messages.getString( Relational.DUPLICATE_SOURCE_PATH, sourcePathToAdd ) );
                }

                result[i++] = sourcePath;
            }
        }

        result[i] = sourcePathToAdd;
        setProperty( KomodoLexicon.ViewDefinition.SOURCE_PATHS, ( Object[] )result );

        return result;
	}

	@Override
	public void setComplete(boolean complete) throws KException {
		setObjectProperty( getTransaction(), "setComplete", KomodoLexicon.ViewDefinition.IS_COMPLETE, complete ); //$NON-NLS-1$
	}

	@Override
	public boolean isComplete() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        
        final Boolean value = getObjectProperty(getTransaction(), PropertyValueType.BOOLEAN, "isComplete", //$NON-NLS-1$
                                                             KomodoLexicon.ViewDefinition.IS_COMPLETE );
		return value;
	}

	@Override
	public void setUserDefined(boolean userDefined) throws KException {
		setObjectProperty( getTransaction(), "setUserDefined", KomodoLexicon.ViewDefinition.IS_USER_DEFINED, userDefined ); //$NON-NLS-1$
	}

	@Override
	public boolean isUserDefined() throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        
        final Boolean value = getObjectProperty(getTransaction(), PropertyValueType.BOOLEAN, "isUserDefined", //$NON-NLS-1$
                                                             KomodoLexicon.ViewDefinition.IS_USER_DEFINED );
		return value;
	}

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ViewDefinition#addSqlComposition(UnitOfWork, String)
     */
    @Override
    public SqlProjectedColumn addProjectedColumn(String columnName) throws KException {
        return RelationalModelFactory.createSqlProjectedColumn( getTransaction(), getRepository(), this, columnName);
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.ViewDefinition#removeProjectedColumn(UnitOfWork, String...)
     */
    @Override
    public void removeProjectedColumn(String columnToRemove) throws KException {
        ArgCheck.isNotNull(getTransaction(), "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((getTransaction().getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(columnToRemove, "columnToRemove"); //$NON-NLS-1$

        final SqlProjectedColumnImpl[] columns = getProjectedColumns(columnToRemove);

        if (columns.length == 0) {
            throw new KException(Messages.getString(Relational.PROJECTED_COLUMN_NOT_FOUND_TO_REMOVE, columnToRemove));
        }

        // remove first occurrence
        columns[0].remove(getTransaction());
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.profile.ViewDefinition#getProjectedColumns(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public SqlProjectedColumnImpl[] getProjectedColumns(String... namePatterns) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getSqlProjectedColumnsGroupingNode( );

        if ( grouping != null ) {
            final List< SqlProjectedColumnImpl > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( namePatterns ) ) {
                final SqlProjectedColumnImpl gitRepo = new SqlProjectedColumnImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
                temp.add( gitRepo );
            }

            return temp.toArray( new SqlProjectedColumnImpl[ temp.size() ] );
        }

        return NO_SQL_PROJECTED_COLUMNS;
    }

}
