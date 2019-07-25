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

import java.util.ArrayList;
import java.util.List;

import org.komodo.core.LexiconConstants.JcrLexicon;
import org.komodo.core.LexiconConstants.NTLexicon;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.Descriptor;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.Messages;
import org.komodo.core.repository.Messages.Komodo;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.Property;
import org.komodo.core.repository.PropertyDescriptor;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;

/**
 * A base implementation of a relational object.
 */
public abstract class RelationalObjectImpl extends ObjectImpl implements RelationalObject, KomodoObject {
	
    /**
     * A filter to use when deciding which properties and descriptors apply to an object.
     */
    public interface Filter {

        /**
         * @param descriptorName
         *        the name of the descriptor being checked (cannot be empty)
         * @return <code>true</code> if the descriptor should be reject
         */
        boolean rejectDescriptor( final String descriptorName );

        /**
         * @param propName
         *        the name of the property being checked (cannot be empty)
         * @return <code>true</code> if this property should be rejected
         */
        boolean rejectProperty( final String propName );

    }

    /**
     * A filter to exclude specific DDL-namespaced properties and type descriptors.
     */
    static final Filter DDL_QNAMES_FILTER = new ExcludeQNamesFilter( StandardDdlLexicon.DDL_EXPRESSION,
                                                        StandardDdlLexicon.DDL_LENGTH,
                                                        StandardDdlLexicon.DEFAULT_PRECISION,
                                                        StandardDdlLexicon.DEFAULT_OPTION,
                                                        StandardDdlLexicon.DDL_ORIGINAL_EXPRESSION,
                                                        StandardDdlLexicon.DDL_START_CHAR_INDEX,
                                                        StandardDdlLexicon.DDL_START_COLUMN_NUMBER,
                                                        StandardDdlLexicon.DDL_START_LINE_NUMBER );

    /**
     * A filter to exclude JCR-namespaced properties and type descriptors.
     */
    static final Filter JCR_FILTER = new ExcludeNamespaceFilter( JcrLexicon.Namespace.PREFIX, JcrLexicon.Namespace.URI );

    /**
     * An empty collection of filters.
     */
    Filter[] NO_FILTERS = new Filter[ 0 ];

    /**
     * A filter to exclude NT-namespaced properties and type descriptors.
     */
    static final Filter NT_FILTER = new ExcludeNamespaceFilter( NTLexicon.Namespace.PREFIX, NTLexicon.Namespace.URI );

    /**
     * A filter to exclude residual properties and type descriptors.
     */
    static final Filter RESIDUAL_FILTER = new Filter() {

        private String NAME = "*"; //$NON-NLS-1$

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.RelationalObject.Filter#rejectProperty(java.lang.String)
         */
        @Override
        public boolean rejectProperty( final String propName ) {
            ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
            return NAME.equals( propName );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.RelationalObject.Filter#rejectDescriptor(java.lang.String)
         */
        @Override
        public boolean rejectDescriptor( final String descriptorName ) {
            ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$
            return NAME.equals( descriptorName );
        }

    };

    /**
     * The default set of filters for restricting which properties and descriptors apply to relational objects.
     */
    protected static final Filter[] DEFAULT_FILTERS = new Filter[] { DDL_QNAMES_FILTER, JCR_FILTER, NT_FILTER, RESIDUAL_FILTER };

    private static TypeResolverRegistry _resolverRegistry;

    protected static final KLog LOGGER = KLog.getLogger();

    /**
     * Indicates if the initial state after construction should be validated.
     */
    public static final boolean VALIDATE_INITIAL_STATE = true;

    private Filter[] filters = DEFAULT_FILTERS;

    protected RelationalObjectImpl( final UnitOfWork uow,
                                    final Repository repository,
                                    final String path ) throws KException {
        this( uow, repository, path, 0 );
    }

    protected RelationalObjectImpl( final UnitOfWork transaction,
                                    final Repository repository,
                                    final String path,
                                    final int index ) throws KException {
        super(transaction, repository, path, index );

        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        internalValidateInitialState( this );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChild(java.lang.String)
     */
    @Override
    public KomodoObject getChild( final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject kobject = super.getChild( name );
        final KomodoObject result = resolveType( kobject );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChild(java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if ( !StringUtils.isBlank( typeName ) && isDescriptorFiltered( typeName ) ) {
            throw new KException( Messages.getString( Messages.Komodo.CHILD_NOT_FOUND, name, getAbsolutePath() ) );
        }

        final KomodoObject kobject = super.getChild( name, typeName );
        final KomodoObject result = resolveType( kobject );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildren(java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;
        final KomodoObject[] kids = super.getChildren( namePatterns );

        if ( kids.length == 0 ) {
            result = kids;
        } else {
            final List< KomodoObject > temp = new ArrayList<>( kids.length );

            for ( final KomodoObject kobject : kids ) {
                // ensure child has at least one non-filtered descriptor
                for ( final Descriptor descriptor : getAllDescriptors( getTransaction(), kobject ) ) {
                    if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                        temp.add( resolveType( kobject ) );
                        break;
                    }
                }
            }

            result = temp.toArray( new KomodoObject[ temp.size() ] );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getChildrenOfType(java.lang.String, java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( isDescriptorFiltered( type ) ) {
            result = KomodoObject.EMPTY_ARRAY;
        } else {
            final KomodoObject[] kids = super.getChildrenOfType( type, namePatterns );

            if ( kids.length == 0 ) {
                result = kids;
            } else {
                final List< KomodoObject > temp = new ArrayList<>( kids.length );

                for ( final KomodoObject kobject : kids ) {
                    // ensure child has at least one non-filtered descriptor
                    for ( final Descriptor descriptor : getAllDescriptors( getTransaction(), kobject ) ) {
                        if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                            temp.add( resolveType( kobject ) );
                            break;
                        }
                    }
                }

                result = temp.toArray( new KomodoObject[ temp.size() ] );
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getDescriptor(java.lang.String)
     */
    @Override
    public Descriptor getDescriptor(
                                     final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Descriptor result = null;

        if ( !isDescriptorFiltered( typeName ) ) {
            result = new FilteredDescriptor( super.getDescriptor( typeName ) );
        }

        if ( result == null ) {
            throw new KException( Messages.getString( Komodo.DESCRIPTOR_NOT_FOUND, typeName, getAbsolutePath() ) );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getDescriptors()
     */
    @Override
    public Descriptor[] getDescriptors( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Descriptor[] temp = super.getDescriptors( );
        final List< Descriptor > result = new ArrayList<>( temp.length );

        if ( ( temp.length != 0 ) && ( getFilters().length != 0 ) ) {
            for ( final Descriptor descriptor : temp ) {
                if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                    result.add( new FilteredDescriptor( descriptor ) );
                }
            }
        }

        return result.toArray( new Descriptor[ result.size() ] );
    }

    /**
     * @return the filters to use when deciding which {@link PropertyDescriptor properties} and {@link Descriptor descriptors} are
     *         valid for this object (never <code>null</code> but can be empty)
     */
    public Filter[] getFilters() {
        assert (this.filters != null);
        return this.filters;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getParent()
     */
    @Override
    public KomodoObject getParent( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject result = super.getParent( );

        if ( result != null ) {
            return resolveType( result );
        }

        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getProperty(java.lang.String)
     */
    @Override
    public Property getProperty(
                                 final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        Property result = null;

        if ( !isPropertyFiltered( name ) ) {
            result = super.getProperty( name );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getPropertyDescriptors()
     */
    @Override
    public PropertyDescriptor[] getPropertyDescriptors( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final PropertyDescriptor[] descriptors = super.getPropertyDescriptors( );
        final List< PropertyDescriptor > result = new ArrayList<>( descriptors.length );

        for ( final PropertyDescriptor descriptor : descriptors ) {
            if ( !isPropertyFiltered( descriptor.getName() ) ) {
                result.add( descriptor );
            }

        }

        return result.toArray( new PropertyDescriptor[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getPropertyNames()
     */
    @Override
    public String[] getPropertyNames( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] props = super.getPropertyNames( );
        final List< String > result = new ArrayList<>( props.length );

        for ( final String propName : props ) {
            if ( !isPropertyFiltered( propName ) ) {
                result.add( propName );
            }
        }

        return result.toArray( new String[ result.size() ] );
    }

    private TypeResolverRegistry getResolverRegistry() {
        if (_resolverRegistry == null)
            _resolverRegistry = TypeResolverRegistry.getInstance();

        return _resolverRegistry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#hasChild(java.lang.String)
     */
    @Override
    public boolean hasChild( final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        boolean result = super.hasChild( name );

        if ( result ) {
            result = false;

            // if one of the children with that name has a type that is not filtered return true
            for ( final KomodoObject kobject : getChildren( name ) ) {
                for ( final Descriptor descriptor : getAllDescriptors( getTransaction(), kobject ) ) {
                    if ( !isDescriptorFiltered( descriptor.getName() ) ) {
                        result = true;
                        break;
                    }
                }

                if ( result ) {
                    break;
                }
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#hasChild(java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        boolean result = false;

        if ( !isDescriptorFiltered( typeName ) ) {
            result = super.hasChild( name, typeName );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#hasChildren()
     */
    @Override
    public boolean hasChildren( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if ( super.hasChildren( ) ) {
            return ( getChildren( ).length != 0 ); // filtered children > 0
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#hasDescriptor(java.lang.String)
     */
    @Override
    public boolean hasDescriptor(
                                  final String descriptorName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptorName );

        boolean result = false;

        if ( !isDescriptorFiltered( descriptorName ) ) {
            result = super.hasDescriptor( descriptorName );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#hasProperty(java.lang.String)
     */
    @Override
    public boolean hasProperty(
                                final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        boolean result = false;

        if ( !isPropertyFiltered( name ) ) {
            result = super.hasProperty( name );
        }

        return result;
    }

    private final void internalValidateInitialState(
                                                     final KomodoObject kobject ) throws KException {
        assert ( getTransaction() != null );

        if ( VALIDATE_INITIAL_STATE ) {
            validateInitialState( kobject );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#isChildRestricted()
     */
    @Override
    public boolean isChildRestricted() {
        return false;
    }

    private boolean isDescriptorFiltered( final String descriptorName ) {
        assert !StringUtils.isBlank( descriptorName );

        if ( getFilters().length != 0 ) {
            for ( final Filter filter : getFilters() ) {
                if ( filter.rejectDescriptor( descriptorName ) ) {
                    return true;
                }
            }
        }

        return false;
    }

    private boolean isPropertyFiltered( final String propName ) {
        assert !StringUtils.isBlank( propName );

        if ( getFilters().length != 0 ) {
            for ( final Filter filter : getFilters() ) {
                if ( filter.rejectProperty( propName ) ) {
                    return true;
                }
            }
        }

        return false;
    }

    protected KomodoObject resolveType(
                                        final KomodoObject kobject ) throws KException {
        TypeResolver< ? > resolver = getResolverRegistry().getResolver(kobject.getTypeIdentifier());
        if (resolver != null && resolver.resolvable(kobject))
            return resolver.resolve( kobject );

        // Failed with the type identifier so try to be safe than sorry
        // and iterate through all resolvers to check this object is really
        // not resolvable.
        for ( final TypeResolver< ? > aResolver : getResolverRegistry().getResolvers() ) {
            if ( aResolver.resolvable( kobject ) ) {
                return aResolver.resolve( kobject );
            }
        }

        return (KomodoObject)kobject;
    }

    /**
     * @param newFilters
     *        the new set of filters to use when deciding which {@link PropertyDescriptor properties} and {@link Descriptor
     *        descriptors} are valid for this object (can be <code>null</code>)
     */
    public void setFilters( final Filter[] newFilters ) {
        this.filters = ( ( newFilters == null ) ? NO_FILTERS : newFilters );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#setPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public final void setPrimaryType( final UnitOfWork uow,
                                      final String typeName ) {
        throw new UnsupportedOperationException( Messages.getString( Relational.PROPERTY_NOT_MODIFIABLE,
        		JcrConstants.JCR_PRIMARY_TYPE ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#toString()
     */
    @Override
    public String toString() {
        return getAbsolutePath();
    }

    /**
     * @param uow
     *        the rollback only transaction (never <code>null</code>)
     * @param kobject
     *        the object being checked (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    protected void validateInitialState(
                                         final KomodoObject kobject ) throws KException {
        final TypeResolver< ? > resolver = getResolverRegistry().getResolver( kobject.getClass() );

        if ( ( resolver != null ) && !resolver.resolvable( kobject ) ) {
            throw new KException( Messages.getString( Komodo.INCORRECT_TYPE,
                                                      kobject.getAbsolutePath(),
                                                      kobject.getClass().getSimpleName() ) );
        }
    }

    class FilteredDescriptor implements Descriptor {

        private final Descriptor delegate;

        FilteredDescriptor( final Descriptor delegate ) {
            this.delegate = delegate;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.core.repository.Descriptor#getName()
         */
        @Override
        public String getName() {
            return delegate.getName();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.core.repository.Descriptor#getPropertyDescriptors()
         */
        @Override
        public PropertyDescriptor[] getPropertyDescriptors( ) throws KException {
            final PropertyDescriptor[] descriptors = this.delegate.getPropertyDescriptors();

            if ( descriptors.length == 0 ) {
                return descriptors;
            }

            final List< PropertyDescriptor > result = new ArrayList<>( descriptors.length );

            for ( final PropertyDescriptor descriptor : descriptors ) {
                if ( !RelationalObjectImpl.this.isPropertyFiltered( descriptor.getName() ) ) {
                    result.add( descriptor );
                }
            }

            return result.toArray( new PropertyDescriptor[ result.size() ] );
        }

    }
    
}
