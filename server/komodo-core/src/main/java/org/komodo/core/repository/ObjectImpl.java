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


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.komodo.core.internal.repository.KObjectFactory;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoTypeRegistry.TypeIdentifier;
import org.komodo.spi.KException;
import org.komodo.spi.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.modeshape.sequencer.ddl.DdlConstants;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlConstants;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;

/**
 * An implementation of a {@link KomodoObject Komodo object}.
 */
public class ObjectImpl extends AbstractKNode implements KomodoObject, StringConstants {

    private static final KLog LOGGER = KLog.getLogger();

    protected static Descriptor[] getAllDescriptors( final UnitOfWork transaction,
                                                     final KomodoObject kobject ) throws KException {
        assert ( transaction != null );

        final Descriptor[] mixins = kobject.getDescriptors( );
        final Descriptor[] allDescriptors = new Descriptor[ mixins.length + 1 ];
        System.arraycopy( mixins, 0, allDescriptors, 0, mixins.length );
        allDescriptors[mixins.length] = kobject.getPrimaryType( );
        return allDescriptors;
    }

    /**
     * Wraps error in a {@link KException} if necessary.
     *
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error (never <code>null</code>)
     */
    public static KException handleError( final Exception e ) {
        assert ( e != null );

        if ( e instanceof KException ) {
            return ( KException )e;
        }

        return new KException( e );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param kobject
     *        the object whose property value is being validated (cannot be empty)
     * @param name
     *        the name of the property being validated (cannot be empty)
     * @param expectedValue
     *        the expected value or <code>null</code> if the property should not exist
     * @return <code>true</code> if the property value is the expected value
     * @throws KException
     *         if an error occurs
     */
    public static boolean validatePropertyValue( final UnitOfWork transaction,
                                                 final Repository repository,
                                                 final KomodoObject kobject,
                                                 final String name,
                                                 final Object expectedValue ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        boolean valid = true;
        final Property property = kobject.getRawProperty( transaction, name );

        if ( property == null ) {
            if ( expectedValue != null ) {
                valid = ( ( expectedValue instanceof String ) && StringUtils.isBlank( ( String )expectedValue ) );
            }
        } else {
            valid = property.getValue( transaction ).equals( expectedValue );
        }

        return valid;
    }

    /**
     * @param kobject
     *        the object whose type is being validated (cannot be empty)
     * @param types
     *        the primary type or descriptor names that the object must have (cannot be <code>null</code> or empty or have a
     *        <code>null</code> element)
     * @return <code>true</code> if object is resolvable to the specified type(s)
     * @throws KException
     *         if an error occurs or if object does not have all the specified types
     */
    public static boolean validateType( final KomodoObject kobject,
                                        final String... types ) throws KException {
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
        UnitOfWork transaction = kobject.getTransaction();
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( types, "types" ); //$NON-NLS-1$

        boolean result = true;

        for ( final String type : types ) {
            ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

            if ( !kobject.hasDescriptor( type ) && !type.equals( kobject.getPrimaryType( ).getName() ) ) {
                result = false;
                break;
            }
        }

        return result;
    }

    protected int index;

    /**
     * @param komodoRepository
     *        the repository where the object is located (cannot be <code>null</code>)
     * @param path
     *        the workspace path (cannot be <code>null</code> or empty)
     * @param index
     *        the object index (value is zero for non-SNS)
     */
    public ObjectImpl( final UnitOfWork transaction,
    				   final Repository komodoRepository,
                       final String path,
                       final int index ) {
    	super(komodoRepository, path);
        this.index = index;
    }

    protected void provision(UnitOfWork transaction, OperationType operationType) throws KException {
        getRepository().provision(transaction, this, operationType);
    }

    public KObjectFactory getObjectFactory() {
        return this.repository.getObjectFactory();
    }

    public KPropertyFactory getPropertyFactory() {
        return this.repository.getPropertyFactory();
    }

    private void internalSetProperty( final UnitOfWork transaction,
                                      final String name,
                                      final Object... values ) throws Exception {
        provision(transaction, OperationType.MODIFY_OPERATION);

        getObjectFactory().setProperty(transaction, this, name, values);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#isChildRestricted()
     */
    @Override
    public boolean isChildRestricted() {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#addChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public KomodoObject addChild( final UnitOfWork transaction,
                                  final String name,
                                  final String primaryType ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        provision(transaction, OperationType.CHILD_OPERATION);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-addChild: transaction = {0}, name = {1}, primaryType = {2}", //$NON-NLS-1$
                         transaction.getName(),
                         name,
                         primaryType);
        }

        final String type = (StringUtils.isBlank(primaryType) ? JcrConstants.NT_UNSTRUCTURED : primaryType);

        try {
            KomodoObject result = getObjectFactory().addChild(transaction, this, name, type);

            if ( LOGGER.isDebugEnabled() ) {
                LOGGER.debug( "objectimpl-addChild: transaction = {0}, path = {1}", transaction.getName(), result.getAbsolutePath() ); //$NON-NLS-1$
            }

            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#addDescriptor(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public void addDescriptor( final UnitOfWork transaction,
                               final String... descriptorNames ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(descriptorNames, "descriptorNames"); //$NON-NLS-1$

        provision(transaction, OperationType.MODIFY_OPERATION);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-addDescriptor: transaction = {0}, descriptorNames = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         Arrays.asList(descriptorNames));
        }

        try {
            getObjectFactory().addDescriptor(transaction, this, descriptorNames);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object object ) {
        if (object instanceof KomodoObject) {
            return this.path.equals(((ObjectImpl)object).path);
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KNode#getAbsolutePath()
     */
    @Override
    public String getAbsolutePath() {
        return this.path;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getChild(java.lang.String)
     */
    @Override
    public KomodoObject getChild( final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        provision(getTransaction(), OperationType.READ_OPERATION);

        try {
            final KomodoObject result = getObjectFactory().getChild(getTransaction(), this, name);

            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getChild(java.lang.String, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final String name,
                                  final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        provision(getTransaction(), OperationType.READ_OPERATION);

        { // try if one child with that name first
            final KomodoObject kobject = getChild( name );

            // verify type is right
            if ( typeName.equals( kobject.getPrimaryType( ).getName() )
                 || kobject.hasDescriptor( typeName ) ) {
                return kobject;
            }
        }

        // see if multiple children have same name
        final KomodoObject[] kids = getChildren( name );

        if ( kids.length != 0 ) {
            for ( final KomodoObject kid : kids ) {
                if ( typeName.equals( kid.getPrimaryType( ).getName() ) || kid.hasDescriptor( typeName ) ) {
                    return kid;
                }
            }
        }

        throw new KException( Messages.getString( Messages.Komodo.CHILD_NOT_FOUND, name, getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getChildren(java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren( final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject[] result = getRawChildren( getTransaction(), namePatterns );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getChildrenOfType(java.lang.String,
     *      java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

        provision(getTransaction(), OperationType.READ_OPERATION);

        try {
            Collection<KomodoObject> children = getObjectFactory().getChildren(getTransaction(), this, namePatterns);
            if (!children.isEmpty()) {
                final List<KomodoObject> matches = new ArrayList<>(children.size());

                for (final KomodoObject child : children) {
                    if (type.equals(child.getPrimaryType().getName()) || child.hasDescriptor(type)) {
                        matches.add(child);
                    }
                }

                children = matches;
            }

            return children.toArray(new KomodoObject[0]);
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return KomodoType.NO_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getDescriptor(java.lang.String)
     */
    @Override
    public Descriptor getDescriptor( final String typeName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        provision(getTransaction(), OperationType.READ_OPERATION);

        try {
            Descriptor result = getObjectFactory().getDescriptor(getTransaction(), this, typeName);
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getDescriptors()
     */
    @Override
    public Descriptor[] getDescriptors( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Descriptor[] result = getRawDescriptors( getTransaction() );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getIndex()
     */
    @Override
    public int getIndex() {
        return this.index;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KNode#getName()
     */
    @Override
    public String getName( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(getTransaction(), OperationType.READ_OPERATION);

        try {
            final String result = getObjectFactory().getName(getTransaction(), this);
            return result;
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KNode#getParent()
     */
    @Override
    public KomodoObject getParent( ) throws KException {
        return getRawParent( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getPrimaryType()
     */
    @Override
    public Descriptor getPrimaryType( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(getTransaction(), OperationType.READ_OPERATION);

        try {
            return getObjectFactory().getType(getTransaction(), this);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * Convenience method for retrieving a property.
     *
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param returnValueType
     *        the type of the return value type
     * @param getterName
     *        name of the method name calling this method
     * @param propertyPath
     *        relative path of the actual property
     * @return the value of the property cast to the specified return value type
     * @throws KException
     */
    @SuppressWarnings( "unchecked" )
    protected < T > T getObjectProperty( final UnitOfWork transaction,
                                         final PropertyValueType returnValueType,
                                         final String getterName,
                                         final String propertyPath ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        if ( LOGGER.isDebugEnabled() ) {
            LOGGER.debug( getterName + ": transaction = {0}", transaction.getName() ); //$NON-NLS-1$
        }

        T result = null;
        Property property = getProperty( propertyPath );

        if ( property != null ) {
            switch ( returnValueType ) {
                case STRING:
                    result = ( T )property.getStringValue( transaction );
                    break;
                case LONG:
                    result = ( T )Long.valueOf( property.getLongValue( transaction ) );
                    break;
                case INTEGER:
                    result = ( T )Integer.valueOf( Long.valueOf( property.getLongValue( transaction ) ).intValue() );
                    break;
                case DOUBLE:
                    result = ( T )Double.valueOf( property.getDoubleValue( transaction ) );
                    break;
                case BOOLEAN:
                    result = ( T )Boolean.valueOf( property.getBooleanValue( transaction ) );
                    break;
                case DATE:
                    result = ( T )property.getDateValue( transaction );
                    break;
                default:
                    throw new UnsupportedOperationException( "Further property types should be added for support in this method" ); //$NON-NLS-1$
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getProperty(java.lang.String)
     */
    @Override
    public Property getProperty( final String name ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        final Property result = getRawProperty( getTransaction(), name );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getPropertyDescriptors()
     */
    @Override
    public PropertyDescriptor[] getPropertyDescriptors( ) throws KException {
        final PropertyDescriptor[] result = getRawPropertyDescriptors( getTransaction() );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getPropertyNames()
     */
    @Override
    public String[] getPropertyNames( ) throws KException {
        final String[] result = getRawPropertyNames( getTransaction() );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getRawChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public final KomodoObject[] getRawChildren( final UnitOfWork transaction,
                                                final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        try {
            Collection<KomodoObject> children = getObjectFactory().getChildren(transaction, this, namePatterns);            
            return children.toArray(new KomodoObject[0]);
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getRawDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final Descriptor[] getRawDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        try {
            Collection<Descriptor> results = getObjectFactory().getDescriptors(transaction, this);
            return results.toArray(new Descriptor[0]);
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getRawParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final KomodoObject getRawParent( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        if (RepositoryImpl.KOMODO_ROOT.equals( getAbsolutePath() )) {
            return null;
        }

        try {
            return getObjectFactory().getParent(transaction, this);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getRawProperty(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public final Property getRawProperty( final UnitOfWork transaction,
                                          final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        //
        // Normally all reserved paths should return no properties
        //
        if ( RepositoryImpl.isReservedPath(getAbsolutePath())) {
            return null;
        }

        try {
            return getObjectFactory().getProperty(transaction, this, name);
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getRawPropertyDescriptors(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final PropertyDescriptor[] getRawPropertyDescriptors( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        if ( RepositoryImpl.isReservedPath(getAbsolutePath() ) ) {
            return PropertyDescriptor.NO_DESCRIPTORS;
        }

        final List< PropertyDescriptor > result = new ArrayList<>();

        for ( final Descriptor descriptor : getAllDescriptors( transaction, this ) ) {
            result.addAll( Arrays.asList( descriptor.getPropertyDescriptors( ) ) );
        }

        return result.toArray( new PropertyDescriptor[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getRawPropertyNames(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final String[] getRawPropertyNames( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        if ( RepositoryImpl.isReservedPath(getAbsolutePath() ) ) {
            return StringConstants.EMPTY_ARRAY;
        }

        try {
            Collection<String> names = getObjectFactory().getPropertyNames(transaction, this);
            return names.toArray(new String[0]);
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KNode#getRepository()
     */
    public Repository getRepository() {
        return this.repository;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return getClass().hashCode();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#getTypeIdentifier()
     */
    @Override
    public KomodoType getTypeIdentifier( ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(getTransaction(), OperationType.READ_OPERATION);

        List<Descriptor> descriptors = new ArrayList<Descriptor>();

        descriptors.add(getPrimaryType());
        descriptors.addAll(Arrays.asList(getDescriptors()));

        KomodoTypeRegistry registry  = KomodoTypeRegistry.getInstance();
        Set<TypeIdentifier> identifiers = new HashSet<>();
        for (Descriptor descriptor : descriptors) {
            String name = descriptor.getName();
            identifiers.addAll(registry.getIdentifiers(name));
        }

        KomodoType result = KomodoType.UNKNOWN;
        if (identifiers.isEmpty()) {
            // No identifiers but could be DDL Statements container
            String nodeName = getName();
            if (StandardDdlLexicon.STATEMENTS_CONTAINER.equals(nodeName))
                result = KomodoType.DDL_SCHEMA;

        } else if( identifiers.size() == 1 ) {
            result = identifiers.iterator().next().getKomodoType();
        } else {

            // Multiple identifiers all with the same lexiconType
            String lexiconType = identifiers.iterator().next().getLexiconType();

            if (TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT.equals(lexiconType)) {
                /*
                 * TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT
                 *
                 * * STORED_PROCEDURE
                 * * VIRTUAL_PROCEDURE
                 */

                // If identifiers contains virtual procedure then its one of those,
                // otherwise its a stored procedure.
                result = KomodoType.STORED_PROCEDURE;
                for (TypeIdentifier identifier : identifiers) {
                    if (KomodoType.VIRTUAL_PROCEDURE.equals(identifier.getKomodoType())) {
                        result = KomodoType.VIRTUAL_PROCEDURE;
                        break;
                    }
                }

            } else if (TeiidDdlLexicon.Constraint.TABLE_ELEMENT.equals(lexiconType)) {
                /*
                 * TeiidDdlLexicon.Constraint.TABLE_ELEMENT
                 *
                 * * ACCESS_PATTERN
                 * * COLUMN
                 * * PRIMARY_KEY
                 * * UNIQUE_CONSTRAINT
                 */
                String accessPatternConstraint = TeiidDdlConstants.TeiidNonReservedWord.ACCESSPATTERN.toDdl();
                String primaryKeyConstraint = DdlConstants.PRIMARY_KEY;
                String uniqueConstraint = TeiidDdlConstants.TeiidReservedWord.UNIQUE.toDdl();

                Property constProperty = getRawProperty(getTransaction(), TeiidDdlLexicon.Constraint.TYPE);
                if (constProperty != null) {
                    String constType = constProperty.getStringValue(getTransaction());
                    if (accessPatternConstraint.equals(constType))
                        result = KomodoType.ACCESS_PATTERN;
                    else if (primaryKeyConstraint.equals(constType))
                        result = KomodoType.PRIMARY_KEY;
                    else if (uniqueConstraint.equals(constType))
                        result = KomodoType.UNIQUE_CONSTRAINT;
                    else
                        result = KomodoType.COLUMN;
                }
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasChild(java.lang.String)
     */
    @Override
    public boolean hasChild( final String name ) throws KException {
        return hasRawChild( getTransaction(), name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasChild(java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final String name,
                             final String typeName ) throws KException {
        return hasRawChild( getTransaction(), name, typeName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasChildren()
     */
    @Override
    public boolean hasChildren( ) throws KException {
        return hasRawChildren( getTransaction() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasDescriptor(java.lang.String)
     */
    @Override
    public boolean hasDescriptor( final String descriptorName ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptorName );

        provision(getTransaction(), OperationType.READ_OPERATION);

        boolean result = false;

        for ( final Descriptor descriptor : getDescriptors( ) ) {
            if ( descriptorName.equals( descriptor.getName() ) ) {
                result = true;
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return this.path.hashCode();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasProperties()
     */
    @Override
    public boolean hasProperties( ) throws KException {
        return (getPropertyNames().length > 0);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasProperty(java.lang.String)
     */
    @Override
    public boolean hasProperty( final String name ) throws KException {
        return hasRawProperty( getTransaction(), name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasRawChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public final boolean hasRawChild( final UnitOfWork transaction,
                                      final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        try {
            return getObjectFactory().hasChild(transaction, this, name);
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasRawChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    public final boolean hasRawChild( final UnitOfWork transaction,
                                      final String name,
                                      final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        try {
            if ( hasRawChild( transaction, name ) ) {
                for ( final KomodoObject kid : getRawChildren( transaction, name ) ) {
                    if ( typeName.equals( kid.getPrimaryType( ).getName() )
                         || kid.hasDescriptor( typeName ) ) {
                        return true;
                    }
                }
            }
        } catch ( final KException e ) {
            // child not found
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasRawChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public final boolean hasRawChildren( UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        try {
            return getObjectFactory().hasChildren(transaction, this);
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#hasRawProperty(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public final boolean hasRawProperty( final UnitOfWork transaction,
                                         final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        if ( RepositoryImpl.isReservedPath(getAbsolutePath() ) ) {
            return false;
        }

        try {
            return getObjectFactory().hasProperty(transaction, this, name);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#print(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void print( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.READ_OPERATION);

        try {
            getObjectFactory().print(transaction, this);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#remove(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void remove( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.REMOVE_OPERATION);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "objectimpl-remove: transaction = {0}, path = {1}", transaction.getName(), getAbsolutePath() ); //$NON-NLS-1$
        }

        try {
            getObjectFactory().remove(transaction, this);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#removeChild(java.lang.String[])
     */
    @Override
    public void removeChild( final String... names ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(names, "names"); //$NON-NLS-1$

        provision(getTransaction(), OperationType.CHILD_OPERATION);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("objectimpl-removeChild: transaction = {0}, names = {1}", //$NON-NLS-1$
                         getTransaction().getName(),
                         Arrays.asList(names));
        }

        try {
            for (final String name : names) {
                KomodoObject child = getObjectFactory().getChild(getTransaction(), this, name);
                if (child != null) {
                    getObjectFactory().remove(getTransaction(), child);
                } else {
                    throw new KException(Messages.getString(Messages.Komodo.UNABLE_TO_REMOVE_CHILD, names, getAbsolutePath()));
                }
            }
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void rename( final UnitOfWork transaction,
                        final String newName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( newName, "newName" ); //$NON-NLS-1$

        provision(transaction, OperationType.MODIFY_OPERATION);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "objectimpl-rename: transaction = {0}, old path = {1}, new name = {2}", //$NON-NLS-1$
                          transaction.getName(),
                          getAbsolutePath(),
                          newName );
        }

        // If the supplied newName is not an absolute path, assume its a simple name and append the parent absolute path
        String newPath = newName;
        if(!newPath.startsWith(FORWARD_SLASH)) {
        	newPath = getRawParent( transaction ).getAbsolutePath();
        	if(!newPath.endsWith(FORWARD_SLASH)) {
        		newPath += FORWARD_SLASH;
        	}
        	newPath += newName;
        }

        try {
            getObjectFactory().move(transaction, this, newPath);
            this.path = newPath;
            // TODO seems like index could change also
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#setPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setPrimaryType( final UnitOfWork transaction,
                                final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, OperationType.MODIFY_OPERATION);

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setPrimaryType: transaction = {0}, typeName = {1}", transaction.getName(), typeName); //$NON-NLS-1$
        }

        try {
            final String type = (StringUtils.isBlank(typeName) ? JcrConstants.NT_UNSTRUCTURED : typeName);
            getObjectFactory().setType(transaction, this, type);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    private boolean isArray(Object value) {
        return value != null && value.getClass().isArray();
    }

    protected <T> void setObjectProperty( final UnitOfWork transaction,
                                      final String setterName,
                                      final String propertyName,
                                      final T value ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug(setterName + ": transaction = {0}, value = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         value);
        }

        try {
            if (isArray(value)) {
                setProperty(propertyName, (Object[]) value);
            } else {
                setProperty(propertyName, value);
            }
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.KomodoObject#setProperty(java.lang.String, java.lang.Object[])
     */
    @Override
    public void setProperty( final String propertyName,
                             final Object... values ) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty(propertyName, "propertyName"); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("setProperty: transaction = {0}, propertyName = {1}, value(s) = {2}", //$NON-NLS-1$
                         getTransaction().getName(),
                         propertyName,
                         values);
        }

        try {
            internalSetProperty(getTransaction(), propertyName, values);
        } catch (final Exception e) {
            throw handleError( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.path;
    }

    @Override
    public void accept( final UnitOfWork transaction,
                        final KomodoObjectVisitor visitor ) throws Exception {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        provision(transaction, visitor.getRequestType());

        visitor.visit(transaction, this);
    }

}
