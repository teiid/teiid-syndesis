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
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of an user profile.
 */
public class ProfileImpl extends RelationalObjectImpl implements Profile {
	
	/**
     * An empty array of view editor states.
     */
    final static ViewEditorStateImpl[] NO_VIEW_EDITOR_STATES = new ViewEditorStateImpl[0];
	
    /**
     * The resolver of a {@link Profile}.
     */
    public static final TypeResolver< ProfileImpl > RESOLVER = new TypeResolver< ProfileImpl >() {

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
        public Class< ProfileImpl > owningClass() {
            return ProfileImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( kobject, KomodoLexicon.Profile.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public ProfileImpl resolve( final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Profile.TYPE_ID ) {
                return ( ProfileImpl )kobject;
            }

            return new ProfileImpl( kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath() );
        }
    };


    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { ViewEditorState.IDENTIFIER };

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
    public ProfileImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier() {
        return Profile.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(java.lang.String)
     */
    @Override
    public boolean hasChild( final String name ) throws KException {
        if ( KomodoLexicon.Profile.VIEW_EDITOR_STATES.equals( name ) ) {
            return false; // use hasRawChild
        }

        if ( KomodoLexicon.Profile.GIT_REPOSITORIES.equals( name ) ) {
            return false; // use hasRawChild
        }

        return ( super.hasChild( name ) ||
                            getViewEditorState( name ) != null );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildren(final String... namePatterns) throws KException {
        ArgCheck.isNotNull(getTransaction(), "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((getTransaction().getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        final KomodoObject[] viewEditorStates = getViewEditorStates(namePatterns);

        final KomodoObject[] result = new KomodoObject[viewEditorStates.length];
        System.arraycopy(viewEditorStates, 0, result, 0, viewEditorStates.length);

        return result;
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
    public ViewEditorStateImpl addViewEditorState(String stateId) throws KException {
        // first delete if already exists
        if ( getViewEditorState( stateId ) != null ) {
            removeViewEditorState( stateId );
        }

        return RelationalModelFactory.createViewEditorState( getTransaction(), getRepository(), this, stateId );
    }

    private KomodoObject getViewEditorStatesGroupingNode() {
        try {
            final KomodoObject[] groupings = getRawChildren( getTransaction(), KomodoLexicon.Profile.VIEW_EDITOR_STATES );

            if ( groupings.length == 0 ) {
                return null;
            }

            return groupings[ 0 ];
        } catch ( final KException e ) {
            return null;
        }
    }

    @Override
    public ViewEditorStateImpl[] getViewEditorStates(String... namePatterns) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getViewEditorStatesGroupingNode( );

        if ( grouping != null ) {
            final List< ViewEditorStateImpl > temp = new ArrayList<>();

            for ( final KomodoObject kobject : grouping.getChildren( namePatterns ) ) {
                final ViewEditorStateImpl gitRepo = new ViewEditorStateImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
                temp.add( gitRepo );
            }

            return temp.toArray( new ViewEditorStateImpl[ temp.size() ] );
        }

        return NO_VIEW_EDITOR_STATES;
    }
    
    @Override
    public ViewEditorStateImpl getViewEditorState(String name) throws KException {
        ArgCheck.isNotNull( getTransaction(), "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( getTransaction().getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject grouping = getViewEditorStatesGroupingNode();

        if ( grouping != null && grouping.hasChild( name ) ) {
        	KomodoObject kobject = grouping.getChild( name );
            return new ViewEditorStateImpl( getTransaction(), getRepository(), kobject.getAbsolutePath() );
        }

        return null;
    }

    @Override
    public void removeViewEditorState(String viewEditorStateId) throws KException {
        ArgCheck.isNotNull(getTransaction(), "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((getTransaction().getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(viewEditorStateId, "viewEditorStateId"); //$NON-NLS-1$

        ViewEditorStateImpl state = getViewEditorState(viewEditorStateId);

        if (state == null) {
            throw new KException(Messages.getString(Relational.VIEW_EDITOR_STATE_NOT_FOUND_TO_REMOVE, viewEditorStateId));
        }

        // remove first occurrence
        state.remove(getTransaction());
    }
}
