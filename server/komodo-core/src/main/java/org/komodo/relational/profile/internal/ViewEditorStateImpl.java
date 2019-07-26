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
import org.komodo.relational.dataservice.StateCommandAggregate;
import org.komodo.relational.dataservice.ViewEditorState;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;

/**
 * An implementation of a view editor state object.
 */
public class ViewEditorStateImpl extends RelationalObjectImpl implements ViewEditorState {
	
    /**
     * The resolver of a {@link ViewEditorState}.
     */
    public static final TypeResolver<ViewEditorStateImpl> RESOLVER = new TypeResolver<ViewEditorStateImpl>() {

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
        public Class<ViewEditorStateImpl> owningClass() {
            return ViewEditorStateImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public boolean resolvable(final KomodoObject kobject) throws KException {
            return ObjectImpl.validateType(kobject, KomodoLexicon.ViewEditorState.NODE_TYPE);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.core.repository.KomodoObject)
         */
        @Override
        public ViewEditorStateImpl resolve(final KomodoObject kobject) throws KException {
            if (kobject.getTypeId() == ViewEditorState.TYPE_ID) {
                return (ViewEditorStateImpl)kobject;
            }

            return new ViewEditorStateImpl(kobject.getTransaction(), kobject.getRepository(), kobject.getAbsolutePath());
        }

    };


    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { StateCommandAggregate.IDENTIFIER };

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param path
     *        the path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ViewEditorStateImpl(final UnitOfWork uow, final Repository repository, final String path) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier() {
        return ViewEditorState.IDENTIFIER;
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
     * @see org.komodo.relational.model.Model#addSource(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public StateCommandAggregateImpl addCommand() throws KException {
        return RelationalModelFactory.createStateCommandAggregate(getTransaction(), getRepository(), this);
    }

    @Override
    public StateCommandAggregate[] getCommands() throws KException {
        KomodoObject[] commands = getChildrenOfType(KomodoLexicon.StateCommandAggregate.NODE_TYPE);
        if (commands == null)
            return StateCommandAggregate.NO_STATE_COMMAND_AGGREGATES;

        List<StateCommandAggregate> cmdList = new ArrayList<>();
        for (KomodoObject cmd : commands) {
            cmdList.add(new StateCommandAggregateImpl(getTransaction(), getRepository(), cmd.getAbsolutePath()));
        }

        return cmdList.toArray(new StateCommandAggregate[0]);
    }
    

	@Override
	public ViewDefinitionImpl setViewDefinition() throws KException {
        // Create the a new ViewDefinition
        return RelationalModelFactory.createViewDefinition(getTransaction(), getRepository(), this);
	}

	@Override
	public ViewDefinitionImpl getViewDefinition() throws KException {
		KomodoObject[] viewDefs = getChildrenOfType(KomodoLexicon.ViewDefinition.NODE_TYPE);
		if (viewDefs == null || viewDefs.length == 0) {
			return null;
		}

		return new ViewDefinitionImpl(getTransaction(), getRepository(), viewDefs[0].getAbsolutePath());
	}

}
