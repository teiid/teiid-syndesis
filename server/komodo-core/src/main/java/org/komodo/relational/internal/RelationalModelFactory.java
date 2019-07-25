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

import java.util.Map;

import org.komodo.core.KomodoLexicon;
import org.komodo.core.internal.repository.Repository;
import org.komodo.core.repository.KomodoObject;
import org.komodo.core.repository.RepositoryTools;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.profile.SqlComposition;
import org.komodo.relational.profile.SqlProjectedColumn;
import org.komodo.relational.profile.StateCommand;
import org.komodo.relational.profile.internal.ProfileImpl;
import org.komodo.relational.profile.internal.SqlCompositionImpl;
import org.komodo.relational.profile.internal.SqlProjectedColumnImpl;
import org.komodo.relational.profile.internal.StateCommandAggregateImpl;
import org.komodo.relational.profile.internal.StateCommandImpl;
import org.komodo.relational.profile.internal.ViewDefinitionImpl;
import org.komodo.relational.profile.internal.ViewEditorStateImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A factory for {@link RelationalObject relational model objects}.
 */
public final class RelationalModelFactory {

    /**
     * Wraps the given exception in a {@link KException}
     *
     * @param e
     *        the exception
     * @return return a {@link KException} from the given {@link Exception}
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
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param parentWorkspacePath
     *        the parent path (can be empty)
     * @param serviceName
     *        the name of the dataservice fragment to create (cannot be empty)
     * @return the Dataservice model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static DataserviceImpl createDataservice( final UnitOfWork transaction,
                                                 final Repository repository,
                                                 final String parentWorkspacePath,
                                                 final String serviceName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( serviceName, "serviceName" ); //$NON-NLS-1$

        // make sure path is in the library
        String parentPath = parentWorkspacePath;
        final String workspacePath = repository.komodoWorkspace( transaction ).getAbsolutePath();

        if ( StringUtils.isBlank( parentWorkspacePath ) ) {
            parentPath = workspacePath;
        } else if ( !parentPath.startsWith( workspacePath ) ) {
            parentPath = ( workspacePath + parentPath );
        }

        final KomodoObject kobject = repository.add( transaction, parentPath, serviceName, DataVirtLexicon.DataService.NODE_TYPE );
        final DataserviceImpl result = new DataserviceImpl( transaction, repository, kobject.getAbsolutePath() );
        return result;
    }

    /**
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param profile
     *        the parent profile object
     * @param stateId
     *        the id of the view editor state
     * @return the view editor state object
     * @throws KException
     *        if an error occurs
     */
    public static ViewEditorStateImpl createViewEditorState(UnitOfWork transaction, Repository repository, ProfileImpl profile,
                                                                                                   String stateId) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotNull( stateId, "stateId" ); //$NON-NLS-1$

        try {
            final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction,
                                                                             profile,
                                                                             KomodoLexicon.Profile.VIEW_EDITOR_STATES,
                                                                             KomodoLexicon.Profile.VIEW_EDITOR_STATES );
            final KomodoObject kobject = grouping.addChild( transaction, stateId, KomodoLexicon.ViewEditorState.NODE_TYPE );
            final ViewEditorStateImpl result = new ViewEditorStateImpl( transaction, repository, kobject.getAbsolutePath() );
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }
    
    /**
    *
    * @param transaction
    *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
    * @param repository
    *        the repository where the model object will be created (cannot be <code>null</code>)
    * @param profile
    *        the parent profile object
    * @param stateId
    *        the id of the view definition
    * @return the view definition object
    * @throws KException
    *        if an error occurs
    */
   public static ViewDefinitionImpl createViewDefinition(UnitOfWork transaction, Repository repository, ViewEditorStateImpl viewEditorState) throws KException {
       ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
       ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
       ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$

       try {
   		   // If a ViewDefinitionImpl already exists, remove it first
           final ViewDefinitionImpl viewDefn = viewEditorState.getViewDefinition();
           if (viewDefn != null) {
           	viewDefn.remove(transaction);
           }

    	   final KomodoObject kobject = viewEditorState.addChild(transaction, KomodoLexicon.ViewEditorState.VIEW_DEFINITION, KomodoLexicon.ViewEditorState.VIEW_DEFINITION);
           final ViewDefinitionImpl result = new ViewDefinitionImpl( transaction, repository, kobject.getAbsolutePath() );
           return result;
       } catch ( final Exception e ) {
           throw handleError( e );
       }
   }
   
   /**
   *
   * @param transaction
   *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
   * @param repository
   *        the repository where the model object will be created (cannot be <code>null</code>)
   * @param viewDefinition
   *        the parent view definition
   * @param compositionName
   *        the sql composition name
   * @return the sql composition object
   * @throws KException
   *        if an error occurs
   */
	public static SqlComposition createSqlComposition(UnitOfWork transaction, 
			                                          Repository repository, 
			                                          ViewDefinitionImpl viewDefinition, 
			                                          String compositionName) throws KException {
		
	       ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
	       ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
	       ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
	       ArgCheck.isNotNull( compositionName, "compositionName" ); //$NON-NLS-1$
	       
	       try {
	           final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction, viewDefinition, KomodoLexicon.ViewDefinition.SQL_COMPOSITIONS,
	                                                                                                         KomodoLexicon.ViewDefinition.SQL_COMPOSITIONS );
	           final KomodoObject kobject = grouping.addChild( transaction, compositionName, KomodoLexicon.SqlComposition.NODE_TYPE );
	           final SqlComposition result = new SqlCompositionImpl( transaction, repository, kobject.getAbsolutePath() );
	           return result;
	       } catch ( final Exception e ) {
	           throw handleError( e );
	       }
	}

	/**
	 *
	 * @param transaction
	 *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
	 * @param repository
	 *        the repository where the model object will be created (cannot be <code>null</code>)
	 * @param viewDefinition
	 *        the parent view definition
	 * @param columnName
	 *        the sql projected column name
	 * @return the sql projected column object
	 * @throws KException
	 *        if an error occurs
	 */
	public static SqlProjectedColumn createSqlProjectedColumn(UnitOfWork transaction, 
	                                                          Repository repository, 
	                                                          ViewDefinitionImpl viewDefinition, 
	                                                          String columnName) throws KException {

	    ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
	    ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
	    ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
	    ArgCheck.isNotNull( columnName, "columnName" ); //$NON-NLS-1$

	    try {
	        final KomodoObject grouping = RepositoryTools.findOrCreateChild( transaction, viewDefinition, KomodoLexicon.ViewDefinition.SQL_PROJECTED_COLUMNS,
	                                                                         KomodoLexicon.ViewDefinition.SQL_PROJECTED_COLUMNS );
	        final KomodoObject kobject = grouping.addChild( transaction, columnName, KomodoLexicon.SqlProjectedColumn.NODE_TYPE );
	        final SqlProjectedColumn result = new SqlProjectedColumnImpl( transaction, repository, kobject.getAbsolutePath() );
	        return result;
	    } catch ( final Exception e ) {
	        throw handleError( e );
	    }
	}

    /**
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository where the model object will be created (cannot be <code>null</code>)
     * @param viewEditorState
     *        the parent view editor state object
     * @return the view editor state command object
     * @throws KException
     *        if an error occurs
     */
    public static StateCommandAggregateImpl createStateCommandAggregate (UnitOfWork transaction, Repository repository,
                                                                     ViewEditorStateImpl viewEditorState) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$

        try {
            int cmdSize = viewEditorState.getCommands().length;
            int index = cmdSize > 0 ? cmdSize - 1 : 0;
            String aggName = KomodoLexicon.StateCommandAggregate.NAME_PREFIX + index;
            final KomodoObject kobject = viewEditorState.addChild(transaction, aggName,
                                                                  KomodoLexicon.StateCommandAggregate.NODE_TYPE);
            final StateCommandAggregateImpl result = new StateCommandAggregateImpl(transaction, repository, kobject.getAbsolutePath());
            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
    *
    * @param transaction
    *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
    * @param repository
    *        the repository where the model object will be created (cannot be <code>null</code>)
    * @param stateCommandAgg
    *        the parent state command aggregate
    * @param stateCommandType
    *        the state command type (either 'undo' or 'redo')
    * @param commandId
    *        the id of the command
    * @param arguments
    *        the command arguments
    * @return the view editor state command object
    * @throws KException
    *        if an error occurs
    */
   public static StateCommand createStateCommand(UnitOfWork transaction, Repository repository,
                                                 StateCommandAggregateImpl stateCommandAgg,
                                                 String stateCommandType,
                                                 String commandId,
                                                 Map<String, String> arguments) throws KException {
       ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
       ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
       ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$
       ArgCheck.isNotNull(commandId, "commandId"); //$NON-NLS-1$
       ArgCheck.isNotNull(arguments, "arguments"); //$NON-NLS-1$

       
       try {
           if (stateCommandAgg.hasChild(stateCommandType))
               stateCommandAgg.removeChild(stateCommandType);

           KomodoObject stateCmdObject = stateCommandAgg.addChild(transaction, stateCommandType,
                                                                              KomodoLexicon.StateCommand.NODE_TYPE);
           StateCommand stateCmd = StateCommandImpl.RESOLVER.resolve(stateCmdObject);
           stateCmd.setId(commandId);
           stateCmd.setArguments(arguments);
           return stateCmd;
       } catch ( final Exception e ) {
           throw handleError( e );
       }
   }

    private RelationalModelFactory() {
        // nothing to do
    }
}
