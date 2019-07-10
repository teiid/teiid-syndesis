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
package org.komodo.relational.workspace;

import java.util.ArrayList;
import java.util.List;

import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.spi.KEvent;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.spi.utils.KeyInValueMap;
import org.komodo.spi.utils.KeyInValueMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 *
 */
public class WorkspaceManager extends ObjectImpl implements RelationalObject {
	
	protected static final KLog LOGGER = KLog.getLogger();

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Vdb.IDENTIFIER,
                                                                       Schema.IDENTIFIER,
                                                                       Dataservice.IDENTIFIER };

    /**
     * The type identifier.
     */
    public static final int TYPE_ID = WorkspaceManager.class.hashCode();

    // @formatter:off
    private static final String FIND_ALL_QUERY_PATTERN = "SELECT [jcr:path] FROM [%s]" //$NON-NLS-1$
                                                         + " WHERE ISDESCENDANTNODE('%s')" //$NON-NLS-1$
                                                         + " ORDER BY [jcr:path] ASC"; //$NON-NLS-1$

    private static final String FIND_MATCHING_QUERY_PATTERN = "SELECT [jcr:path] FROM [%s]"  //$NON-NLS-1$
                                                              + " WHERE ISDESCENDANTNODE('%s')" //$NON-NLS-1$
                                                              + " AND [jcr:name] LIKE '%s'" //$NON-NLS-1$
                                                              + " ORDER BY [jcr:path] ASC"; //$NON-NLS-1$
    // @formatter:on

    private static class CacheKey {
        private final Repository.Id repoId;

        private final String user;

        public CacheKey(Repository.Id repoId, String user) {
            this.repoId = repoId;
            this.user = user;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((repoId == null) ? 0 : repoId.hashCode());
            result = prime * result + ((user == null) ? 0 : user.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey)obj;
            if (repoId == null) {
                if (other.repoId != null)
                    return false;
            } else if (!repoId.equals(other.repoId))
                return false;
            if (user == null) {
                if (other.user != null)
                    return false;
            } else if (!user.equals(other.user))
                return false;
            return true;
        }
    }

    private static class WskpMgrAdapter implements KeyFromValueAdapter< CacheKey, WorkspaceManager > {

        @Override
        public CacheKey getKey( WorkspaceManager value ) {
            Repository repository = value.getRepository();
            String user = value.getOwner();

            return new CacheKey(repository.getId(), user);
        }
    }

    private static KeyFromValueAdapter< CacheKey, WorkspaceManager > adapter = new WskpMgrAdapter();

    private static KeyInValueMap< CacheKey, WorkspaceManager > instances = 
                                                                        new KeyInValueMap< CacheKey, WorkspaceManager >(adapter);

    private final String owner;
    
    private AdapterFactory adapterFactory = new AdapterFactory( );
    

    /**
     * @param repository
     *        the repository whose workspace manager is being requested (cannot be <code>null</code>)
     * @param transaction
     *        the transaction containing the user name of the owner of this workspace manager
     *        (if <code>null</code> then this manager is owner by the system user and has the workspace root as its path)
     *
     * @return the singleton instance for the given repository (never <code>null</code>)
     * @throws KException
     *         if there is an error obtaining the workspace manager
     */
    public static WorkspaceManager getInstance( Repository repository, UnitOfWork transaction) throws KException {
        boolean txNotProvided = transaction == null;

        if (txNotProvided) {
			transaction = repository.createTransaction(Repository.SYSTEM_USER, "createWorkspaceManager", false, null, //$NON-NLS-1$
					Repository.SYSTEM_USER);
        }

        WorkspaceManager instance = instances.get(new CacheKey(repository.getId(), transaction.getRepositoryUser()));

        if ( instance == null ) {
            // We must create a transaction here so that it can be passed on to the constructor. Since the
            // node associated with the WorkspaceManager always exists we don't have to create it.
            instance = new WorkspaceManager(repository, transaction);

            if (txNotProvided)
                transaction.commit();

            instances.add( instance );
        }

        return instance;
    }

    /**
     * @return the owner of this workspace manager
     */
    public String getOwner() {
        return this.owner;
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
     * @see org.komodo.relational.RelationalObject#getFilters()
     */
    @Override
    public Filter[] getFilters() {
        return RelationalObject.NO_FILTERS;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return KomodoType.WORKSPACE;
    }

    /**
     * Primarily used in tests to remove the workspace manager instance from the instances cache.
     *
     * @param repository remove instance with given repository
     */
    public static void uncacheInstance(final Repository repository, final String owner) {
        if (repository == null)
            return;

        instances.remove(new CacheKey(repository.getId(), owner));
    }

    private WorkspaceManager(Repository repository, UnitOfWork uow ) throws KException {
        super( repository, RepositoryImpl.komodoWorkspacePath(uow), 0 );
        this.owner = uow.getRepositoryUser();

        repository.addObserver(new RepositoryObserver() {

            @Override
            public void eventOccurred(KEvent<?> event) {
                // Disposal observer
                if (getRepository() == null || State.NOT_REACHABLE == getRepository().getState() || !(getRepository().ping())) {
                    instances.remove(adapter.getKey(WorkspaceManager.this));
                }
            }

            @Override
            public void errorOccurred(Throwable e) {
                // Nothing to do
            }
        });
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the dataservice object being created (can be <code>null</code>)
     * @param serviceName
     *        the name of the dataservice to create (cannot be empty)
     * @return the Dataservice object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Dataservice createDataservice( final UnitOfWork uow,
                                        final KomodoObject parent,
                                        final String serviceName ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                 : parent.getAbsolutePath() );
         return RelationalModelFactory.createDataservice( uow, getRepository(), path, serviceName );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the model object being created (can be <code>null</code> if VDB should be created at the workspace
     *        root)
     * @param vdbName
     *        the name of the VDB to create (cannot be empty)
     * @param externalFilePath
     *        the VDB file path on the local file system (cannot be empty)
     * @return the VDB (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Vdb createVdb( final UnitOfWork uow,
                          final KomodoObject parent,
                          final String vdbName,
                          final String externalFilePath ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                : parent.getAbsolutePath() );
        return RelationalModelFactory.createVdb( uow, getRepository(), path, vdbName, externalFilePath );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param kobjects
     *        the object(s) being deleted (cannot be <code>null</code>, empty, or have a <code>null</code> element)
     * @throws KException
     *         if an error occurs or if an object does not exist
     */
    public void delete( final UnitOfWork transaction,
                        final KomodoObject... kobjects ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( kobjects, "kobjects" ); //$NON-NLS-1$

        for ( final KomodoObject kobject : kobjects ) {
            ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
            validateWorkspaceMember( transaction, kobject );
            kobject.remove( transaction );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param type
     *        the lexicon node type name of objects being found (cannot be empty)
     * @param parentPath
     *        the parent path whose children recursively will be checked (can be empty if searching from the workspace root)
     * @param namePattern
     *        the regex used to match object names (can be empty if all objects of the given type are being requested)
     * @param includeSubTypes
     *        determines whether sub types are included in the return
     * @return the paths of all the objects under the specified parent path with the specified type (never <code>null</code> but
     *         can be empty)
     * @throws KException
     *         if an error occurs
     */
    public String[] findByType( final UnitOfWork transaction,
                                final String type,
                                String parentPath,
                                final String namePattern,
                                boolean includeSubTypes) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state must be NOT_STARTED and was " + transaction.getState() ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

        if ( StringUtils.isBlank( parentPath ) ) {
            parentPath = RepositoryImpl.komodoWorkspacePath(transaction);
        }

        try {
            String queryText = null;

            if ( StringUtils.isBlank( namePattern ) ) {
                queryText = String.format( FIND_ALL_QUERY_PATTERN, type, parentPath );
            } else {
                queryText = String.format( FIND_MATCHING_QUERY_PATTERN, type, parentPath, namePattern );
            }

            final List< KomodoObject > kObjs = getRepository().query( transaction, queryText );
            List< KomodoObject > results = new ArrayList< > ();
            for( final KomodoObject kObj : kObjs ) {
                if(includeSubTypes) {
                    results.add(kObj);
                } else if ( type.equals(kObj.getPrimaryType(transaction).getName()) ) {
                    results.add(kObj);
                }
            }

            final int numPaths = results.size();

            if ( numPaths == 0 ) {
                return StringConstants.EMPTY_ARRAY;
            }

            final String[] result = new String[ numPaths ];
            int i = 0;

            for ( final KomodoObject kObject : results ) {
                result[ i++ ] = kObject.getAbsolutePath();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param type
     *        the lexicon node type name of objects being found
     * @return the paths of all the objects in the workspace with the specified type (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public String[] findByType( final UnitOfWork transaction,
                                final String type) throws KException {
        return findByType( transaction, type, RepositoryImpl.komodoWorkspacePath(transaction), null, false );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param type
     *        the lexicon node type name of objects being found
     * @param includeSubTypes
     *        determines whether sub types are included in the return
     * @return the paths of all the objects in the workspace with the specified type (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public String[] findByType( final UnitOfWork transaction,
                                final String type,
                                boolean includeSubTypes) throws KException {
        return findByType( transaction, type, RepositoryImpl.komodoWorkspacePath(transaction), null, includeSubTypes );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param searchPattern pattern to match.  If blank, will match all. 
     * @return {@link Dataservice}s in the workspace
     * @throws KException
     *         if an error occurs
     */
    public Dataservice[] findDataservices( UnitOfWork transaction, String searchPattern ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, DataVirtLexicon.DataService.NODE_TYPE, null, searchPattern, false);
        Dataservice[] result = null;

        if (paths.length == 0) {
            result = Dataservice.NO_DATASERVICES;
        } else {
            result = new Dataservice[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new DataserviceImpl(transaction, getRepository(), path);
            }
        }

        LOGGER.debug("getDataservices:found '{0}' DataServices using pattern '{1}'", result.length, //$NON-NLS-1$
                searchPattern);
        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Vdb}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Vdb[] findVdbs( final UnitOfWork transaction) throws KException {
    	return findVdbs(transaction, null);
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param searchPattern regex search pattern, if blank all are returned
     * @return {@link Vdb}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Vdb[] findVdbs( final UnitOfWork transaction, String searchPattern ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, VdbLexicon.Vdb.VIRTUAL_DATABASE, null, searchPattern, false);
        Vdb[] result = null;

        if (paths.length == 0) {
            result = Vdb.NO_VDBS;
        } else {
            result = new Vdb[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new VdbImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.repository.ObjectImpl#remove(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void remove( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Relational.REMOVE_NOT_ALLOWED, getAbsolutePath() ) );
    }

    /**
     * <strong><em>Rename is not allowed!!</em></strong>
     *
     * @see org.komodo.spi.repository.KomodoObject#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException if called
     */
    @Override
    public final void rename( final UnitOfWork transaction,
                              final String newName ) throws UnsupportedOperationException {
        throw new UnsupportedOperationException( Messages.getString( Relational.RENAME_NOT_ALLOWED, getAbsolutePath() ) );
    }

    /**
     * Attempts to adapt the given object to a relational model typed class.
     * If the object is not an instance of {@link KomodoObject} then null is
     * returned.
     *
     * The type id of the {@link KomodoObject} is extracted and the correct
     * relational model object created. If the latter is not assignable from the
     * given adapted class then it is concluded the adaption should fail and
     * null is returned, otherwise the new object is returned.
     *
     * @param <T>
     *        the desired outcome class
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param object
     *        the object being resolved
     * @param resolvedClass
     *        the class the object should be resolved to (cannot be <code>null</code>)
     * @return the strong typed object of the desired type (can be <code>null</code> if not resolvable)
     * @throws KException
     *         if a resolver could not be found or if an error occurred
     */
    public <T> T resolve( final UnitOfWork transaction,
                                                 final Object object,
                                                 final Class<T> resolvedClass ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        T kobject = adapterFactory.adapt(transaction, object, resolvedClass);
        return kobject;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#setFilters(org.komodo.relational.RelationalObject.Filter[])
     */
    @Override
    public void setFilters( Filter[] newFilters ) {
        // filters not allowed
    }

    private void validateWorkspaceMember( final UnitOfWork uow,
                                          final KomodoObject kobject ) throws KException {
        if (!getRepository().equals(kobject.getRepository())) {
            throw new KException(Messages.getString(Relational.OBJECT_BEING_DELETED_HAS_WRONG_REPOSITORY,
                                                    kobject.getAbsolutePath(),
                                                    kobject.getRepository().getId().getUrl(),
                                                    getRepository().getId().getUrl()));
        }

        if (!kobject.getAbsolutePath().startsWith(getRepository().komodoWorkspace(uow).getAbsolutePath())) {
            throw new KException(Messages.getString(Relational.OBJECT_BEING_DELETED_HAS_NULL_PARENT, kobject.getAbsolutePath()));
        }
    }
    
    public Dataservice findDataservice(UnitOfWork uow, String dataserviceName) throws KException {
        if (! hasChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE ) ) {
            return null;
        }

        final KomodoObject kobject = getChild( uow, dataserviceName, DataVirtLexicon.DataService.NODE_TYPE );
        final Dataservice dataservice = resolve( uow, kobject, Dataservice.class );

        LOGGER.debug( "Dataservice '{0}' was found", dataserviceName ); //$NON-NLS-1$
        return dataservice;
    }
    
    public Vdb findVdb(UnitOfWork uow, String vdbName) throws KException {
        if (! hasChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE ) ) {
            return null;
        }

        final KomodoObject kobject = getChild( uow, vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE );
        final Vdb vdb = resolve( uow, kobject, Vdb.class );

        LOGGER.debug( "VDB '{0}' was found", vdbName ); //$NON-NLS-1$
        return vdb;
    }

	public Model findModel(UnitOfWork uow, Vdb vdb, String modelName) throws KException {
		if (! vdb.hasChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL)) {
            return null;
        }

		KomodoObject kModel = vdb.getChild(uow, modelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        return resolve( uow, kModel, Model.class );
	}
}
