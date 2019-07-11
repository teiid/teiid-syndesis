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
package org.komodo.core;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.komodo.core.repository.RepositoryImpl;
import org.komodo.metadata.MetadataClientEvent;
import org.komodo.metadata.MetadataInstance;
import org.komodo.relational.WorkspaceManager;
import org.komodo.relational.workspace.WorkspaceManagerImpl;
import org.komodo.spi.KClient;
import org.komodo.spi.KEngine;
import org.komodo.spi.KErrorHandler;
import org.komodo.spi.KEvent;
import org.komodo.spi.KEvent.Type;
import org.komodo.spi.KException;
import org.komodo.spi.KObserver;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.komodo.utils.observer.KLatchObserver;
import org.teiid.query.parser.QueryParser;
import org.teiid.query.sql.LanguageObject;

/**
 * The Komodo engine. It is responsible for persisting and retriever user session data and Teiid artifacts.
 */
public final class KEngineImpl implements KEngine, KClient, StringConstants {

    private static final String PREFIX = KEngineImpl.class.getSimpleName() + DOT;
    
    /**
     * Check the engine data directory property has a legitimate value
     */
    public static void checkDataDirProperty() {
        // Initialize default data directory system property if necessary
        if ( ! StringUtils.isBlank( System.getProperty( SystemConstants.ENGINE_DATA_DIR ) ) )
            return;

        final String defaultValue = ( System.getProperty( "user.home", "/" ) + "/.komodo" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        System.setProperty( SystemConstants.ENGINE_DATA_DIR, defaultValue );
    }

    /**
     * @return engine started event
     */
    private static KEvent<KEngineImpl> engineStartedEvent(KEngineImpl engine) {
        return new KEvent<KEngineImpl>(engine, Type.ENGINE_STARTED);
    }

    /**
     * @return engine shutdown event
     */
    private static KEvent<KEngineImpl> engineShutdownEvent(KEngineImpl engine) {
        return new KEvent<KEngineImpl>(engine, Type.ENGINE_SHUTDOWN);
    }

    private final Set<Repository> repositories = new HashSet<Repository>();

    private State state = State.SHUTDOWN;

    private Repository defaultRepository;

    private KomodoErrorHandler errorHandler = new KomodoErrorHandler();

    private MetadataInstance metadataInstance;

    private final Set<KObserver> observers = new HashSet<>();

    public KEngineImpl() {
        checkDataDirProperty();

        // Initialize the logging system
        try {
            KLog.getLogger();
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        //
        // Logs error thrown by sub-components without
        // having to call the error handler directly
        //
        addObserver(errorHandler);
    }

    /**
     * @return the defaultRepository
     */
    public Repository getDefaultRepository() throws KException {
        if (this.defaultRepository == null) {        	
            throw new KException(Messages.getString(Messages.KEngine.No_Repository));
        }
        return this.defaultRepository;
    }

    /**
     * Sets the default repository.
     *
     * Note: This should hardly ever be called except in testing.
     *
     * To use correctly a test harness should:
     * 1. Call this with a valid repository before calling {{@link #start()}
     * 2. When tests are completed, shutdown the engine
     * 3. Call this again with a value of null to clear the default repository field
     *
     * @param repository the default repository
     * @throws Exception if an error occurs
     */
    @Override
    public void setDefaultRepository(Repository repository) throws Exception {
        ArgCheck.isTrue(State.SHUTDOWN.equals(getState()), "Engine should be shutdown before calling setDefaultRepository"); //$NON-NLS-1$

        boolean clearingRepo = repository == null;
        boolean settingNewRepo = repository != null && defaultRepository == null;
        String failMsg = "Can only call setDefaultRepository with a null argument or if the default repository is already null"; //$NON-NLS-1$
        ArgCheck.isTrue(clearingRepo || settingNewRepo, failMsg);

        if (defaultRepository != null)
            remove(defaultRepository); // Remove the old repository

        // Set the new repository
        defaultRepository = repository;
        if (repository != null) {
            add(defaultRepository);
            ((RepositoryImpl) repository).registerKEngine(this);
        }
    }

    public MetadataInstance getMetadataInstance() throws KException {
        if (this.metadataInstance == null) {
        	throw new KException(Messages.getString(Messages.KEngine.No_Metadata_Instance));
        }
        return this.metadataInstance;
    }
    
    @Override
    public void setMetadataInstance(MetadataInstance instance) {
    	assert instance != null;
    	this.metadataInstance = instance;
    	metadataInstance.addObserver(this);
    }

    /**
     * Adds the repository to the engine. Does nothing if repository has already been added
     *
     * @param repository the repository being added (cannot be <code>null</code>)
     * @throws KException if error occurs
     */
    public void add(final Repository repository) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$

        if (this.repositories.add(repository)) {
            repository.addClient(this);
            KLog.getLogger().debug(Messages.getString(Messages.KEngine.Added_Repository, PREFIX, repository.getId().getUrl()));
            notifyObservers(KEvent.repositoryAddedEvent(repository));

            // Notify this repository if it has started
            if (State.STARTED == state)
                repository.notify(RepositoryClientEvent.createStartedEvent(this));

        }
    }

    @Override
    public State getState() {
        return this.state;
    }

    /**
     * @return the registered repositories (never <code>null</code> but can be empty)
     */
    public Set<Repository> getRepositories() {
        Set<Repository> allRepositories = new HashSet<Repository>();

        if (defaultRepository != null)
            allRepositories.add(defaultRepository);

        allRepositories.addAll(this.repositories);

        return Collections.unmodifiableSet(allRepositories);
    }

    private void notifyRepositories(final RepositoryClientEvent event) {
        ArgCheck.isNotNull(event);

        for (Repository repository : getRepositories()) {
            repository.notify(event);
        }
    }

    private void notifyMetadataServer(final MetadataClientEvent event) throws KException {
        ArgCheck.isNotNull(event);

        getMetadataInstance().notify(event);
    }

    /**
     * @param repository the repository being removed (cannot be <code>null</code>)
     *
     * @throws KException if the repository was not removed
     */
    public void remove(final Repository repository) throws KException {
        ArgCheck.isNotNull(repository, "repository"); //$NON-NLS-1$

        if (this.repositories.remove(repository)) {
            repository.removeClient(this);
            KLog.getLogger().debug(Messages.getString(Messages.KEngine.Removed_Repository, PREFIX, repository.getId().getUrl()));
            notifyObservers(KEvent.repositoryRemovedEvent(repository));
        } else {
            throw new KException(Messages.getString(Messages.KEngine.Removed_Repository_Failure, repository.getId().getUrl()));
        }
    }

    /**
     * @throws KException if there is an error during engine shutdown
     */
    public void shutdown() throws KException {
        try {
            this.state = State.SHUTDOWN;
            KLog.getLogger().debug("Komodo engine successfully shutdown"); //$NON-NLS-1$

            // Notify any registered repositories that this engine has shutdown
            notifyRepositories(RepositoryClientEvent.createShuttingDownEvent(this));

            // Notify the metadata instance that this engine has shutdown
            notifyMetadataServer(MetadataClientEvent.createShuttingDownEvent(this));

            // Notify any 3rd-party listeners that this engine has shutdown
            notifyObservers(engineShutdownEvent(this));

        } catch (final Exception e) {
            this.state = State.ERROR;
            throw new KException(Messages.getString(Messages.KEngine.Shutdown_Failure), e);
        }
    }

    /**
     * Shutdown the engine and wait for it and all repositories to
     * be disconnected, including the local repository which should
     * be shutdown as well.
     *
     * @throws Exception if shutdown fails
     */
    public void shutdownAndWait() throws Exception {
        Callable<Boolean> shutdownTask = new Callable<Boolean>() {

            @Override
            public Boolean call() throws Exception {
                shutdown();

                boolean shutdown = false;
                while(!shutdown) {
                    if (! KClient.State.SHUTDOWN.equals(KEngineImpl.this.getState())) {
                        Thread.sleep(5);
                        continue;
                    }

                    if (MetadataInstance.Condition.REACHABLE.equals(KEngineImpl.this.getMetadataInstance().getCondition())) {
                        Thread.sleep(5);
                        continue;
                    }

                    int reposShutdown = 0;
                    for (Repository repository : getRepositories()) {
                        if (Repository.State.REACHABLE.equals(repository.getState()))
                            break;

                        reposShutdown++;
                    }

                    if (reposShutdown == getRepositories().size())
                        shutdown = true;
                    else
                        Thread.sleep(5);
                }

                return shutdown;
            }
        };

        ExecutorService executor = Executors.newSingleThreadExecutor();
        Future<Boolean> future = executor.submit(shutdownTask);

        KLog.getLogger().info("Starting shutdown procedure ..."); //$NON-NLS-1$

        //
        // Hold the thread until shutdown is complete
        //
        future.get(5, TimeUnit.MINUTES);

        KLog.getLogger().info("Shutdown completed."); //$NON-NLS-1$

        executor.shutdownNow();
    }

    /**
     * @throws KException if there is an error starting the engine
     */
    public void start() throws KException {
        try {
            // Initialise the local repository
            getDefaultRepository();

            // Initialise the metadata instance
            getMetadataInstance();

            // TODO implement start (read any saved session state, connect to repos if auto-connect, etc.)
            this.state = State.STARTED;
            KLog.getLogger().debug("Komodo engine successfully started"); //$NON-NLS-1$

            // Notify any registered repositories that this engine has started
            notifyRepositories(RepositoryClientEvent.createStartedEvent(this));

            // Notify any 3rd-party listeners that this engine has started
            notifyObservers(engineStartedEvent(this));

        } catch (final Exception e) {
            this.state = State.ERROR;
            String stackTrace = StringUtils.exceptionToString(e);
            throw new KException(Messages.getString(Messages.KEngine.Startup_Failure) + NEW_LINE + stackTrace, e);
        }
    }

    /**
     * Start the engine and wait for the default repository,
     * metadata server to be started
     * @return true if engine started successfully, false otherwise
     *
     * @throws Exception if start fails
     */
    @Override
    public boolean startAndWait() throws Exception {

        KLatchObserver observer = new KLatchObserver(Type.REPOSITORY_STARTED,
                                                                                                 Type.ENGINE_STARTED);

        addObserver(observer);

        // wait for engine to start
        boolean started = false;
        try {
            start();
            started = observer.getLatch().await(3, TimeUnit.MINUTES);
            if (observer.getError() != null) {
                //
                // latch was released due to the engine throwing an error rather than starting
                //
                throw observer.getError();
            }
        } catch (Throwable t) {
            throw new KException(t);
        } finally {
            removeObserver(observer);
        }

        return started;
    }

    /**
     * @return the error handler
     */
    public KErrorHandler getErrorHandler() {
        return this.errorHandler;
    }

    /**
     * Sets the error handler implementation of the engine
     *
     * @param errorHandler additional error handler to be notified of errors
     */
    public void addErrorHandler(KErrorHandler errorHandler) {
        this.errorHandler.add(errorHandler);
    }

    /**
     * Adds the observer to the engine
     * @param observer
     */
    public void addObserver(KObserver observer) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.add(observer);
    }

    /**
     * Removes the observer from the engine
     * @param observer
     */
    public void removeObserver( final KObserver observer ) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.remove(observer);
    }

    private <T> void notifyObservers(final KEvent<T> event) {
        ArgCheck.isNotNull(event);
        final Set<KObserver> copy = new HashSet<>(this.observers);

        for (final KObserver observer : copy) {
            try {
                observer.eventOccurred(event);
            } catch (final Exception e) {
                observer.errorOccurred(e);
            }
        }
    }

    @Override
    public void eventOccurred(KEvent<?> event) {
        //
        // Pass any events received by internal sub-components
        // to any external observers of the engine
        //
        notifyObservers(event);
    }

    @Override
    public void errorOccurred(Throwable e) {
        //
        // Pass any exceptions received by internal sub-components
        // to any external observers of the engine
        //
        ArgCheck.isNotNull(e);

        final Set<KObserver> copy = new HashSet<>(this.observers);

        for (final KObserver observer : copy) {
            observer.errorOccurred(e);
        }
    }

    /**
     * Not included on {@link MetadataInstance} interface due to dependency on {@link LanguageObject}
     *
     * @param sql
     * @return a language object tree representing the given sql string
     * @throws Exception
     */
    public LanguageObject parse(String sql) throws Exception {
        return QueryParser.getQueryParser().parseDesignerCommand(sql);
    }

	@Override
	public WorkspaceManager getWorkspaceManager(UnitOfWork transaction) throws KException {
		return WorkspaceManagerImpl.getInstance(getDefaultRepository(), transaction);
	}
}
