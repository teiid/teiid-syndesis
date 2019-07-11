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

import java.net.URL;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.WeakHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.komodo.core.KEvent;
import org.komodo.core.internal.repository.JcrEngine;
import org.komodo.core.internal.repository.KObjectFactory;
import org.komodo.core.internal.repository.Repository;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KPropertyFactory;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWorkDelegate;
import org.komodo.spi.repository.UnitOfWorkListener;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.springframework.stereotype.Component;

/**
 * A repository installed on the local machine, using the modeshape engine and repository.
 */
@Component
public class LocalRepository extends RepositoryImpl {

    private static String LOCAL_REPOSITORY_CONFIG = "local-repository-config.json"; //$NON-NLS-1$

    /**
     * The default local repository identifier used for the production komodo engine.
     */
    public static final LocalRepositoryId DEFAULT_LOCAL_REPOSITORY_ID = new LocalRepositoryId(
                                                                                              LocalRepository.class.getResource(LOCAL_REPOSITORY_CONFIG),
                                                                                              DEFAULT_LOCAL_WORKSPACE_NAME);

    /**
     * Identifier for the local repository
     */
    public static class LocalRepositoryId implements Id {

        private final URL configPath;
        private final String workspaceName;

        /**
         * @param configPathUrl url of configuration file
         * @param workspaceName name of workspace
         */
        public LocalRepositoryId(final URL configPathUrl, final String workspaceName ) {
            this.configPath = configPathUrl;
            this.workspaceName = workspaceName;
        }

        @Override
        public URL getConfiguration() {
            return configPath;
        }

        @Override
        public String getUrl() {
            return this.configPath.toString();
        }

        @Override
        public String getWorkspaceName() {
            return this.workspaceName;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.configPath == null) ? 0 : this.configPath.hashCode());
            result = prime * result + ((this.workspaceName == null) ? 0 : this.workspaceName.hashCode());
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
            LocalRepositoryId other = (LocalRepositoryId)obj;
            if (this.configPath == null) {
                if (other.configPath != null)
                    return false;
            } else if (!this.configPath.equals(other.configPath))
                return false;
            if (this.workspaceName == null) {
                if (other.workspaceName != null)
                    return false;
            } else if (!this.workspaceName.equals(other.workspaceName))
                return false;
            return true;
        }

        @Override
        public String toString() {
            return "LocalRepositoryId [configPath=" + this.configPath + ", workspaceName=" + this.workspaceName + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
    }

    private WeakHashMap< UnitOfWorkDelegate, UnitOfWork > sessions = new WeakHashMap<>();

    private State state = State.NOT_REACHABLE;

    private RepoEngine engine;

    /**
     * Create an instance if a local repository using the specified configuration file.
     *
     * @param configPathUrl
     *        the URL of the configuration file (cannot be empty)
     * @param workspaceName
     *        the name of the repository workspace in the configuration file (cannot be empty)
     */
    public LocalRepository( final URL configPathUrl,
                            final String workspaceName) {
        super(new LocalRepositoryId(configPathUrl, workspaceName));
    }

    /**
     * Create an instance of local repository.
     *
     * @param repositoryId repository configuration of the instance
     */
    public LocalRepository(LocalRepositoryId repositoryId) {
        super(repositoryId);
    }

    /**
     * Create an instance of local repository using default configuration file and workspace name.
     */
    public LocalRepository() {
        this(DEFAULT_LOCAL_REPOSITORY_ID);
    }

    @Override
    public KObjectFactory getObjectFactory() {
        return engine.getNodeFactory();
    }

    @Override
    public KPropertyFactory getPropertyFactory() {
        return engine.getPropertyFactory();
    }

    @Override
    protected KQueryManager getQueryManager() {
        return engine.getQueryManager();
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object obj ) {
        if (this == obj) {
            return true;
        }

        if ((obj == null) || !getClass().equals(obj.getClass())) {
            return false;
        }

        final LocalRepository that = (LocalRepository)obj;
        return getId().equals(that.getId());
    }

    @Override
    public State getState() {
        return state;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return getId().hashCode();
    }

    @Override
    public boolean ping() {
        return ((this.engine != null) && ((this.engine.isAlive())) && this.engine.isRunning());
    }

    private UnitOfWorkDelegate createSession() throws KException {
        final CountDownLatch latch = new CountDownLatch(1);

        class CreateSessionCallback implements RepoEngine.RequestCallback {

            private Throwable error = null;
            private UnitOfWorkDelegate result = null;

            @Override
            public void errorOccurred( final Throwable e ) {
                this.error = e;
                latch.countDown();
            }

            Throwable getError() {
                return this.error;
            }

            UnitOfWorkDelegate getSession() {
                return this.result;
            }

            @Override
            public void respond( final Object results ) {
                this.result = (UnitOfWorkDelegate)results;
                latch.countDown();
            }

        }

        final CreateSessionCallback callback = new CreateSessionCallback();
        this.engine.accept(new RepoEngine.Request(RepoEngine.RequestType.CREATE_SESSION, callback));

        boolean timeout = false;

        try {
            timeout = !latch.await(1, TimeUnit.MINUTES);
        } catch (final Exception e) {
            throw new KException(e);
        }

        if (timeout) {
            throw new KException(Messages.getString(Messages.LocalRepository.Unable_To_Create_Session));
        }

        if (callback.getError() != null) {
            throw new KException(callback.getError());
        }

        return callback.getSession();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.core.internal.repository.Repository#createTransaction(java.lang.String, boolean,
     *      org.komodo.spi.repository.Repository.UnitOfWorkListener)
     */
    @Override
    public UnitOfWork createTransaction(final String userName, final String name,
                                         final boolean rollbackOnly,
                                         final UnitOfWorkListener callback, String repoUser) throws KException {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        LOGGER.debug("creating transaction {0} with rollbackOnly = {1}", name, rollbackOnly); //$NON-NLS-1$
        final UnitOfWorkDelegate session = createSession();
        final UnitOfWork uow = new LocalRepositoryTransaction(userName, name, session, rollbackOnly, callback, repoUser);
        this.sessions.put(session, uow);
        return uow;
    }

    class LocalRepositoryTransaction extends RepositoryImpl.UnitOfWorkImpl {

        LocalRepositoryTransaction(final String userName,
                                    final String uowName,
                                    final UnitOfWorkDelegate uowSession,
                                    final boolean uowRollbackOnly,
                                    final UnitOfWorkListener listener,
                                    final String repoUser) {
            super(userName, uowName, uowSession, uowRollbackOnly, listener, repoUser);
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.core.repository.RepositoryImpl.UnitOfWorkImpl#commit()
         */
        @Override
        public void commit() {
            if (this.state != State.NOT_STARTED) {
                this.error = new KException( Messages.getString( Messages.Komodo.ERROR_TRANSACTION_FINISHED,
                                                                 this.name,
                                                                 this.state ) );
                this.state = State.ERROR;
            } else {
                if (isRollbackOnly()) {
                    rollback();
                } else {
                    this.state = State.RUNNING;

                    // engine thread callback that communicates with transaction callback
                    class CommitCallback implements RepoEngine.RequestCallback {

                        /**
                         * {@inheritDoc}
                         *
                         * @see org.komodo.internal.repository.JcrEngine.RequestCallback#errorOccurred(java.lang.Throwable)
                         */
                        @Override
                        public void errorOccurred( final Throwable error ) {
                            setState( State.ERROR );
                            setError( error );

                            if (getCallback() == null) {
                                kEngine.getErrorHandler().error( error );
                            } else {
                                getCallback().errorOccurred( error );
                            }
                        }

                        /**
                         * {@inheritDoc}
                         *
                         * @see org.komodo.internal.repository.JcrEngine.RequestCallback#respond(java.lang.Object)
                         */
                        @Override
                        public void respond( final Object results ) {
                            setState( State.COMMITTED );

                            if (getCallback() != null) {
                                KLog.getLogger().debug(LocalRepositoryTransaction.class.getName() + ": Responding to callback: " + getCallback().getClass().getName()); //$NON-NLS-1$
                                getCallback().respond( null );
                            } else
                                KLog.getLogger().debug(LocalRepositoryTransaction.class.getName() + ": No callback specified"); //$NON-NLS-1$
                        }

                    }

                    // send commit request
                    final CommitCallback callback = new CommitCallback();
                    JcrEngine.SessionRequest request = new JcrEngine.SessionRequest( RepoEngine.RequestType.COMMIT_SESSION,
                                                                                                        callback,
                                                                                                        getSession(),
                                                                                                        getName() );
                    KLog.getLogger().debug("LocalRepository.LocalRepositoryTransaction.commit() post commit request for session: {0}",  //$NON-NLS-1$
                                           getSession().hashCode());
                    LocalRepository.this.engine.accept( request );
                }
            }
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.core.repository.RepositoryImpl.UnitOfWorkImpl#rollback()
         */
        @Override
        public void rollback() {
            if (this.state != State.NOT_STARTED) {
                this.error = new KException( Messages.getString( Messages.Komodo.ERROR_TRANSACTION_FINISHED,
                                                                 this.name,
                                                                 this.state ) );
                this.state = State.ERROR;
            } else {
                this.state = State.RUNNING;
            }

            // engine thread callback that communicates with transaction callback
            class RollbackCallback implements RepoEngine.RequestCallback {

                /**
                 * {@inheritDoc}
                 *
                 * @see org.komodo.internal.repository.JcrEngine.RequestCallback#errorOccurred(java.lang.Throwable)
                 */
                @Override
                public void errorOccurred( final Throwable error ) {
                    setState( State.ERROR );
                    setError( error );

                    if (getCallback() == null) {
                        kEngine.getErrorHandler().error( error );
                    } else {
                        getCallback().errorOccurred( error );
                    }
                }

                /**
                 * {@inheritDoc}
                 *
                 * @see org.komodo.internal.repository.JcrEngine.RequestCallback#respond(java.lang.Object)
                 */
                @Override
                public void respond( final Object results ) {
                    setState( State.ROLLED_BACK );

                    if (getCallback() != null) {
                        getCallback().respond( null );
                    }
                }

            }

            // send rollback request
            final RollbackCallback callback = new RollbackCallback();

            if ( this.state == State.ERROR ) {
                callback.errorOccurred( getError() );
            } else {
                KLog.getLogger().debug( "LocalRepository.LocalRepositoryTransaction.rollback post rollback request for session: {0}", //$NON-NLS-1$
                                        getSession().hashCode() );
                LocalRepository.this.engine.accept( new JcrEngine.SessionRequest( RepoEngine.RequestType.ROLLBACK_SESSION,
                                                                                                    callback,
                                                                                                    getSession(),
                                                                                                    getName() ) );
            }
        }

        protected void setError( final Throwable e ) {
            this.state = State.ERROR;

            if (e instanceof KException) {
                this.error = ( KException )e;
            } else {
                this.error = new KException( e );
            }
        }

        protected void setState ( final State newState ) {
            this.state = newState;
        }

    }

    private void createEngine() throws Exception {
        if (engine != null && engine.isAlive()) return;

        if (engine != null && !engine.isAlive()) {
            String msg = Messages.getString(Messages.LocalRepository.EngineThread_Died);

            Exception error = engine.getError();
                if (error != null) {
                    String stackTrace = StringUtils.exceptionToString(error);
                    msg = msg + NEW_LINE + stackTrace;
                }
            throw new Exception(msg);
        }

        engine = new JcrEngine(getId(), this.kEngine);
        engine.start();
    }

    private void startRepository() {
        if (this.state == State.REACHABLE) return;

        try {
            createEngine();
        } catch (Exception e) {
            errorObservers(e);
            return;
        }

        RepoEngine.RequestCallback callback = new RepoEngine.RequestCallback() {

            @Override
            public void errorOccurred( final Throwable error ) {
                errorObservers(error);
            }

            @Override
            public void respond( final Object results ) {
                if (engine.isRunning()) {
                    LocalRepository.this.state = State.REACHABLE;
                    KEvent<Repository> event = new KEvent<Repository>(LocalRepository.this, KEvent.Type.REPOSITORY_STARTED);
                    notifyObservers(event);
                }
            }
        };

        KLog.getLogger().debug("LocalRepository.startRepository() post start repository request"); //$NON-NLS-1$
        engine.accept(new RepoEngine.Request(RepoEngine.RequestType.START, callback));
    }

    private void stopRepository() {
        RepoEngine.RequestCallback callback = new RepoEngine.RequestCallback() {

            /**
             * {@inheritDoc}
             *
             * @see org.komodo.internal.repository.JcrEngine.RequestCallback#errorOccurred(java.lang.Throwable)
             */
            @Override
            public void errorOccurred( final Throwable error ) {
                errorObservers(error);
            }

            /**
             * {@inheritDoc}
             *
             * @see org.komodo.internal.repository.JcrEngine.RequestCallback#respond(java.lang.Object)
             */
            @Override
            public void respond( final Object results ) {
                if (engine != null && !engine.isRunning()) {
                    LocalRepository.this.state = State.NOT_REACHABLE;
                }

                //
                // If this repository is restarted then createEngineThread() is going to be called
                // hence this defunct engineThread must be discarded to ensure a clean restart
                //
                engine = null;

                KEvent<Repository> event = new KEvent<Repository>(LocalRepository.this, KEvent.Type.REPOSITORY_STOPPED);
                notifyObservers(event);
            }
        };

        if (engine == null) {
            callback.respond(null);
            return;
        }

        KLog.getLogger().debug("LocalRepository.stopRepository() post stop request"); //$NON-NLS-1$
        this.engine.accept(new RepoEngine.Request(RepoEngine.RequestType.STOP, callback));
    }

    private void clearRepository() {
        // cleanup session cache
        if (!this.sessions.isEmpty()) {
            final Iterator< Entry< UnitOfWorkDelegate, UnitOfWork > > itr = this.sessions.entrySet().iterator();

            while (itr.hasNext()) {
                final Entry< UnitOfWorkDelegate, UnitOfWork > entry = itr.next();
                final UnitOfWorkDelegate session = entry.getKey();

                // rollback and close all leftover sessions
                if ( session.isLive() ) {
                    final UnitOfWork uow = entry.getValue();
                    LOGGER.debug( "LocalRepository.stopRepository: closing session for transaction {0}", uow.getName() ); //$NON-NLS-1$

                    // rollback any session that is in an incomplete state
                    if ( !uow.getState().isFinal() ) {
                        try {
                            session.refresh( false );
                        } catch ( final Exception e ) {
                            LOGGER.error( "LocalRepository.stopRepository(): Exception rolling back transaction \"{0}\"", //$NON-NLS-1$
                                          e,
                                          uow.getName() );
                        }
                    }

                    // does not hurt to call logout if already called
                    session.complete();
                }

                itr.remove();
            }
        }

        RepoEngine.RequestCallback callback = new RepoEngine.RequestCallback() {

            /**
             * {@inheritDoc}
             *
             * @see org.komodo.internal.repository.JcrEngine.RequestCallback#errorOccurred(java.lang.Throwable)
             */
            @Override
            public void errorOccurred( final Throwable error ) {
                errorObservers(error);
            }

            /**
             * {@inheritDoc}
             *
             * @see org.komodo.internal.repository.JcrEngine.RequestCallback#respond(java.lang.Object)
             */
            @Override
            public void respond( final Object results ) {
                KEvent<Repository> event = new KEvent<Repository>(LocalRepository.this, KEvent.Type.REPOSITORY_CLEARED);
                notifyObservers(event);
            }
        };

        KLog.getLogger().debug("LocalRepository.clearRepository() post clear request"); //$NON-NLS-1$
        this.engine.accept(new RepoEngine.Request(RepoEngine.RequestType.CLEAR, callback));
    }

    @Override
    public void notify( RepositoryClientEvent event ) {
        if (event.getType() == RepositoryClientEvent.EventType.STARTED) {
            // Start the modeshape engine if not already started
            startRepository();
        } else if (event.getType() == RepositoryClientEvent.EventType.SHUTTING_DOWN) {
            stopRepository();
        } else if (event.getType() == RepositoryClientEvent.EventType.CLEAR) {
            clearRepository();
        }
    }

}
