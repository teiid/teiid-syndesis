/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.core.internal.repository;

import java.net.URL;
import java.util.Iterator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.Session;
import org.komodo.core.JcrLogConfigurator;
import org.komodo.core.KEngine;
import org.komodo.core.KomodoLexicon.Environment;
import org.komodo.core.KomodoLexicon.Komodo;
import org.komodo.core.repository.KSequencerController;
import org.komodo.core.repository.KSequencerListener;
import org.komodo.core.repository.Messages;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.spi.query.KQueryManager;
import org.komodo.spi.repository.KObjectFactory;
import org.komodo.spi.repository.KPropertyFactory;
import org.komodo.spi.repository.RepoEngine;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.UnitOfWorkDelegate;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.modeshape.common.collection.Problem;
import org.modeshape.common.collection.Problems;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.RepositoryConfiguration;

/**
 * The thread the ModeShape engine uses for local repositories.
 */
public class JcrEngine extends Thread implements RepoEngine {

    /**
     * A request to save or rollback a session.
     */
    public static class SessionRequest extends RepoEngine.Request {

        private final String name;
        private final UnitOfWorkDelegate session;

        /**
         * @param requestType
         *        the request type (cannot be <code>null</code> and must be a {@link SessionRequest session request}.
         * @param callback
         *        the callback (can be <code>null</code>)
         * @param requestSession
         *        the session the request pertains to (cannot be <code>null</code>)
         * @param requestName
         *        a name given to the request for logging purposes
         */
        public SessionRequest( final RepoEngine.RequestType requestType,
                               final RepoEngine.RequestCallback callback,
                               final UnitOfWorkDelegate requestSession,
                               final String requestName ) {
            super(requestType, callback);

            ArgCheck.isTrue(RepoEngine.RequestType.isSessionRequest(requestType), "request type is not a session request"); //$NON-NLS-1$
            ArgCheck.isNotNull(requestSession, "requestSession"); //$NON-NLS-1$
            ArgCheck.isNotEmpty(requestName, "requestName"); //$NON-NLS-1$

            this.session = requestSession;
            this.name = requestName;
        }

        String getName() {
            return this.name;
        }

        UnitOfWorkDelegate getSession() {
            return this.session;
        }

    }

    private final static ModeShapeEngine msEngine = new ModeShapeEngine();

    private static final KLog LOGGER = KLog.getLogger();

    private BlockingQueue< RepoEngine.Request > queue = new LinkedBlockingQueue< RepoEngine.Request >();

    private volatile boolean stop = false;

    private volatile Exception error = null;

    private final WorkspaceIdentifier identifier;

    private final Repository.Id repoId;

    private final KObjectFactory nodeFactory;

    private final KPropertyFactory propertyFactory;

    private final JcrQueryManager queryManager;

    private KSequencerController sequencers;
    
    private KEngine kEngine;

    /**
     * Create this thread and give it a name
     *
     * @param repoId
     *        information identifying the repository (cannot be <code>null</code>)
     */
    public JcrEngine( final Repository.Id repoId , KEngine kEngine) {
        super("Modeshape Engine Thread"); //$NON-NLS-1$
        this.repoId = repoId;
        this.identifier = new WorkspaceIdentifier(repoId.getWorkspaceName());
        this.kEngine = kEngine;
        setDaemon(true);
        nodeFactory = new JcrNodeFactory();
        propertyFactory = nodeFactory.getPropertyFactory();
        queryManager = new JcrQueryManager((JcrNodeFactory) nodeFactory);
    }

    public KObjectFactory getNodeFactory() {
        return nodeFactory;
    }

    public KPropertyFactory getPropertyFactory() {
        return propertyFactory;
    }

    @Override
    public KQueryManager getQueryManager() {
        return queryManager;
    }

    private void respondCallback(final RepoEngine.Request request, Object result) {
        if (request.getCallback() != null) {
            LOGGER.debug("ModeshapeEngineThread: Responding to callback: " + request.getCallback().getClass().getName()); //$NON-NLS-1$
            request.getCallback().respond(result);
        } else
            LOGGER.debug("ModeshapeEngineThread: No callback assigned"); //$NON-NLS-1$
    }

    private void errorCallback(final RepoEngine.Request request, Throwable e) {
        if (request.getCallback() != null) {
            request.getCallback().errorOccurred(e);
        }
    }

    private void logoutSession(final UnitOfWorkDelegate session) {
        if (session == null || ! session.isLive())
            return;

        LOGGER.debug("ModeShapeEngineThread.logoutSession: {0}", session.hashCode()); //$NON-NLS-1$
        session.complete();
    }

    private synchronized void commitSession( final RepoEngine.Request request ) {
        ArgCheck.isTrue(request.getRequestType() == RepoEngine.RequestType.COMMIT_SESSION,
                        "commitSession called when request is not a commit session"); //$NON-NLS-1$
        final SessionRequest commitRequest = (SessionRequest)request;
        final UnitOfWorkDelegate session = commitRequest.getSession();
        final String commitRequestId = commitRequest.getName() + HYPHEN + session.hashCode();

        LOGGER.debug("commit session for request {0}", commitRequest.getName()); //$NON-NLS-1$

        KSequencerListener sequencerListener = null;
        try {
            //
            // Only bother to save if we actually have changes to save
            //
            if (! session.hasPendingChanges()) {
                try {
                    respondCallback(request, null);
                } finally {
                    logoutSession(session);
                }
                return;
            }

            //
            // If the request has been flagged to await the completion of the sequencers
            // then attach a listener to the sequencers controller class, which will be responsible
            // for responding to the callback and finalising the session.
            //
            sequencerListener = new KSequencerListener() {

                private boolean aborted = false;

                @Override
                public String id() {
                    return commitRequestId;
                }

                @Override
                public UnitOfWorkDelegate session() {
                    return session;
                }

                @Override
                public void sequencingCompleted() {
                    LOGGER.debug("Sequencers completed. Calling request callback"); //$NON-NLS-1$
                    try {
                        if (aborted) {
                            //
                            // Sequencing listener told to abort regardless of the sequencing completion
                            // Should occur only if parent tx has thrown an exception and it will take care
                            // of informing the callback.
                            //
                            return;
                        }

                        respondCallback(request, null);
                    } finally {
                        logoutSession(session);
                    }
                }

                @Override
                public void sequencingError(Exception exception) {
                    try {
                        LOGGER.debug(Messages.getString(Messages.Komodo.SEQUENCING_ERROR_TRYING_TO_COMMIT, exception, commitRequest.getName()));
                        errorCallback(request, exception);
                    } finally {
                        logoutSession(session);
                    }
                }

                @Override
                public void abort() {
                    this.aborted = true;
                }
            };

            sequencers.addSequencerListener(sequencerListener);

            //
            // Save the session
            //
            session.save();

            LOGGER.debug("commit session request {0} has been saved", commitRequest.getName()); //$NON-NLS-1$

        } catch (final Throwable e) {
            if (sequencerListener != null) {
                //
                // Want to rollback session rather than respond normally
                // so signal to listener to abort
                //
                sequencerListener.abort();
            }

            request.setRequestType(RepoEngine.RequestType.ROLLBACK_SESSION);
            LOGGER.error(Messages.getString(Messages.Komodo.ERROR_TRYING_TO_COMMIT, e, commitRequest.getName()));
            rollbackSession(request, e);
        }
    }

    /**
     * @return is modeshape engine and repository are running
     */
    public boolean isRunning() {
        return RepositoryUtils.isEngineRunning(msEngine) &&
                    RepositoryUtils.isRepositoryRunning(identifier.getRepository());
    }

    /**
     * @return any error that may have occurred when the run method threw an error
     */
    public Exception getError() {
        return this.error;
    }

    private synchronized void rollbackSession( final RepoEngine.Request request, Throwable error) {
        ArgCheck.isTrue(request.getRequestType() == RepoEngine.RequestType.ROLLBACK_SESSION,
                        "rollbackSession called when request is not a rollback session"); //$NON-NLS-1$
        final SessionRequest rollbackRequest = (SessionRequest)request;
        final UnitOfWorkDelegate session = rollbackRequest.getSession();
        LOGGER.debug("rollback session for request {0}", rollbackRequest.getName()); //$NON-NLS-1$

        try {
            if (session.isLive()) session.refresh(false);
            LOGGER.debug("rollback session request {0} has been rolled back", rollbackRequest.getName()); //$NON-NLS-1$

            if (error != null)
                errorCallback(request, error);
            else
                respondCallback(request, null);

        } catch (final Exception e) {
            LOGGER.error(Messages.getString(Messages.Komodo.ERROR_TRYING_TO_ROLLBACK, e, rollbackRequest.getName()));
            errorCallback(request, e);
        } finally {
            logoutSession(session);
        }
    }

    /**
     * Initialise the repository configuration.
     *
     * Works around [ISPN-5527] / [MODE-2471] where java system properties are not
     * replaced for the expiration property of the leveldb cache store configuration.
     *
     * @param configuration
     * @return the repository configuration
     */
    private RepositoryConfiguration initialiseRepositoryConfiguration(URL configUrl) throws Exception {
        RepositoryConfiguration config = RepositoryConfiguration.read(configUrl);
//        if (config.getPersistenceConfiguration() == null)
//            return config; // No cache configuration specified so nothing to do
//
//        InputStream cacheConfigStream = FileLookupFactory.newInstance().lookupFileStrict(config.getPersistenceConfiguration(), Thread.currentThread().getContextClassLoader());
//        if (cacheConfigStream == null)
//            return config; // Cannot find the file so not much point in going further
//
//        //
//        // * Read the contents of the cache configuration stream
//        // * While reading each line, check for the syntax ${xxx} and if found
//        // *           replace with the associated java system property
//        // * Write the new configuration out to a temporary file
//        // * Set the cache configuration property to point to the temporary file instead
//        // * Return a new configuration based on the edited value
//        //
//        BufferedReader reader = null;
//        FileWriter writer = null;
//        try {
//            // Read the cache configuration stream
//            reader = new BufferedReader(new InputStreamReader(cacheConfigStream));
//
//            StringBuilder builder = new StringBuilder();
//            String line;
//
//            while ((line = reader.readLine()) != null) {
//                // No need to replace properties if line does not contain any
//                if (line.contains(DOLLAR_SIGN + OPEN_BRACE)) {
//                    //
//                    // Calls infinispan function that SHOULD have already been called
//                    //
//                    line = StringPropertyReplacer.replaceProperties(line);
//                }
//
//                builder.append(line);
//                builder.append(NEW_LINE);
//            }
//
//            // Create a new temporary file for new configuration
//            String configFilePrefix = "replacement-" + config.getName(); //$NON-NLS-1$
//            File tempConfigFile = File.createTempFile(configFilePrefix, DOT + XML);
//            tempConfigFile.deleteOnExit();
//            writer = new FileWriter(tempConfigFile);
//            writer.write(builder.toString());
//
//            //
//            // Fetch the editable version of the current config and update the
//            // cache configuration path to point to the temporary file
//            //
//            Editor editor = config.edit();
//            EditableDocument storageDoc = editor.getDocument(FieldName.STORAGE);
//            storageDoc.setString(FieldName.CACHE_CONFIGURATION, tempConfigFile.getAbsolutePath());
//
//            // Create a new repository configuration based on the original
//            config = new RepositoryConfiguration(editor, configFilePrefix);
//        } finally {
//            if (reader != null)
//                reader.close();
//            if (writer != null)
//                writer.close();
//        }

        return config;
    }

    private synchronized void startEngine(RepoEngine.Request request) {
        if (RepositoryUtils.isEngineRunning(msEngine))
            return;

        try {
            JcrLogConfigurator.getInstance().setLevel(Level.INFO);

            // start the ModeShape Engine
            msEngine.start();

            // start the local repository
            final RepositoryConfiguration config = initialiseRepositoryConfiguration(this.repoId.getConfiguration());

            //
            // Validate the configuration for any errors
            //
            Problems problems = config.validate();
            if (problems.hasProblems()) {
                Iterator<Problem> iterator = problems.iterator();
                while (iterator.hasNext()) {
                    Problem problem = iterator.next();
                    switch (problem.getStatus()) {
                        case ERROR:
                            // Catastrophic error if the configuration is not valid!
                            throw new Exception(Messages.getString(
                                                                   Messages.LocalRepository.Configuration_Problem,
                                                                   problem.getMessageString()),
                                                                   problem.getThrowable());
                        case WARNING:
                            this.kEngine.getErrorHandler().warn(problem.getMessageString());
                            break;
                        default:
                            this.kEngine.getErrorHandler().error(problem.getThrowable());
                    }
                }
            }

            // Deploy configuration to engine
            JcrRepository repository = msEngine.deploy(config);
            identifier.setRepository(repository);

            //
            // Check for errors in startup
            //
            problems = repository.getStartupProblems();
            if (problems.hasErrors() || problems.hasWarnings()) {
                Iterator<Problem> iterator = problems.iterator();
                while (iterator.hasNext()) {
                    Problem problem = iterator.next();
                    switch (problem.getStatus()) {
                        case ERROR:
                            throw new Exception(Messages.getString(Messages.LocalRepository.Deployment_Failure, problem.getMessageString()), problem.getThrowable());
                        default:
                            this.kEngine.getErrorHandler().error(problem.getThrowable());
                    }
                }
            }

            // Start the repository
            Future<JcrRepository> startRepository = msEngine.startRepository(repository.getName());

            // Await the start of the repository
            startRepository.get(5, TimeUnit.MINUTES);

            // Add the sequencing listener
            sequencers = new KSequencers( identifier );

            respondCallback(request, null);
        } catch (Throwable ex) {
            LOGGER.error(Messages.getString(Messages.Komodo.ERROR_STARTING_ENGINE, ex));
            errorCallback(request, ex);
        }
    }

    private synchronized void stopEngine(RepoEngine.Request request) {
        try {
            if (sequencers != null) {
                sequencers.dispose();
                sequencers = null;
            }

            Future<Boolean> shutdown = msEngine.shutdown();
            // Await the shutdown
            shutdown.get();

            respondCallback(request, null);
        } catch (Exception ex) {
            LOGGER.error(Messages.getString(Messages.Komodo.ERROR_STOPPING_ENGINE, ex));
            errorCallback(request, ex);
        } finally {
            identifier.setRepository(null);
        }
    }

    private synchronized void clear(RepoEngine.Request request)  throws Exception {
        Session session = RepositoryUtils.createSession(identifier);
        if (session == null || !session.isLive())
            return;

        LOGGER.debug("ModeShapeEngineThread.clear: session = {0}", session.hashCode()); //$NON-NLS-1$

        Node rootNode = session.getRootNode();
        NodeIterator children = rootNode.getNodes();
        while(children.hasNext()) {
            Node child = children.nextNode();
            try {
                // since /tko:komodo, /tko:komodo/tko:workspace, /tko:komodo/tko:library, and /tko:komodo/tko:environment
                // nodes are created by the repository configuration file we don't want to delete them. We do want to
                // delete their children though.
                if ( RepositoryImpl.KOMODO_ROOT.equals( child.getPath() ) ) {
                    { // remove all children of workspace
                        assert child.hasNode( Komodo.WORKSPACE );

                        final Node workspace = child.getNode( Komodo.WORKSPACE );
                        final NodeIterator itr = workspace.getNodes();

                        while ( itr.hasNext() ) {
                            final Node kid = itr.nextNode();
                            LOGGER.debug( "ModeShapeEngineThread.clear: deleting node = {0}", kid.getPath() ); //$NON-NLS-1$
                            kid.remove();
                        }
                    }

                    { // remove all children of library
                        assert child.hasNode( Komodo.LIBRARY );

                        final Node library = child.getNode( Komodo.LIBRARY );
                        final NodeIterator itr = library.getNodes();

                        while ( itr.hasNext() ) {
                            final Node kid = itr.nextNode();
                            LOGGER.debug( "ModeShapeEngineThread.clear: deleting node = {0}", kid.getPath() ); //$NON-NLS-1$
                            kid.remove();
                        }
                    }

                    { // remove all children of environment except the validation rules which are loaded at startup
                        assert child.hasNode( Komodo.ENVIRONMENT );

                        final Node env = child.getNode( Komodo.ENVIRONMENT );
                        final NodeIterator itr = env.getNodes();

                        while ( itr.hasNext() ) {
                            final Node kid = itr.nextNode();

                            // don't delete validation rules
                            if (Environment.VALIDATION.equals( kid.getName() ) || Environment.SERVERS.equals( kid.getName() )) {
                                continue;
                            }

                            LOGGER.debug( "ModeShapeEngineThread.clear: deleting node = {0}", kid.getPath() ); //$NON-NLS-1$
                            kid.remove();
                        }
                    }
                } else if (!child.isNodeType("mode:system")) { //$NON-NLS-1$
                    // Cannot legally remove system nodes and they are not created
                    // by the tests anyway so leave them alone
                    child.remove();
                }
            } catch (Exception ex) {
                // No need to display these exceptions
            }
        }

        RepoEngine.Request saveRequest = new JcrEngine.SessionRequest(RepoEngine.RequestType.COMMIT_SESSION,
                                                                                                                       request.getCallback(),
                                                                                                                       new JcrUowDelegateImpl(session),
                                                                                                                       "Clearing-Session"); //$NON-NLS-1$
        commitSession(saveRequest);
    }

    private synchronized void createSession(final RepoEngine.Request request) {
        Object results = null;
        try {
            results = new JcrUowDelegateImpl(identifier);
            LOGGER.debug("ModeShapeEngineThread.createSession: {0}", results.hashCode()); //$NON-NLS-1$
            respondCallback(request, results);
        } catch (Exception ex) {
            errorCallback(request, ex);
        }
    }

    @Override
    public void run() {
        while (!stop) {
            try {
                RepoEngine.Request request = queue.poll(1000L, TimeUnit.MILLISECONDS);

                if (request == null) continue;

                switch (request.getRequestType()) {
                    case START:
                        startEngine(request);
                        break;
                    case STOP:
                        stopEngine(request);
                        stop = true;
                        break;
                    case CLEAR:
                        clear(request);
                        break;
                    case CREATE_SESSION:
                        createSession(request);
                        break;
                    case COMMIT_SESSION:
                        commitSession(request);
                        break;
                    case ROLLBACK_SESSION:
                        rollbackSession(request, null);
                        break;
                    default:
                        break;
                }

            } catch (final Exception e) {
                stop = true;
                error = e;
                this.kEngine.getErrorHandler().error(Messages.getString(Messages.LocalRepository.General_Exception), e);
            }
        }
    }

    @Override
    public void accept( RepoEngine.Request request ) {
        try {
            queue.put(request);
        } catch (InterruptedException ex) {
            this.kEngine.getErrorHandler().error(Messages.getString(Messages.LocalRepository.General_Exception), ex);
        }
    }
}
