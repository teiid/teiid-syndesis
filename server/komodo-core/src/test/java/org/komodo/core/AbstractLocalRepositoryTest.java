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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.rules.TestName;
import org.komodo.core.internal.repository.Repository.State;
import org.komodo.core.repository.LocalRepository;
import org.komodo.core.repository.LocalRepository.LocalRepositoryId;
import org.komodo.metadata.TeiidConnectionProvider;
import org.komodo.metadata.internal.DefaultMetadataInstance;
import org.komodo.core.repository.ObjectImpl;
import org.komodo.core.repository.RepositoryImpl;
import org.komodo.core.repository.RepositoryTools;
import org.komodo.spi.KClient;
import org.komodo.spi.KEvent;
import org.komodo.spi.KException;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.SynchronousCallback;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWorkListener;
import org.komodo.utils.KLog;
import org.komodo.utils.observer.KLatchRepositoryObserver;
import org.mockito.Mockito;
import org.modeshape.jcr.api.JcrConstants;

/**
 * Provides framework for testing an instance of the local repository
 * which is only cached in memory hence should be persisted between
 * tests.
 *
 * The initLocalRepository method will be called prior to any tests executing
 * ensuring that the _repo is initialised and reachable. This can be added to
 * the singleton KEngine instance using KEngine.setDefaultRepository if required.
 *
 * When tests are complete, destroyLocalRepository will be called and attempt
 * to stop and close down the repository. Since the repository is in-memory only
 * then nullifying it will destroy all data hence clearance between test classes
 * should be unnecessary. Sub-classes using KEngine should stop the KEngine
 * in an @AfterClass annotated method and use the _repoObserver to await
 * the shutdown of the repository. The destoryLocalRepository function will
 * still run but it should do nothing since _repo is shutdown via the KEngine.
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractLocalRepositoryTest extends AbstractLoggingTest {

    private static final String TEST_REPOSITORY_CONFIG = "internal/test-local-repository-in-memory-config.json";

    protected static final long TIME_TO_WAIT = 1; // in minutes

    protected static LocalRepository _repo = null;

    @BeforeClass
    public static void initRepository() throws Exception {
        URL configUrl = AbstractLocalRepositoryTest.class.getResource(TEST_REPOSITORY_CONFIG);
        LocalRepositoryId id = new LocalRepositoryId(configUrl, DEFAULT_LOCAL_WORKSPACE_NAME);
        _repo = new LocalRepository(id);
        assertThat(_repo.getState(), is(State.NOT_REACHABLE));
        assertThat(_repo.ping(), is(false));

    	KEngineImpl engine = new KEngineImpl();
    	TeiidConnectionProvider provider = Mockito.mock(TeiidConnectionProvider.class);
    	DefaultMetadataInstance metadata = new DefaultMetadataInstance(provider);
    	engine.setMetadataInstance(metadata);
        engine.setDefaultRepository(_repo);

        assertTrue(engine.startAndWait());

        { // verify initial content (see initialContent.xml)
            UnitOfWork transaction = null;

            try {
                transaction = _repo.createTransaction(TEST_USER, "verifyInitialRepositoryContent", true, null, TEST_USER);
                final KomodoObject workspace = _repo.komodoWorkspace(transaction);
                workspace.getName( transaction );
                transaction.commit();
            } catch ( final Exception e ) {
                throw new Exception( "Failed verifying initial workspace content: " + e.getLocalizedMessage(), e );
            } finally {
                if ( transaction != null ) {
                    transaction.commit();
                }
            }
        }
    }

    /**
     * Shutdown and destroy repo
     *
     * @throws Exception
     */
    @AfterClass
    public static void destroyLocalRepository() throws Exception {
        assertNotNull(_repo);

        KLatchRepositoryObserver _repoShutdownObserver = new KLatchRepositoryObserver(KEvent.Type.REPOSITORY_STOPPED);
        _repo.addObserver(_repoShutdownObserver);

        KClient client = mock(KClient.class);
        RepositoryClientEvent event = RepositoryClientEvent.createShuttingDownEvent(client);
        _repo.notify(event);

        try {
            if (! _repoShutdownObserver.getLatch().await(TIME_TO_WAIT, TimeUnit.MINUTES))
                fail("Local repository was not stopped");
        } finally {
            _repo.removeObserver(_repoShutdownObserver);
            _repo = null;
        }
    }

    @Rule
    public TestName name = new TestName();
    private boolean rollbackOnly = false;
    private int txCount;
    private UnitOfWork uow;
    private UnitOfWork sysUow;
    protected SynchronousCallback callback;
    protected SynchronousCallback sysCallback;

    @Before
    public void createInitialTransactions() throws Exception {
        this.callback = new TestTransactionListener();
        this.uow = createTransaction(callback);

        this.sysCallback = new TestTransactionListener();
        this.sysUow = createTransaction(SystemConstants.SYSTEM_USER, txId(SystemConstants.SYSTEM_USER, "tx"), false, this.sysCallback);
        KLog.getLogger().debug( "\n\n ----- Test {0}: createInitialTransactions() finished", this.name.getMethodName() );
    }

    @After
    public void cleanup() throws Exception {
        { // process current transaction if necessary
            if ( this.uow != null ) {
                switch ( this.uow.getState() ) {
                    case NOT_STARTED:
                    case RUNNING:
                        rollback();
                        break;
                    case COMMITTED:
                    case ERROR:
                    case ROLLED_BACK:
                    default:
                        break;
                }

                this.uow = null;
                this.callback = null;
                this.txCount = 0;
            }
        }

        //
        // process sys transaction if necessary
        //
        if ( this.sysUow != null ) {
            switch ( this.sysUow.getState() ) {
                case NOT_STARTED:
                case RUNNING:
                    sysRollback();
                    break;
                case COMMITTED:
                case ERROR:
                case ROLLED_BACK:
                default:
                    break;
            }

            this.sysUow = null;
            this.sysCallback = null;
        }

        { // clean repository
            assertNotNull( _repo );

            if ( !State.REACHABLE.equals( _repo.getState() ) ) return;

            KLatchRepositoryObserver _repoClearObserver = new KLatchRepositoryObserver(KEvent.Type.REPOSITORY_CLEARED);
            _repo.addObserver(_repoClearObserver);

            KClient client = mock( KClient.class );
            RepositoryClientEvent event = RepositoryClientEvent.createClearEvent( client );
            _repo.notify( event );

            try {
                if ( !_repoClearObserver.getLatch().await( TIME_TO_WAIT, TimeUnit.MINUTES ) )
                    throw new RuntimeException( "Local repository was not cleared" );
            } finally {
                _repo.removeObserver(_repoClearObserver);
            }

            KLog.getLogger().debug( "Test {0}: clearLocalRepository() finished\n\n=====\n\n", this.name.getMethodName() );
        }
    }

    protected static void commit( UnitOfWork currTx,
                                                              SynchronousCallback currCallback,
                                                              UnitOfWork.State expectedState) throws Exception {
        currTx.commit();

        assertThat( currCallback.await( TIME_TO_WAIT, TimeUnit.MINUTES ), is( true ) );

        if (expectedState == UnitOfWork.State.ERROR) {
            assertThat( currTx.getState(), is( expectedState ) );
        } else {
            assertThat( currTx.getError(), is( nullValue() ) );
            assertThat( currTx.getState(), is( expectedState ) );
        }

        if ( currCallback instanceof TestTransactionListener ) {
            final boolean respond = ( UnitOfWork.State.COMMITTED == expectedState );

            if ( !respond ) {
                assertThat( expectedState, is( UnitOfWork.State.ERROR ) );
            }

            assertThat( ( ( TestTransactionListener )currCallback ).respondCallbackReceived(), is( respond ) );
            assertThat( ( ( TestTransactionListener )currCallback ).errorCallbackReceived(), is( !respond ) );
        }
    }

    protected static void commit(UnitOfWork currTx, UnitOfWork.State expectedState) throws Exception {
        assertTrue(currTx.getCallback() instanceof SynchronousCallback);
        commit(currTx, (SynchronousCallback) currTx.getCallback(), expectedState);
    }

    private void commit( final UnitOfWork.State expectedState, final SynchronousCallback nextCallback ) throws Exception {
        commit(this.uow, this.callback, expectedState);

        this.callback = nextCallback;
        this.uow = _repo.createTransaction(TEST_USER, this.name.getMethodName(), this.rollbackOnly, this.callback, TEST_USER);
    }

    protected void commit( final UnitOfWork.State expectedState ) throws Exception {
        final SynchronousCallback nextCallback = new TestTransactionListener();
        commit( expectedState, nextCallback );
    }

    protected void commit() throws Exception {
        commit(UnitOfWork.State.COMMITTED);
    }

    protected void sysCommit( final UnitOfWork.State expectedState, final SynchronousCallback nextCallback ) throws Exception {
        commit(this.sysUow, this.sysCallback, expectedState);

        this.sysCallback = nextCallback;
        this.sysUow = createTransaction(SystemConstants.SYSTEM_USER, txId(SystemConstants.SYSTEM_USER, "sysTx"), false, sysCallback);
    }

    protected void sysCommit( final UnitOfWork.State expectedState ) throws Exception {
        final SynchronousCallback nextCallback = new TestTransactionListener();
        sysCommit( expectedState, nextCallback );
    }

    protected void sysCommit() throws Exception {
        sysCommit(UnitOfWork.State.COMMITTED);
    }

    protected void useCustomCallback( final SynchronousCallback callback,
                                      final boolean commitCurrentTransaction ) throws Exception {
        if ( commitCurrentTransaction ) {
            commit( UnitOfWork.State.COMMITTED, callback );
        } else {
            rollback( callback );
        }
    }

    protected static UnitOfWork createTransaction( String user, String txName, boolean rollback, final UnitOfWorkListener callback ) throws Exception {
        return _repo.createTransaction(user, txName , rollback, callback, user);
    }

    private UnitOfWork createTransaction( final UnitOfWorkListener callback ) throws Exception {
        return createTransaction(TEST_USER, (this.name.getMethodName() + '-' + this.txCount++), this.rollbackOnly, callback);
    }

    protected static String txId(String... components) {
        StringBuffer buf = new StringBuffer();

        assertNotNull(components);
        for (int i = 0; i < components.length; ++i) {
            buf.append(components[i]);
            if (i < (components.length - 1))
                buf.append(HYPHEN);
        }

        return buf.toString();
    }

    /**
     * System Transaction can search from workspace root rather than just inside home directory
     */
    protected UnitOfWork sysTx() throws Exception {
        return this.sysUow;
    }

    /**
     * The transaction object should <strong>NOT</strong> be cached.
     *
     * @return the current transaction (never <code>null</code>)
     */
    protected UnitOfWork getTransaction() {
        return this.uow;
    }

    protected static void rollback( UnitOfWork currTx, SynchronousCallback currCallback) throws Exception {
        currTx.rollback();

        assertThat( currCallback.await( TIME_TO_WAIT, TimeUnit.MINUTES ), is( true ) );
        assertThat( currTx.getError(), is( nullValue() ) );
        assertThat( currTx.getState(), is( UnitOfWork.State.ROLLED_BACK ) );

        if ( currCallback instanceof TestTransactionListener ) {
            assertThat( ( ( TestTransactionListener )currCallback ).respondCallbackReceived(), is( true ) );
            assertThat( ( ( TestTransactionListener )currCallback ).errorCallbackReceived(), is( false ) );
        }
    }

    private void rollback( final SynchronousCallback nextCallback ) throws Exception {
        rollback(this.uow, this.callback);

        // create new transaction
        this.callback = nextCallback;
        this.uow = createTransaction( this.callback );
    }

    private void sysRollback( final SynchronousCallback nextCallback ) throws Exception {
        rollback(this.sysUow, this.sysCallback);

        // create new transaction
        this.sysCallback = nextCallback;
        this.sysUow = createTransaction( this.sysCallback );
    }

    protected void rollback() throws Exception {
        rollback( new TestTransactionListener() );
    }

    protected void sysRollback() throws Exception {
        sysRollback( new TestTransactionListener() );
    }

    protected void traverse(UnitOfWork uow, KomodoObject node, StringBuffer buffer) throws Exception {
        String traversal = RepositoryTools.traverse(uow, node);
        buffer.append(traversal);
    }

    protected void traverse(UnitOfWork uow, KomodoObject node) throws Exception {
        StringBuffer buffer = new StringBuffer(NEW_LINE);
        traverse(uow, node, buffer);
        KLog.getLogger().info(buffer.toString());
    }

    protected void traverse(UnitOfWork uow, String nodePath) throws Exception {
        KomodoObject ko = new ObjectImpl(_repo, nodePath, 0);
        traverse(uow, ko);
    }

    protected void traverse(UnitOfWork uow) throws Exception {
        traverse(uow, RepositoryImpl.KOMODO_ROOT);
    }

    /**
     * @param property
     * @return String representation of property and its values
     * @throws Exception
     */
    @SuppressWarnings( "unused" )
    private String toString(Property property) throws Exception {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append(property.getName(this.uow)).append('=');
            if (property.isMultiple(this.uow)) {
                sb.append('[');
                Object[] values = property.getValues(this.uow);
                for (int i = 0; i < values.length; ++i) {
                    Object value = values[i];
                    sb.append(value);
                    if ((i + 1) < values.length)
                        sb.append(',');
                }
                sb.append(']');
            } else {
                Object value = property.getValue(this.uow);
                sb.append(value);
            }
        } catch (Exception e) {
            sb.append(" on deleted node ").append(property.getAbsolutePath());
        }

        return sb.toString();
    }

    protected void verifyProperty( UnitOfWork uow, KomodoObject node, String propertyName, String... expectedValues ) throws KException {
        Property property = node.getRawProperty(getTransaction(), propertyName);
        assertNotNull(property);

        if ( property.isMultiple( getTransaction() ) ) {
            final List< String > values = Arrays.asList( property.getStringValues( getTransaction() ) );
            assertThat( values.size(), is( expectedValues.length ) );

            for ( String expectedValue : expectedValues ) {
                assertTrue( values.contains( expectedValue ) );
            }
        } else {
            assertThat( property.getStringValue( getTransaction() ), is( expectedValues[ 0 ] ) );
        }
    }

    protected void verifyProperty( UnitOfWork uow, KomodoObject node, String propertyName, long expectedValue ) throws KException {
        Property property = node.getProperty(uow, propertyName);
        Long value = property.isMultiple(uow) ? property.getLongValues(uow)[0] : property.getLongValue(uow);
        assertEquals(expectedValue, value.longValue());
    }

    protected void verifyProperty( UnitOfWork uow, KomodoObject node, String propertyName, boolean expectedValue ) throws KException {
        Property property = node.getProperty(uow, propertyName);
        Boolean value = property.isMultiple(uow) ? property.getBooleanValues(uow)[0] : property.getBooleanValue(uow);
        assertEquals(expectedValue, value.booleanValue());
    }

    protected void verifyProperty( UnitOfWork uow, KomodoObject node, String propertyName, java.sql.Date expectedValue ) throws KException {
        Property property = node.getProperty(uow, propertyName);
        Object value = property.isMultiple(uow) ? property.getValues(uow)[0] : property.getValue(uow);
        assertEquals(expectedValue, java.sql.Date.valueOf(value.toString()));
    }

    protected void verifyProperty( UnitOfWork uow, KomodoObject node, String propertyName, java.sql.Time expectedValue ) throws KException {
        Property property = node.getProperty(uow, propertyName);
        Object value = property.isMultiple(uow) ? property.getValues(uow)[0] : property.getValue(uow);
        assertEquals(expectedValue, java.sql.Time.valueOf(value.toString()));
    }

    protected boolean verifyHasProperty( UnitOfWork uow, KomodoObject node, String propNameStr ) throws KException {
        return node.hasProperty(uow, propNameStr);
    }

    protected void verifyPrimaryType( UnitOfWork uow, KomodoObject node, String expectedValue ) throws KException {
        verifyProperty(uow, node, JcrConstants.JCR_PRIMARY_TYPE, expectedValue);
    }

    protected void verifyMixinType( UnitOfWork uow, KomodoObject node, String expectedValue ) throws KException {
        assertTrue(node.hasDescriptor(uow, expectedValue));
    }

    protected void verifyMixinTypes( UnitOfWork uow, KomodoObject node, String... expectedValues ) throws KException {
        for (String expectedValue : expectedValues) {
            verifyMixinType(uow, node, expectedValue);
        }
    }

    protected void verifyBaseProperties( UnitOfWork uow, KomodoObject node, String primaryType, String mixinType) throws KException {
        verifyPrimaryType(uow, node, primaryType);
        if (mixinType == null)
            return;

        // Only if mixinType is not null do we check it
        verifyMixinType(uow, node, mixinType);
    }

    protected KomodoObject verify(UnitOfWork uow, KomodoObject parentNode, String relativePath, int index, String primaryType, String mixinType) throws Exception {
        String indexExp = EMPTY_STRING;
        if (index > -1)
            indexExp = OPEN_SQUARE_BRACKET + index + CLOSE_SQUARE_BRACKET;

        KomodoObject childNode = null;
        if (parentNode.hasRawChild(uow, relativePath))
            childNode = parentNode.getChild(uow, relativePath + indexExp);

//        traverse(uow, parentNode);
        assertNotNull(childNode);

        verifyBaseProperties(uow, childNode, primaryType, mixinType);
        return childNode;
    }

    protected KomodoObject verify(UnitOfWork uow, KomodoObject parentNode, String relativePath, String primaryType, String mixinType) throws Exception {
        return verify(uow, parentNode, relativePath, -1, primaryType, mixinType);
    }

    protected KomodoObject verify(UnitOfWork uow, KomodoObject parentNode, String relativePath, String mixinType) throws Exception {
        return verify(uow, parentNode, relativePath, -1, JcrConstants.NT_UNSTRUCTURED, mixinType);
    }

    protected KomodoObject verify(UnitOfWork uow, KomodoObject parentNode, String relativePath) throws Exception {
        return verify(uow, parentNode, relativePath, -1, JcrConstants.NT_UNSTRUCTURED, null);
    }

    protected static class TestTransactionListener extends SynchronousCallback {

        private boolean errorCallback = false;
        private boolean successCallback = false;

        public TestTransactionListener() {
            // Nothing to do
        }

        protected boolean errorCallbackReceived() {
            return this.errorCallback;
        }

        protected boolean respondCallbackReceived() {
            return this.successCallback;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.SynchronousCallback#errorOccurred(java.lang.Throwable)
         */
        @Override
        public void errorOccurred( final Throwable error ) {
            this.errorCallback = true;
            super.errorOccurred( error );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.SynchronousCallback#respond(java.lang.Object)
         */
        @Override
        public void respond( final Object results ) {
            this.successCallback = true;
            super.respond( results );
        }

    }

}
