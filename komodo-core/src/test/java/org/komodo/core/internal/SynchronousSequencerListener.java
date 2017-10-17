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
package org.komodo.core.internal;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.jcr.Session;
import org.komodo.core.internal.repository.JcrUowDelegate;
import org.komodo.core.repository.KSequencerController;
import org.komodo.core.repository.KSequencerListener;
import org.komodo.spi.repository.UnitOfWorkDelegate;
import org.komodo.utils.KLog;

/**
 * Listener that will wait for the sequencers to complete prior to letting
 * the thread calling {@link #wait()} to continue.
 */
public class SynchronousSequencerListener implements KSequencerListener {

    private static class ListenerUnitOfWorkDelegate implements JcrUowDelegate {

        private final Session session;

        public ListenerUnitOfWorkDelegate(Session session) {
            this.session = session;
        }

        @Override
        public Session getImplementation() {
            return session;
        }

        @Override
        public boolean hasPendingChanges() throws Exception {
            return session.hasPendingChanges();
        }

        @Override
        public boolean isLive() {
            return session.isLive();
        }

        @Override
        public void save() throws Exception {
            session.save();
        }

        @Override
        public void complete() {
            session.logout();
        }

        @Override
        public void refresh(boolean keepChanges) throws Exception {
            session.refresh(keepChanges);
        }
    }

    private final CountDownLatch latch = new CountDownLatch(1);

    private final KSequencerController sequencers;

    private final String listenerId;

    private final UnitOfWorkDelegate sessionDelegate;

    private Exception sequencerException = null;

    /**
     * @param listenerId the id of this listener
     * @param session the session
     * @param sequencers the sequencers
     * @throws Exception if error occurs
     */
    public SynchronousSequencerListener(String listenerId, Session session, KSequencerController sequencers) throws Exception {
        this.listenerId = listenerId;
        this.sequencers = sequencers;
        this.sessionDelegate = new ListenerUnitOfWorkDelegate(session);
        this.sequencers.addSequencerListener(this);        
    }

    /**
     * Wait for the completion of the sequencers
     *
     * @param timeout the maximum time to wait
     * @param unit the time unit of the {@code timeout} argument
     * @return {@code true} if the count reached zero and {@code false}
     *         if the waiting time elapsed before the count reached zero
     * @throws Exception if error occurs
     */
    public boolean await(long timeout, TimeUnit unit) throws Exception {
        return latch.await(timeout, unit);
    }

    @Override
    public String id() {
        return listenerId;
    }

    @Override
    public UnitOfWorkDelegate session() {
        return sessionDelegate;
    }

    @Override
    public void sequencingCompleted() {
        latch.countDown();
    }

    @Override
    public void sequencingError(Exception ex) {
        sequencerException = ex;
        KLog.getLogger().error("Test Sequencer failure: "); //$NON-NLS-1$
        sequencerException.printStackTrace();

        latch.countDown();
    }

    /**
     * @return true if exception occurred
     */
    public boolean exceptionOccurred() {
        return sequencerException != null;
    }

    /**
     * @return any exception that may have occurred or null.
     */
    public Exception exception() {
        return sequencerException;
    }

    @Override
    public void abort() {
        throw new UnsupportedOperationException();
    }
}