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
package org.komodo.core.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;
import java.util.concurrent.TimeUnit;
import org.junit.Test;
import org.komodo.core.KEngine;
import org.komodo.spi.KClient.State;
import org.komodo.spi.KEvent;
import org.komodo.spi.KException;
import org.komodo.spi.metadata.MetadataServer;
import org.komodo.spi.repository.Repository;
import org.komodo.test.utils.AbstractLoggingTest;
import org.komodo.test.utils.KEngineObserver;

public class TestKEngine extends AbstractLoggingTest {

    private void startEngine() throws KException, InterruptedException {
        KEngine engine = KEngine.getInstance();

        KEngineObserver engineObserver = new KEngineObserver(KEvent.Type.REPOSITORY_STARTED, KEvent.Type.METADATA_SERVER_STARTED);
        assertNotNull(engineObserver);
        engine.addObserver(engineObserver);
        engine.start();

        // Wait for the starting of the kengine or timeout of 30 seconds
        if (!engineObserver.getLatch().await(30, TimeUnit.SECONDS)) {
            fail("Test timed-out waiting for kengine to start");
        }

        assertNull(engineObserver.getError());

        assertEquals(State.STARTED, engine.getState());
        assertEquals(Repository.State.REACHABLE, engine.getDefaultRepository().getState());
        assertEquals(MetadataServer.Condition.REACHABLE, engine.getMetadataServer().getCondition());
    }

    private void shutdownEngine() throws Exception {
        KEngine engine = KEngine.getInstance();

        KEngineObserver engineObserver = new KEngineObserver(KEvent.Type.REPOSITORY_STOPPED, KEvent.Type.METADATA_SERVER_STOPPED);
        assertNotNull(engineObserver);
        engine.addObserver(engineObserver);
        engine.shutdownAndWait();

        // Wait for the starting of the kengine or timeout of 30 seconds
        if (!engineObserver.getLatch().await(30, TimeUnit.SECONDS)) {
            fail("Test timed-out waiting for kengine to stop");
        }

        assertNull(engineObserver.getError());

        assertEquals(State.SHUTDOWN, engine.getState());
        assertEquals(Repository.State.NOT_REACHABLE, engine.getDefaultRepository().getState());
        assertEquals(MetadataServer.Condition.NOT_REACHABLE, engine.getMetadataServer().getCondition());
    }

    @Test
    public void testStart() throws Exception {
        startEngine();
    }

    @Test
    public void testShutdown() throws Exception {
        startEngine();
        shutdownEngine();
    }
}
