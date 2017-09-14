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
package org.komodo.test.utils;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import org.komodo.core.KEngine;
import org.komodo.spi.KEvent;
import org.komodo.spi.KEvent.Type;
import org.komodo.spi.KObserver;

/**
 * A {@link KObserver} containing a latch that can be used to hold
 * a thread until a state change in the {@link KEngine} has occurred
 */
public class KEngineObserver implements KObserver {

    private CountDownLatch latch;

    private Throwable error;

    private List<Type> targetEvents;

    /**
     * Constructor
     * @param targetEvents the countdown of the latch is dependent on
     *                  the number of events to wait for
     */
    public KEngineObserver(Type... targetEvents) {
        this.targetEvents = Arrays.asList(targetEvents);
        resetLatch();
    }

    /**
     * Reset the latch
     */
    public void resetLatch() {
        latch = new CountDownLatch(targetEvents.size());
    }

    /**
     * @return the latch
     */
    public CountDownLatch getLatch() {
        return this.latch;
    }

    public Throwable getError() {
        return error;
    }

    @Override
    public void eventOccurred(KEvent<?> event) {
        if (! targetEvents.contains(event.getType()))
            return;

        latch.countDown();
    }

    @Override
    public void errorOccurred(Throwable e) {
        error = e;
        latch.countDown();
    }
}