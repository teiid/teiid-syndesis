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
package org.komodo.utils.observer;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import org.komodo.spi.KEvent;
import org.komodo.spi.KEvent.Type;
import org.komodo.spi.KObserver;

/**
 * A {@link KObserver} containing a latch that can be used to hold
 * a thread until a state change in the {@link KEngine} has occurred
 */
public class KLatchObserver implements KObserver {

    private CountDownLatch latch;

    private Throwable error;

    private List<Type> targetEvents;

    /**
     * Constructor
     * @param targetEvents the countdown of the latch is dependent on
     *                  the number of events to wait for
     */
    public KLatchObserver(Type... targetEvents) {
        this.targetEvents = new ArrayList<Type>();
        this.targetEvents.addAll(Arrays.asList(targetEvents));
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

        //
        // Avoids counting down if the same event if
        // fired more than once
        //
        targetEvents.remove(event.getType());
    }

    @Override
    public void errorOccurred(Throwable e) {
        error = e;
        latch.countDown();
    }
}