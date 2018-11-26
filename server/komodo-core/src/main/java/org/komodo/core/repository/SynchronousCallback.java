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

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;

/**
 * Makes an asynchronous transaction commit operation synchronous.
 *
 * <p>
 * Add an instance of this class to a transaction:
 * <p>
 * {@code SynchronousCallback callback = new SynchronousCallback();
                 UnitOfWork transaction = repo.createTransaction("the-transaction", false, callback);
    }
 * <p>
 * <p>
 * At the point of committing the transaction, do the following:
 * <p>
 * {@code transaction.commit();
                 if (! callback.wait(3, TimeUnit.MINUTES)) throw someException
                 if (callback.hasError()) throw someException
    }
 * <p>
 * <p>
 * This will hold the thread committing the transaction until it has completely
 * finished.
 */
public class SynchronousCallback implements UnitOfWorkListener {

    private final CountDownLatch latch = new CountDownLatch(1);

    private Throwable error;

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
    public void respond(Object results) {
        latch.countDown();
    }

    @Override
    public void errorOccurred(Throwable error) {
        this.error = error;
        latch.countDown();
    }

    /**
     * @return the error
     */
    public Throwable error() {
        return this.error;
    }

    /**
     * @return true if error occurred
     */
    public boolean hasError() {
        return this.error != null;
    }

}
