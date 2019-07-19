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

/**
 * Acts like a {@link SynchronousCallback} but will also call a delegate
 * callback added by its constructor. Thus, allowing for a callback to 'do'
 * something as well as being called synchronously.
 */
public class SynchronousNestedCallback extends SynchronousCallback {

    private final SynchronousCallback delegate;

    /**
     * @param delegate additional callback to be called synchronously
     */
    public SynchronousNestedCallback(SynchronousCallback delegate) {
        this.delegate = delegate;
    }

    @Override
    public void respond(Object results) {
        delegate.respond(results);
        super.respond(results);
    }

    @Override
    public void errorOccurred(Throwable error) {
        delegate.errorOccurred(error);
        super.errorOccurred(error);
    }
}
