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

import org.komodo.utils.KLog;

/**
 * Composite error handler implemented to delegate errors and warnings
 * to multiple {@link KErrorHandler} implementations as well as logging
 * the message to {@link KLog}.
 */
public class KomodoErrorHandler implements KErrorHandler, KObserver {

    private Set<KErrorHandler> errorHandlers = Collections.emptySet();

    /**
     * @param errorHandler the error handler
     */
    public void add(KErrorHandler errorHandler) {
        if (! (errorHandlers instanceof HashSet))
            errorHandlers = new HashSet<KErrorHandler>();

        errorHandlers.add(errorHandler);
    }

    /**
     * @param errorHandler the error handler
     */
    public void remove(KErrorHandler errorHandler) {
        if (errorHandlers.isEmpty())
            return;

        errorHandlers.remove(errorHandler);
    }

    @Override
    public void warn(String message) {
        KLog.getLogger().warn(message);

        for (KErrorHandler handler : errorHandlers) {
            handler.warn(message);
        }
    }

    @Override
    public void error(String message) {
        KLog.getLogger().error(message);

        for (KErrorHandler handler : errorHandlers) {
            handler.error(message);
        }
    }

    @Override
    public void error(Throwable ex) {
        KLog.getLogger().error(ex == null ? "<no message>" : ex.getLocalizedMessage(), ex); //$NON-NLS-1$

        for (KErrorHandler handler : errorHandlers) {
            handler.error(ex);
        }
    }

    @Override
    public void error(String message, Throwable ex) {
        KLog.getLogger().error(message, ex);

        for (KErrorHandler handler : errorHandlers) {
            handler.error(message, ex);
        }
    }

    @Override
    public void eventOccurred(KEvent<?> event) {
        if (! KLog.getLogger().isDebugEnabled())
            return;

        KLog.getLogger().debug(event.toString());
    }

    @Override
    public void errorOccurred(Throwable e) {
        error(e);
    }
}
