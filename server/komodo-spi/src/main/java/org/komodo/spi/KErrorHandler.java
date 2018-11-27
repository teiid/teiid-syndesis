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
package org.komodo.spi;

/**
 * Handler for processing warnings and errors. Providing a handler implementation
 * to the KEngine will enable UI clients to display errors appropriately in their own
 * environment.
 */
public interface KErrorHandler {

    /**
     * Handle an error message
     *
     * @param message
     */
    void error(String message);

    /**
     * Handle an error
     *
     * @param ex
     */
    void error(Throwable ex);

    /**
     * Handle an error with overview message
     *
     * @param message
     * @param ex
     */
    void error(String message, Throwable ex);

    /**
     * Handle a warning message
     *
     * @param message
     */
    void warn(String message);

}
