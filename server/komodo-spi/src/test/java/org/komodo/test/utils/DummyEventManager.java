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
package org.komodo.test.utils;

import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;

public class DummyEventManager implements EventManager {

    @Override
    public boolean addListener(ExecutionConfigurationListener listener) {
        return true;
    }

    @Override
    public void permitListeners(boolean enable) {
        // Do Nothing
    }

    @Override
    public void notifyListeners(ExecutionConfigurationEvent event) {
        // Do Nothing
    }

    @Override
    public boolean removeListener(ExecutionConfigurationListener listener) {
        return true;
    }
}