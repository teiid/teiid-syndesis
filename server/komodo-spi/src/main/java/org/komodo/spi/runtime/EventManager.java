/*************************************************************************************
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
package org.komodo.spi.runtime;


/**
 *
 *
 *
 */
public interface EventManager {

    /**
     * Listeners already registered will not be added again. The new listener will receive events for all existing teiid instances.
     * 
     * @param listener the listener being register to receive events (never <code>null</code>)
     * @return <code>true</code> if listener was added
     */
    boolean addListener( ExecutionConfigurationListener listener );

    /**
     * Enable / disable the listeners in the event manager
     *
     * @param enable
     */
    void permitListeners(boolean enable);

    /**
     * @param event the event the registry listeners are to process
     */
    void notifyListeners( ExecutionConfigurationEvent event );

    /**
     * @param listener the listener being unregistered and will no longer receive events (never <code>null</code>)
     * @return <code>true</code> if listener was removed
     */
    boolean removeListener( ExecutionConfigurationListener listener );
    
    /**
     * An <code>EventManager</code> that does not do anything.
     */
    EventManager EVENT_MANAGER_ADAPTER = new EventManager() {

        @Override
        public boolean addListener( ExecutionConfigurationListener listener ) {
            return true;
        }

        @Override
        public void notifyListeners( ExecutionConfigurationEvent event ) {
            // nothing to do
        }

        @Override
        public boolean removeListener( ExecutionConfigurationListener listener ) {
            return true;
        }

        @Override
        public void permitListeners(boolean enable) {
            //TODO Not yet implemented
        }
        
    };

}
