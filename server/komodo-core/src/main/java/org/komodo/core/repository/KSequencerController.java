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

import org.komodo.core.internal.repository.KSequencers;


/**
 * Controls the execution of the sequencers
 */
public interface KSequencerController {

    /**
     * The Sequencers executed by {@link KSequencers}
     */
    public static enum SequencerType {
        /**
         * VDB Sequencer
         */
        VDB("VDB Dynamic Sequencer"), //$NON-NLS-1$

        /**
         * DDL Sequencer
         */
        DDL("DDL Sequencer"), //$NON-NLS-1$

        /**
         * Data Service Sequencer
         */
        DATA_SERVICE("Data Service Sequencer"), //$NON-NLS-1$

        /**
         * Teiid SQL Sequencer
         */
        TSQL("Teiid SQL Sequencer"); //$NON-NLS-1$

        private String id;

        private SequencerType(String id) {
            this.id = id;
        }

        @Override
        public String toString() {
            return id;
        }
    }

    /**
     * Dispose of this instance
     */
    void dispose();

    /**
     * @param listener the listener to add
     *
     * @throws Exception if error occurs
     */
    void addSequencerListener(KSequencerListener listener) throws Exception;

}
