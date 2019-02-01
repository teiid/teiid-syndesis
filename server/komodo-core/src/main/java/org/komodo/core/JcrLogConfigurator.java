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

import java.util.logging.Level;

import org.apache.log4j.Logger;



/**
 * TODO
 *
 * Provides a workaround for a bug in Modeshape's JcrObservationManager (to-be-reported)
 * If the modeshape logger, which uses log4j, is given DEBUG level then all changesets are logged
 * in JcrObservationManager.JcrPropertyEvent (L808). Since the changeEvent.toString contains sets of {..}
 * the logger wrongly interprets these for logging substitution patterns. This falls foul of StringUtil (L188)
 * and throws an {@link IllegalArgumentException}.
 *
 * This series of events has the effect of causing the KSequencers' consumer wrapper to be removed from
 * the modeshape RingBuffer (L466). With the KSequencers class gone, no notifications are ever transmitted
 * back from the JcrEngine and any SynchronousListeners expecting a callback fail.
 *
 * Should be an easy fix ....
 */
public class JcrLogConfigurator {

    private String level = "INFO"; //$NON-NLS-1$

    private static JcrLogConfigurator instance;

    /**
     * @return singleton instance
     * @throws Exception exception if singleton fails
     */
    public static JcrLogConfigurator getInstance() throws Exception {
        if (instance == null) {
            instance = new JcrLogConfigurator();
            instance.setLevel(Level.INFO);
        }

        return instance;
    }

    /**
     * Set the level of the logging configuration. Levels are not completely identical
     * to Modeshape logging levels so method approximates and converts accordingly.
     *
     * @param level level for logging
     * @throws Exception exception
     */
    public void setLevel(Level level) throws Exception {
        if (Level.OFF.equals(level) || Level.ALL.equals(level) || Level.INFO.equals(level))
            this.level = level.getName();
        else if (Level.SEVERE.equals(level))
            this.level = org.apache.log4j.Level.FATAL.toString();
        else if (Level.WARNING.equals(level))
            this.level = org.apache.log4j.Level.WARN.toString();
        else if (Level.FINE.equals(level))
            this.level = org.apache.log4j.Level.DEBUG.toString();
        else if (Level.FINER.equals(level) || Level.FINEST.equals(level))
            this.level = org.apache.log4j.Level.TRACE.toString();
        else
            this.level = org.apache.log4j.Level.INFO.toString();

        // This is the root logger provided by log4j
        Logger rootLogger = Logger.getRootLogger();
        rootLogger.setLevel(org.apache.log4j.Level.toLevel(this.level));
    }
}
