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
