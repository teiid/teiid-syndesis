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
package org.komodo.spi.logging;

/**
 *
 */
public interface KLogger {

    enum Level {
        OFF,
        ERROR,
        WARNING,
        INFO,
        DEBUG,
        TRACE;

        public static Level level(String level) {
            if (level == null)
                return null;

            for (Level myLevel : Level.values()) {
                if (myLevel.name().equalsIgnoreCase(level))
                    return myLevel;
            }

            return null;
        }
    }

    /**
     * Dispose of this logger, releasing any resources
     */
    void dispose();

    /**
     * @return path where logging is taking place
     * @throws Exception
     */
    String getLogPath() throws Exception;

    /**
     * Set the path where the logger will log to
     *
     * @param logPath
     * @throws Exception
     */
    void setLogPath(String logPath) throws Exception;

    /**
     * Set the logging level
     *
     * @param level preferred level
     * @throws Exception exception if level change fails
     */
    void setLevel(Level level) throws Exception;

    /**
     * @param message
     * @param args
     */
    void info(String message, Object... args);

    /**
     * @param message
     * @param throwable
     * @param args
     */
    void info(String message, Throwable throwable, Object... args);

    /**
     * @return <code>true</code> if info logging is enabled
     */
    boolean isInfoEnabled();

    /**
     * @param message
     * @param args
     */
    void warn(String message, Object... args);

    /**
     * @param message
     * @param throwable
     * @param args
     */
    void warn(String message, Throwable throwable, Object... args);

    /**
     * @return <code>true</code> if warning logging is enabled
     */
    boolean isWarnEnabled();

    /**
     * @param message
     * @param args
     */
    void error(String message, Object... args);

    /**
     * @param message
     * @param throwable
     * @param args
     */
    void error(String message, Throwable throwable, Object... args);

    /**
     * @return <code>true</code> if error logging is enabled
     */
    boolean isErrorEnabled();

    /**
     * @param message
     * @param args
     */
    void debug(String message, Object... args);

    /**
     * @param message
     * @param throwable
     * @param args
     */
    void debug(String message, Throwable throwable, Object... args);

    /**
     * @return <code>true</code> if debug logging is enabled
     */
    boolean isDebugEnabled();

    /**
     * @param message
     * @param args
     */
    void trace(String message, Object... args);

    /**
     * @param message
     * @param throwable
     * @param args
     */
    void trace(String message, Throwable throwable, Object... args);

    /**
     * @return <code>true</code> if trace logging is enabled
     */
    boolean isTraceEnabled();

}
