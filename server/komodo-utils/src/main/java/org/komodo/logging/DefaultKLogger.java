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
package org.komodo.logging;

import org.komodo.logging.i18n.TextI18n;
import org.komodo.spi.logging.KLogger;

/**
 *
 */
public class DefaultKLogger implements KLogger {

    private Logger logger;

    private Logger getLogger() {
        if (logger == null)
            logger = Logger.getLogger(KLogger.class);

        return logger;
    }

    @Override
    public void dispose() {
        try {
            if (logger != null)
                logger.dispose();
        } catch (Exception ex) {
            error("", ex); //$NON-NLS-1$
        }
        logger = null;
    }

    @Override
    public String getLogPath() throws Exception {
        return getLogger().getLogPath();
    }

    @Override
    public void setLogPath(String logPath) throws Exception {
        getLogger().setLogPath(logPath);
    }

    @Override
    public void setLevel(KLogger.Level level) throws Exception {
        getLogger().setLevel(level);
    }

    /**
     * @param message
     * @return
     */
    private TextI18n getI18n(String message) {
        TextI18n ti18n = new TextI18n(message);
        return ti18n;
    }

    @Override
    public void info(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().info(ti18n, args);
    }

    @Override
    public void info(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().info(throwable, ti18n, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isInfoEnabled()
     */
    @Override
    public boolean isInfoEnabled() {
        return getLogger().isInfoEnabled();
    }

    @Override
    public void warn(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().warn(ti18n, args);
    }

    @Override
    public void warn(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().warn(throwable, ti18n, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isWarnEnabled()
     */
    @Override
    public boolean isWarnEnabled() {
        return isWarnEnabled();
    }

    @Override
    public void error(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().error(ti18n, args);
    }

    @Override
    public void error(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        getLogger().error(throwable, ti18n, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isErrorEnabled()
     */
    @Override
    public boolean isErrorEnabled() {
        return getLogger().isErrorEnabled();
    }

    @Override
    public void debug(String message, Object... args) {
        getLogger().debug(message, args);
    }

    @Override
    public void debug(String message, Throwable throwable, Object... args) {
        getLogger().debug(throwable, message, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isDebugEnabled()
     */
    @Override
    public boolean isDebugEnabled() {
        return getLogger().isDebugEnabled();
    }

    @Override
    public void trace(String message, Object... args) {
        getLogger().trace(message, args);
    }

    @Override
    public void trace(String message, Throwable throwable, Object... args) {
        getLogger().trace(throwable, message, args);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.logging.KLogger#isTraceEnabled()
     */
    @Override
    public boolean isTraceEnabled() {
        return getLogger().isTraceEnabled();
    }

}
