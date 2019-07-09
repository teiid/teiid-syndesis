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
package org.komodo.utils;

import org.apache.commons.logging.LogFactory;
import org.komodo.logging.i18n.TextI18n;

public class KLog {

    private static KLog instance;

    /**
     * @return singleton instance of this logger
     */
    public static KLog getLogger() {
        if (instance == null)
            instance = new KLog();

        return instance;
    }

    private final org.apache.commons.logging.Log kLogger = LogFactory.getLog(KLog.class);

    private TextI18n getI18n(String message) {
        TextI18n ti18n = new TextI18n(message);
        return ti18n;
    }

    public synchronized void info(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.info(ti18n.text(args));
    }

    public synchronized void info(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.info(ti18n.text(args), throwable);
    }

    public boolean isInfoEnabled() {
        return this.kLogger.isInfoEnabled();
    }

    public synchronized void warn(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.warn(ti18n.text(args));
    }

    public synchronized void warn(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.warn(ti18n.text(args), throwable);
    }

    public boolean isWarnEnabled() {
        return this.kLogger.isWarnEnabled();
    }

    public synchronized void error(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.error(ti18n.text(args));
    }

    public synchronized void error(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.error(ti18n.text(args), throwable);
    }

    public boolean isErrorEnabled() {
        return this.kLogger.isErrorEnabled();
    }

    public synchronized void debug(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.debug(ti18n.text(args));
    }

    public synchronized void debug(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.debug(ti18n.text(args), throwable);
    }

    public boolean isDebugEnabled() {
        return this.kLogger.isDebugEnabled();
    }

    public synchronized void trace(String message, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.trace(ti18n.text(args));
    }

    public synchronized void trace(String message, Throwable throwable, Object... args) {
        TextI18n ti18n = getI18n(message);
        kLogger.trace(ti18n.text(args), throwable);
    }

    public boolean isTraceEnabled() {
        return this.kLogger.isTraceEnabled();
    }
}
