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

public class KLog {
	
    private final static String BLANK = "";

    private static String format(String message, Object... arguments ) {
    	if (message == null) {
    		message = BLANK;
    	}
    	if (arguments == null || arguments.length == 0) {
    		return message;
    	}
        return String.format(message, arguments);
    }

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

    public void info(String message, Object... args) {
        kLogger.info(format(message, args));
    }

    public void info(String message, Throwable throwable, Object... args) {
        kLogger.info(format(message, args), throwable);
    }

    public boolean isInfoEnabled() {
        return this.kLogger.isInfoEnabled();
    }

    public void warn(String message, Object... args) {
        kLogger.warn(format(message, args));
    }

    public void warn(String message, Throwable throwable, Object... args) {
        kLogger.warn(format(message, args), throwable);
    }

    public boolean isWarnEnabled() {
        return this.kLogger.isWarnEnabled();
    }

    public void error(String message, Object... args) {
        kLogger.error(format(message, args));
    }

    public void error(String message, Throwable throwable, Object... args) {
        kLogger.error(format(message, args), throwable);
    }

    public boolean isErrorEnabled() {
        return this.kLogger.isErrorEnabled();
    }

    public void debug(String message, Object... args) {
    	if (!isDebugEnabled()) {
    		return;
    	}
        kLogger.debug(format(message, args));
    }

    public void debug(String message, Throwable throwable, Object... args) {
    	if (!isDebugEnabled()) {
    		return;
    	}
        kLogger.debug(format(message, args), throwable);
    }

    public boolean isDebugEnabled() {
        return this.kLogger.isDebugEnabled();
    }

    public void trace(String message, Object... args) {
    	if (!isTraceEnabled()) {
    		return;
    	}
        kLogger.trace(format(message, args));
    }

    public void trace(String message, Throwable throwable, Object... args) {
    	if (!isTraceEnabled()) {
    		return;
    	}
        kLogger.trace(format(message, args), throwable);
    }

    public boolean isTraceEnabled() {
        return this.kLogger.isTraceEnabled();
    }
}
