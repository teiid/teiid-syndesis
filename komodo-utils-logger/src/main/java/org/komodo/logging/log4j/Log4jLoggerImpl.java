/*
 * ModeShape (http://www.modeshape.org)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.logging.log4j;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;
import org.komodo.logging.i18n.I18nResource;
import org.komodo.logging.util.StringUtil;
import org.komodo.spi.logging.KLogger;

/**
 * {@link org.modeshape.common.logging.Logger} implementation which uses a Log4j logger to perform the logging operation
 *
 * @author Horia Chiorean
 */
final class Log4jLoggerImpl extends org.komodo.logging.Logger {

    private static final String CONSOLE_APPENDER = "KLOG-CONSOLE"; //$NON-NLS-1$

    private static final String FILE_APPENDER = "KLOG-FILE"; //$NON-NLS-1$

    private RollingFileAppender fileAppender;

    private final org.apache.log4j.Logger logger;

    public Log4jLoggerImpl(String name) {
        logger = Logger.getLogger(name);
    }

    private org.apache.log4j.Level convertLoggingLevel(KLogger.Level level) {
        org.apache.log4j.Level log4jLevel;
        switch (level) {
            case DEBUG:
                log4jLevel = org.apache.log4j.Level.DEBUG;
                break;
            case ERROR:
                log4jLevel = org.apache.log4j.Level.ERROR;
                break;
            case OFF:
                log4jLevel = org.apache.log4j.Level.OFF;
                break;
            case TRACE:
                log4jLevel = org.apache.log4j.Level.TRACE;
                break;
            case WARNING:
                log4jLevel = org.apache.log4j.Level.WARN;
                break;
            case INFO:
            default:
                log4jLevel = org.apache.log4j.Level.INFO;
                break;

        }
        return log4jLevel;
    }

    @Override
    protected void configureLogging() throws Exception {
        // get default log file path if necessary
        if ((this.logPath == null) || this.logPath.isEmpty()) {
            String tempPath = System.getProperty("komodo.dataDir"); //$NON-NLS-1$
            if (tempPath == null) {
                tempPath = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
            }
            tempPath += File.separator + "komodo.log"; //$NON-NLS-1$
            this.logPath = tempPath;
        }

        // make sure log file exists
        final Path logFilePath = Paths.get(this.logPath);

        if (!Files.exists(logFilePath)) {
            if (!Files.exists(logFilePath.getParent())) {
                Files.createDirectories(logFilePath.getParent());
            }

            Files.createFile(logFilePath);
        }

        logger.setLevel(convertLoggingLevel(this.level));

        if (logger.getAppender(CONSOLE_APPENDER) == null) {
            //Add console appender to root logger
            PatternLayout layout = new PatternLayout("%d{ISO8601} [%t] %-5p %c %x - %m%n"); //$NON-NLS-1$
            ConsoleAppender consoleAppender = new ConsoleAppender(layout);
            consoleAppender.setName(CONSOLE_APPENDER);
            logger.addAppender(consoleAppender);
        }

        if (logger.getAppender(FILE_APPENDER) == null) {
            //Add console appender to root logger
            PatternLayout layout = new PatternLayout("%d{ISO8601} [%t] %-5p %c %x - %m%n"); //$NON-NLS-1$
            fileAppender = new RollingFileAppender(layout, this.logPath);
            fileAppender.setName(FILE_APPENDER);
            logger.addAppender(fileAppender);
        }
    }

    @Override
    public void dispose() {
        if (fileAppender != null) {
            if (logger != null)
                logger.removeAppender(fileAppender);

            fileAppender.close();
            fileAppender = null;
        }
    }

    @Override
    public void setLevel(KLogger.Level level) throws Exception {
        super.setLevel(level);
        configureLogging();
    }

    @Override
    public void setLogPath(String logPath) throws Exception {
        super.setLogPath(logPath);

        dispose();

        if (this.logPath != null) {
            // Tidy up the old log path if it exists and is empty
            File oldLog = new File(this.logPath);
            if (oldLog.canRead() && oldLog.length() == 0L) {
                Files.deleteIfExists(oldLog.toPath());
            }
        }

        String logPathMsg = "Location of old log file was " + this.logPath; //$NON-NLS-1$

        configureLogging();

        // Log the location of the old log file in case anyone ever needs it
        logger.debug(logPathMsg);
    }

    @Override
    public void debug(String message, Object... params) {
        if (!isDebugEnabled())
            return;
        logger.debug(StringUtil.createString(message, params));
    }

    @Override
    public String getName() {
        return logger.getName();
    }

    @Override
    public void debug(Throwable t, String message, Object... params) {
        if (StringUtil.isBlank(message)) {
            return;
        }
        if (!isDebugEnabled())
            return;
        logger.debug(StringUtil.createString(message, params), t);
    }

    @Override
    public void error(I18nResource message, Object... params) {
        if (message == null) {
            return;
        }
        if (!isErrorEnabled())
            return;
        logger.error(message.text(getLoggingLocale(), params));
    }

    @Override
    public void error(Throwable t, I18nResource message, Object... params) {
        if (message == null) {
            return;
        }
        if (!isErrorEnabled())
            return;
        logger.error(message.text(getLoggingLocale(), params), t);

    }

    @Override
    public void info(I18nResource message, Object... params) {
        if (message == null) {
            return;
        }
        if (!isInfoEnabled())
            return;
        logger.info(message.text(getLoggingLocale(), params));
    }

    @Override
    public void info(Throwable t, I18nResource message, Object... params) {
        if (message == null) {
            return;
        }
        if (!isInfoEnabled())
            return;
        logger.info(message.text(getLoggingLocale(), params), t);
    }

    @Override
    public void trace(String message, Object... params) {
        if (StringUtil.isBlank(message)) {
            return;
        }
        if (!isTraceEnabled())
            return;
        logger.trace(StringUtil.createString(message, params));
    }

    @Override
    public void trace(Throwable t, String message, Object... params) {
        if (StringUtil.isBlank(message)) {
            return;
        }
        if (!isTraceEnabled())
            return;
        logger.trace(StringUtil.createString(message, params), t);
    }

    @Override
    public void warn(I18nResource message, Object... params) {
        if (message == null) {
            return;
        }
        if (!isWarnEnabled())
            return;
        logger.warn(message.text(getLoggingLocale(), params));
    }

    @Override
    public void warn(Throwable t, I18nResource message, Object... params) {
        if (message == null) {
            return;
        }
        if (!isWarnEnabled())
            return;
        logger.warn(message.text(getLoggingLocale(), params), t);
    }

    @Override
    public boolean isInfoEnabled() {
        return logger.isEnabledFor(org.apache.log4j.Level.INFO);
    }

    @Override
    public boolean isWarnEnabled() {
        return logger.isEnabledFor(org.apache.log4j.Level.WARN);
    }

    @Override
    public boolean isErrorEnabled() {
        return logger.isEnabledFor(org.apache.log4j.Level.ERROR);
    }

    @Override
    public boolean isDebugEnabled() {
        return logger.isEnabledFor(org.apache.log4j.Level.DEBUG);
    }

    @Override
    public boolean isTraceEnabled() {
        return logger.isEnabledFor(org.apache.log4j.Level.TRACE);
    }
}
