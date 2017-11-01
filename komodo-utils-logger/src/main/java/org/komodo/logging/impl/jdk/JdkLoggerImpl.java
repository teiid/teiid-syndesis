/*
 * ModeShape (http://www.modeshape.org)
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
* See the AUTHORS.txt file in the distribution for a full listing of 
* individual contributors.
 *
 * ModeShape is free software. Unless otherwise indicated, all code in ModeShape
 * is licensed to you under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * ModeShape is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA, or see the FSF site: http://www.fsf.org.
 */

package org.komodo.logging.impl.jdk;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Logger;
import org.komodo.logging.i18n.I18nResource;
import org.komodo.logging.util.StringUtil;
import org.komodo.spi.logging.KLogger;

/**
 * Logger that delivers messages to a JDK logger
 * 
 * @since 2.5
 */
final class JdkLoggerImpl extends org.komodo.logging.Logger {

    private final java.util.logging.Logger logger;

    private FileHandler fileHandler;

    public JdkLoggerImpl(String category) {
        logger = Logger.getLogger(category);
    }

    @Override
    public String getName() {
        return logger.getName();
    }

    private java.util.logging.Level convertLoggingLevel(KLogger.Level level) {
        java.util.logging.Level jdkLevel;
        switch (level) {
            case DEBUG:
                jdkLevel = java.util.logging.Level.FINE;
                break;
            case ERROR:
                jdkLevel = java.util.logging.Level.SEVERE;
                break;
            case OFF:
                jdkLevel = java.util.logging.Level.OFF;
                break;
            case TRACE:
                jdkLevel = java.util.logging.Level.FINER;
                break;
            case WARNING:
                jdkLevel = java.util.logging.Level.WARNING;
                break;
            case INFO:
            default:
                jdkLevel = java.util.logging.Level.INFO;
                break;

        }
        return jdkLevel;
    }

    @Override
    protected void configureLogging() throws Exception {        
        // get default log file path if necessary
        if ( ( this.logPath == null ) || this.logPath.isEmpty() ) {
            String tempPath = System.getProperty( "komodo.dataDir" ); //$NON-NLS-1$
            if (tempPath == null) {
                tempPath = System.getProperty("java.io.tmpdir"); //$NON-NLS-1$
            }
            tempPath += File.separator + "komodo.log"; //$NON-NLS-1$
            this.logPath = tempPath;
        }

        // make sure log file exists
        final Path logFilePath = Paths.get( this.logPath );

        if ( !Files.exists( logFilePath ) ) {
            if ( !Files.exists( logFilePath.getParent() ) ) {
                Files.createDirectories( logFilePath.getParent() );
            }

            Files.createFile( logFilePath );
        }

        logger.setLevel(convertLoggingLevel(this.level));

        List<Handler> handlers = Arrays.asList(logger.getHandlers());
        if (fileHandler == null || ! handlers.contains(fileHandler)) {
            FileHandler fileHandler = new FileHandler(this.logPath);
            logger.addHandler(fileHandler);
        }
    }

    @Override
    public void dispose() {
        // TODO Auto-generated method stub

    }

    @Override
    public void setLogPath(String logPath) throws Exception {
        super.setLogPath(logPath);

        if (fileHandler != null) {
            fileHandler.close();
            logger.removeHandler(fileHandler);
            fileHandler = null;
        }

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
        debug(logPathMsg);
    }

    @Override
    public void setLevel(KLogger.Level level) throws Exception {
        super.setLevel(level);
        configureLogging();
    }

    private void log(java.util.logging.Level level, String message, Throwable ex) {
        if (logger.isLoggable(level)) {
            Throwable dummyException = new Throwable();
            StackTraceElement locations[] = dummyException.getStackTrace();
            String className = "unknown";
            String methodName = "unknown";
            int depth = 2;
            if (locations != null && locations.length > depth) {
                StackTraceElement caller = locations[depth];
                className = caller.getClassName();
                methodName = caller.getMethodName();
            }
            if (ex == null) {
                logger.logp(level, className, methodName, message);
            } else {
                logger.logp(level, className, methodName, message, ex);
            }
        }
    }

    @Override
    public boolean isTraceEnabled() {
        return logger.isLoggable(java.util.logging.Level.FINER);
    }

    @Override
    public boolean isDebugEnabled() {
        return logger.isLoggable(java.util.logging.Level.FINE);
    }

    @Override
    public boolean isInfoEnabled() {
        return logger.isLoggable(java.util.logging.Level.INFO);
    }

    @Override
    public boolean isWarnEnabled() {
        return logger.isLoggable(java.util.logging.Level.WARNING);
    }

    @Override
    public boolean isErrorEnabled() {
        return logger.isLoggable(java.util.logging.Level.SEVERE);
    }

    @Override
    public void debug(String message, Object... params) {
        log(java.util.logging.Level.FINE, StringUtil.createString(message, params), null);
    }

    @Override
    public void debug(Throwable t, String message, Object... params) {
        log(java.util.logging.Level.FINE, StringUtil.createString(message, params), t);
    }

    @Override
    public void error(I18nResource message, Object... params) {
        log(java.util.logging.Level.SEVERE, message.text(getLoggingLocale(), params), null);
    }

    @Override
    public void error(Throwable t, I18nResource message, Object... params) {
        log(java.util.logging.Level.SEVERE, message.text(getLoggingLocale(), params), t);
    }

    @Override
    public void info(I18nResource message, Object... params) {
        log(java.util.logging.Level.INFO, message.text(getLoggingLocale(), params), null);
    }

    @Override
    public void info(Throwable t, I18nResource message, Object... params) {
        log(java.util.logging.Level.INFO, message.text(getLoggingLocale(), params), t);
    }

    @Override
    public void trace(String message, Object... params) {
        log(java.util.logging.Level.FINER, StringUtil.createString(message, params), null);
    }

    @Override
    public void trace(Throwable t, String message, Object... params) {
        log(java.util.logging.Level.FINER, StringUtil.createString(message, params), t);

    }

    @Override
    public void warn(I18nResource message, Object... params) {
        log(java.util.logging.Level.WARNING, message.text(getLoggingLocale(), params), null);
    }

    @Override
    public void warn(Throwable t, I18nResource message, Object... params) {
        log(java.util.logging.Level.WARNING, message.text(getLoggingLocale(), params), t);

    }
}
