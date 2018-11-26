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

import org.komodo.logging.impl.jdk.JdkLoggerFactory;
import org.komodo.logging.log4j.Log4jLoggerFactory;

/**
 * Taken from Modeshape repository at https://github.com/Modeshape/modeshape.git
 *
 * The abstract class for the LogFactory, which is called to create a specific implementation of the {@link Logger}.
 * <p>
 * Provides out of the box several LogFactory implementations that work with common log frameworks:
 * <ol></li>
 * <li>Log4J</li>
 * <li>JDK Util Logging</li>
 * </ol>
 * The static initializer for this class checks the classpath for the availability of these frameworks, and as soon as one is
 * found the LogFactory implementation for that framework is instantiated and used for all ModeShape logging.
 * </p>
 * <p>
 * However, since ModeShape can be embedded into any application, it is possible that applications use a logging framework other
 * than those listed above. So before falling back to the JDK logging, ModeShape looks for the
 * <code>org.modeshape.common.logging.CustomLoggerFactory</code> class, and if found attempts to instantiate and use it. But
 * ModeShape does not provide this class out of the box; rather an application that is embedding ModeShape can provide its own
 * version of that class that should extend {@link LogFactory} and create an appropriate implementation of {@link Logger} that
 * forwards ModeShape log messages to the application's logging framework.
 * </p>
 */
public abstract class LogFactory {

    /**
     * The name of the {@link LogFactory} implementation that is not provided out of the box but can be created, implemented, and
     * placed on the classpath to have ModeShape send log messages to a custom framework.
     */
    public static final String CUSTOM_LOG_FACTORY_CLASSNAME = "org.modeshape.common.logging.CustomLoggerFactory";

    private static LogFactory LOGFACTORY;

    static {
        boolean log4jLogging = false;

        if (LOGFACTORY == null) {
            if (isLog4jAvailable()) {
                LOGFACTORY = new Log4jLoggerFactory();
                log4jLogging = true;
            } else {
                LOGFACTORY = new JdkLoggerFactory();
            }
        }

        Logger logger = LOGFACTORY.getLogger(LogFactory.class.getName());

        if (log4jLogging) {
            logger.debug("Log4j implementation located in the classpath - preferred for logging");
        } else {
            logger.debug("No SLF4J implementation located in the classpath - falling back to JDK logging");
        }
    }

    private static boolean isLog4jAvailable() {
        try {
            // Check if the Log4J main interface is in the classpath and initialize the class
            Class.forName("org.apache.log4j.Logger");
            return true;
        } catch (ClassNotFoundException e) {
            return false;
        }
    }

    static LogFactory getLogFactory() {
        return LOGFACTORY;
    }

    /**
     * Return a logger named corresponding to the class passed as parameter.
     * 
     * @param clazz the returned logger will be named after clazz
     * @return logger
     */
    Logger getLogger( Class<?> clazz ) {
        Logger logger = Logger.getLogger(clazz.getName());

        try {
            logger.configureLogging();
        } catch (Exception e) {
            e.printStackTrace();
        }

        return logger;
    }

    /**
     * Return a logger named according to the name parameter.
     * 
     * @param name The name of the logger.
     * @return logger
     */
    protected abstract Logger getLogger( String name );

}
