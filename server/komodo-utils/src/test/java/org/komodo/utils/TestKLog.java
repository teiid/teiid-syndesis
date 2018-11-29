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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileAttribute;
import java.util.concurrent.ThreadLocalRandom;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.logging.KLogger;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestKLog {

    private static Path _dataDirectory;

    @BeforeClass
    public static void initDataDirectory() throws Exception {
        // create data directory for engine logging
        _dataDirectory = createEngineDirectory();	
    	File f = new File (_dataDirectory.toAbsolutePath().toString());
    	if (f.exists()) {
    		f.delete();
    	} else {
    		Files.createDirectory(_dataDirectory, new FileAttribute[0]);    		
    	}
        System.setProperty(SystemConstants.ENGINE_DATA_DIR,  _dataDirectory.toAbsolutePath().toString());     	
    }

	public static Path createEngineDirectory() throws IOException {
		Path engineDir = Paths.get("target/KomodoEngineDataDir"+ThreadLocalRandom.current().nextInt());    	
		Files.createDirectory(engineDir, new FileAttribute[0]);    		
        System.setProperty(SystemConstants.ENGINE_DATA_DIR,  engineDir.toAbsolutePath().toString());
        return engineDir;
	}    
    
    @AfterClass
    public static void removeDataDirectory() throws Exception {
        FileUtils.removeDirectoryAndChildren( _dataDirectory.toFile() );
    }

    private KLog logger;

    @Before
    public void setup() {
        logger = KLog.getLogger();
    }

    @After
    public void cleanup() {
        logger.dispose();
    }

    @Test
    public void testLogInit() {
        try {
            logger = KLog.getLogger();
            assertNotNull(logger);
        } catch (Throwable throwable) {
            fail("Should not throw an exception " + throwable.getLocalizedMessage());
        }
    }

    private File configureLogPath(KLog logger) throws IOException, Exception {
        File newLogFile = File.createTempFile("TestKLog", ".log");
        newLogFile.deleteOnExit();

        logger.setLogPath(newLogFile.getAbsolutePath());
        assertEquals(newLogFile.getAbsolutePath(), logger.getLogPath());
        return newLogFile;
    }

    private String retrieveLogContents(File newLogFile) throws Exception {
        try ( BufferedReader reader = new BufferedReader( new FileReader( newLogFile ) ) ) {
            StringBuilder builder = new StringBuilder();
            String line;
            while((line = reader.readLine()) != null) {
                builder.append(line);
                builder.append("\n");
            }

            return builder.toString();
        }
    }

    @Test
    public void testSetLogPath() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String logMsg = "Test Log Message";
        logger.info(logMsg);

        String fileMsg = retrieveLogContents(newLogFile);
        assertTrue(fileMsg.contains(logMsg));
    }

    @Test
    public void testLogInfo() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String msg = "This is a test";
        logger.info(msg);

        String fileMsg = retrieveLogContents(newLogFile);
//        System.out.println("The File Message: " + fileMsg);
        assertTrue(fileMsg.contains("INFO  " + KLogger.class.getName() + "  - " + msg));
    }

    @Test
    public void testLogWarning() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String msg = "This is a {0} test";
        String param1 = "warning";

        logger.warn(msg, param1);

        String fileMsg = retrieveLogContents(newLogFile);
//        System.out.println("The File Message: " + fileMsg);
        String testMsg = "WARN  " + KLogger.class.getName() + "  - " + msg.replace("{0}", param1);
        assertTrue(fileMsg.contains(testMsg));
    }

    @Test
    public void testLogError() throws Exception {
        assertNotNull(logger);

        File newLogFile = configureLogPath(logger);

        String msg = "This is a exception test";
        Exception testException = new Exception("This is a test exception");
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        testException.printStackTrace(pw);

        logger.error(msg, testException);

        String fileMsg = retrieveLogContents(newLogFile);
//        System.out.println("The File Message: " + fileMsg);
        assertTrue(fileMsg.contains("ERROR " + KLogger.class.getName() + "  - " + msg));
        assertTrue(fileMsg.contains(msg));
    }

}
