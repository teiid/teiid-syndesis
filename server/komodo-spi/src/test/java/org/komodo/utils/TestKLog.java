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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileAttribute;
import java.util.concurrent.ThreadLocalRandom;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.spi.SystemConstants;

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

    @Test
    public void testLogInit() {
        try {
            logger = KLog.getLogger();
            assertNotNull(logger);
        } catch (Throwable throwable) {
            fail("Should not throw an exception " + throwable.getLocalizedMessage());
        }
    }
}
