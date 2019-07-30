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
package org.komodo.rest.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.rules.ExternalResource;
import org.komodo.utils.FileUtils;
import org.komodo.utils.TestKLog;

public class ServiceResources extends ExternalResource {

    private static AtomicInteger refCount = new AtomicInteger(0);

    private static ServiceResources instance;

    public static ExternalResource getInstance() {
        if (refCount.get() == 0)
            instance = new ServiceResources();

        return instance;
    };

    @Override
    protected void before() throws Throwable {
        try {
            if (refCount.get() > 0)
                return;
    
            initResources();
    
        } finally {
            //
            // Ensures before is only called once when:
            // a) test class is running on its own
            // b) test suite is running and all classes share this instance
            //
            refCount.getAndIncrement();
        }
    }

    @Override
    protected void after() {
        refCount.getAndDecrement();
    
        if (refCount.get() > 0)
            return; // Still other classes using it so don't tear down yet
    
        destroyResources();
    }

    private Path _kengineDataDir;

    private void initResources() throws IOException {
        _kengineDataDir = TestKLog.createEngineDirectory();     
    }

    private void destroyResources() {
        if (_kengineDataDir != null) {
            FileUtils.removeDirectoryAndChildren(_kengineDataDir.toFile());
        }

        try {
            Files.deleteIfExists(_kengineDataDir);
        } catch (Exception ex) {
            _kengineDataDir.toFile().deleteOnExit();
        }
    }
}