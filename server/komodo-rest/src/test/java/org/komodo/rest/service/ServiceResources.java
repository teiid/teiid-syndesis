/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.rules.ExternalResource;
import org.komodo.spi.repository.ApplicationProperties;
import org.komodo.spi.repository.PersistenceType;
import org.komodo.utils.FileUtils;
import org.komodo.utils.TestKLog;

public class ServiceResources extends ExternalResource {

    private static int refCount = 0;

    private static ServiceResources instance;

    public static ExternalResource getInstance() {
        if (refCount == 0)
            instance = new ServiceResources();

        return instance;
    };

    @Override
    protected void before() throws Throwable {
        try {
            if (refCount > 0)
                return;
    
            initResources();
    
        } finally {
            //
            // Ensures before is only called once when:
            // a) test class is running on its own
            // b) test suite is running and all classes share this instance
            //
            refCount++;
        }
    }

    @Override
    protected void after() {
        refCount--;
    
        if (refCount > 0)
            return; // Still other classes using it so don't tear down yet
    
        destroyResources();
    }

    private Path _kengineDataDir;

    private void initResources() throws IOException {
        System.out.println("XXX - Generating Engine Directory!!");

        _kengineDataDir = TestKLog.createEngineDirectory();     
        //
        // Sets the persistence type to H2 for test purposes
        //
        ApplicationProperties.setRepositoryPersistenceType(PersistenceType.H2.name());
    }

    private void destroyResources() {
        System.out.println("XXX - Tearing down Engine Directory!!");

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