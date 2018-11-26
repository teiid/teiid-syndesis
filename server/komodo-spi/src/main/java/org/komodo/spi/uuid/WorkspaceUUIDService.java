/*************************************************************************************
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
package org.komodo.spi.uuid;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.UUID;

/**
 *
 */
public class WorkspaceUUIDService {

    private static WorkspaceUUIDService instance;

    /**
     * Get the singleton instance
     * 
     * @return the single {@link WorkspaceUUIDService}
     */
    public static WorkspaceUUIDService getInstance() {
        if (instance == null) instance = new WorkspaceUUIDService();

        return instance;
    }

    private File restoreFile;

    private WorkspaceUUIDService() {
    }

    /**
     * Set the restore location
     *
     * @param file location for restoration
     */
    public void setRestoreLocation(File file) {
        restoreFile = file;
    }

    /**
     * Get the Workspace's UUID
     * 
     * @return UUID
     */
    public UUID getUUID() {
        UUID workspaceUuid = null;

            if (restoreFile != null && restoreFile.exists()) {
                BufferedReader reader = null;
                try {
                    reader = new BufferedReader(new FileReader(restoreFile));
                    workspaceUuid = UUID.fromString(reader.readLine());
                } catch (final IOException error) {
                    // ignored
                } finally {
                    try {
                        if (reader != null)
                            reader.close();
                    } catch (final IOException ignored) {
                        // ignored
                    }
                }
            }

            if (workspaceUuid == null)
                workspaceUuid = UUID.randomUUID();

            writeUUID(workspaceUuid);
            

        return workspaceUuid;
    }

    /**
     * @param workspaceUuid
     */
    private void writeUUID(UUID workspaceUuid) {
        if (restoreFile == null)
            return;
        
        FileWriter writer = null;
        try {
            writer = new FileWriter(restoreFile);
            writer.write(workspaceUuid.toString());
        } 
        catch (Exception ex) {
            // ignored
        }
        finally {
            try {
                writer.close();
            } catch (final IOException ignored) {
                // ignored
            }
        }
    }
}
