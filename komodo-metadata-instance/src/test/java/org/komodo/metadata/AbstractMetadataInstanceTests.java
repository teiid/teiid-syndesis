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
package org.komodo.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.BeforeClass;
import org.komodo.spi.KClient;
import org.komodo.spi.KEvent;
import org.komodo.spi.metadata.MetadataClientEvent;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.metadata.MetadataInstance.Condition;

public class AbstractMetadataInstanceTests {

    protected static final MetadataInstance METADATA_INSTANCE = DefaultMetadataInstance.getInstance();

    @BeforeClass
    public static void init() {
        // Initialise the metadata instance
        KClient kClient = new KClient() {
    
            @Override
            public void eventOccurred(KEvent<?> event) {
                // Do Nothing
            }
    
            @Override
            public void errorOccurred(Throwable e) {
                fail("Exception occurred while starting metadata instance in test class: " + TestDataTypeManager.class);
            }
    
            @Override
            public org.komodo.spi.KClient.State getState() {
                return State.SHUTDOWN;
            }
        };
    
        MetadataClientEvent metadataClientEvent = MetadataClientEvent.createStartedEvent(kClient);
        METADATA_INSTANCE.notify(metadataClientEvent);
        assertEquals(Condition.REACHABLE, METADATA_INSTANCE.getCondition());
    }

    protected MetadataInstance getMetadataInstance() {
        return METADATA_INSTANCE;
    }

}
