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

import java.util.HashSet;
import java.util.Set;
import org.komodo.spi.KEvent;
import org.komodo.spi.KObserver;
import org.komodo.spi.metadata.MetadataClientEvent;
import org.komodo.spi.metadata.MetadataObserver;
import org.komodo.spi.metadata.MetadataServer;
import org.komodo.utils.ArgCheck;
import org.teiid.runtime.EmbeddedConfiguration;
import org.teiid.runtime.EmbeddedServer;

public class DefaultMetadataServer implements MetadataServer {

    private EmbeddedConfiguration serverConfig;

    private EmbeddedServer server;

    private final Set<MetadataObserver> observers = new HashSet<>();

    private Condition condition = Condition.NOT_REACHABLE;

    @Override
    public Condition getCondition() {
        return condition;
    }

    private void configureServer() {
        if (serverConfig != null)
            return;

        serverConfig = new EmbeddedConfiguration();
        serverConfig.setUseDisk(false);
    }

    private void startServer() {
        if (this.condition == Condition.REACHABLE)
            return;

        configureServer();

        try {
            server = new EmbeddedServer();
            server.start(serverConfig);
        } catch (Throwable ex) {
            errorObservers(ex);
            return;
        }

        this.condition = Condition.REACHABLE;

        KEvent<MetadataServer> event = new KEvent<MetadataServer>(this, KEvent.Type.METADATA_SERVER_STARTED);
        notifyObservers(event);
    }

    private void stopServer() {
        if (this.condition == Condition.NOT_REACHABLE)
            return;

        try {
            server.stop();
        } catch (Throwable ex) {
            errorObservers(ex);
        } finally {
            this.condition = Condition.NOT_REACHABLE;

            KEvent<MetadataServer> event = new KEvent<MetadataServer>(this, KEvent.Type.METADATA_SERVER_STOPPED);
            notifyObservers(event);
        }
    }

    @Override
    public void notify( MetadataClientEvent event ) {
        if (event.getType() == MetadataClientEvent.EventType.STARTED) {
            // Start the metadata server if not already started
            startServer();
        } else if (event.getType() == MetadataClientEvent.EventType.SHUTTING_DOWN) {
            stopServer();
        }
    }

    protected void notifyObservers(KEvent<?> event) {
        final Set<MetadataObserver> copy = new HashSet<>(this.observers);

        for (final MetadataObserver observer : copy) {
            try {
                // Ensure all observers are informed even if one throws an exception
                observer.eventOccurred(event);
            } catch (final Exception ex) {
                observer.errorOccurred(ex);
            }
        }
    }

    protected void errorObservers(Throwable ex) {
        ArgCheck.isNotNull(ex);

        final Set<KObserver> copy = new HashSet<>(this.observers);

        for (final KObserver observer : copy) {
            observer.errorOccurred(ex);
        }
    }

    @Override
    public void addObserver(MetadataObserver observer) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.add(observer);
    }

    @Override
    public void removeObserver(MetadataObserver observer) {
        ArgCheck.isNotNull(observer, "observer"); //$NON-NLS-1$
        this.observers.remove(observer);
    }

}
