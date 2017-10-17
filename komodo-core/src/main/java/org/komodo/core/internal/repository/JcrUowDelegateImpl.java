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
package org.komodo.core.internal.repository;

import javax.jcr.Session;
import org.komodo.core.repository.Messages;
import org.komodo.spi.KException;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrRepository;

public class JcrUowDelegateImpl implements JcrUowDelegate {

    /**
     * Allows for the generation of a {@link org.modeshape.jcr.api.Session} which can then be
     * further abstracted to a {@link javax.jcr.Session}, restricting the proliferation of specific
     * modeshape-api classes
     *
     * @param identifier
     * @return a new {@link org.modeshape.jcr.api.Session}
     * @throws KException
     */
    static org.modeshape.jcr.api.Session generateSession(WorkspaceIdentifier identifier) throws KException {
        try {
            JcrRepository repository = identifier.getRepository();
            if (repository == null || !RepositoryUtils.isRepositoryRunning(repository)) {
                throw new KException(Messages.getString(Messages.LocalRepository.Repository_Not_Running));
            }

            org.modeshape.jcr.api.Session session = repository.login(null, identifier.getWorkspace());
            KLog.getLogger().debug("ModeShapeUtils.createSession: {0}", session.hashCode()); //$NON-NLS-1$
            return session;
        } catch (Exception ex) {
            throw new KException(ex);
        }
    }

    private final Session session;

    public JcrUowDelegateImpl(WorkspaceIdentifier identifier) throws Exception {
        this.session = generateSession(identifier);
    }

    public JcrUowDelegateImpl(Session session) {
        this.session = session;
    }

    @Override
    public Session getImplementation() {
        return session;
    }

    @Override
    public boolean hasPendingChanges() throws Exception {
        return session.hasPendingChanges();
    }

    @Override
    public boolean isLive() {
        return session.isLive();
    }

    @Override
    public void save() throws Exception {
        session.save();
    }

    @Override
    public void complete() {
        session.logout();
    }

    @Override
    public void refresh(boolean keepChanges) throws Exception {
        session.refresh(keepChanges);
    }

}
