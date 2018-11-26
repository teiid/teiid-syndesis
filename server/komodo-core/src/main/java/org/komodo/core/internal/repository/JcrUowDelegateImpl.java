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
