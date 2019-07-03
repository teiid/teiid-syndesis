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
package org.komodo.relational.template.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.spi.KException;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.utils.ArgCheck;

/**
 * Implementation of template instance model
 */
public class TemplateImpl extends RelationalObjectImpl implements Template, EventManager {

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public TemplateImpl( final UnitOfWork uow,
                      final Repository repository,
                      final String path ) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Template.IDENTIFIER;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return value of teiid id property (never empty)
     * @throws KException
     *         if error occurs
     */
    @Override
    public String getId( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final Property prop = getObjectFactory().getId( transaction, this );
        final String result = prop.getStringValue( transaction );
        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    @Override
    public boolean isJdbc(UnitOfWork uow) throws KException {
        List<TemplateEntry> entries = getEntries(uow, Template.CONN_FACTORY_CLASS_KEY);
        return entries.isEmpty();
    }

    @Override
    public TemplateEntry addEntry( final UnitOfWork transaction, final String entryName ) throws KException {
        return RelationalModelFactory.createTemplateEntry( transaction, getRepository(), this, entryName );
    }

    @Override
    public List<TemplateEntry> getEntries(final UnitOfWork transaction, String... namePatterns) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        final List<TemplateEntry> result = new ArrayList<TemplateEntry>();
        for (final KomodoObject kobject : getChildrenOfType(transaction, DataVirtLexicon.TemplateEntry.NODE_TYPE, namePatterns)) {
            TemplateEntry entry = new TemplateEntryImpl(transaction, getRepository(), kobject.getAbsolutePath());
            result.add(entry);
        }

        if (result.isEmpty()) {
            return Collections.emptyList();
        }

        return result;
    }

    @Override
    public boolean addListener(ExecutionConfigurationListener listener) {
        return false;
    }

    @Override
    public void permitListeners(boolean enable) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public void notifyListeners(ExecutionConfigurationEvent event) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public boolean removeListener(ExecutionConfigurationListener listener) {
        return false;
    }
}
