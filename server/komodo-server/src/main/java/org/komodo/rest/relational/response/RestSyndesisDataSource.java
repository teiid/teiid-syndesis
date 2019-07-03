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
package org.komodo.rest.relational.response;

import java.net.URI;
import java.util.Properties;

import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.syndesis.SyndesisLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.SyndesisDataSource;
import org.komodo.utils.ArgCheck;

public class RestSyndesisDataSource extends RestBasicEntity implements SyndesisDataSource {
    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.protectPrefix(SyndesisLexicon.DataService.NAME);

    /**
     * Label used to describe type
     */
    public static final String TYPE_LABEL = KomodoService.protectPrefix(SyndesisLexicon.DataService.TYPE);

    /**
     * Label used to describe bound 
     */
    public static final String BOUND_LABEL = KomodoService.protectPrefix(SyndesisLexicon.DataService.BOUND);

    /**
     * Label used to describe bound
     */
    public static final String TRANSLATOR_LABEL = KomodoService.protectPrefix(SyndesisLexicon.DataService.TRANSLATOR);

    /**
     * Constructor for use when deserializing
     */
    public RestSyndesisDataSource() {
        super();
    }
    
	public RestSyndesisDataSource(URI baseUri, KomodoObject parent, SyndesisDataSource datasource,
			UnitOfWork uow) throws KException {
		super(baseUri, parent, uow);
		
        ArgCheck.isNotNull(datasource, "datasource"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(datasource.getName());
        setkType(KomodoType.SYNDESIS_DATA_SOURCE);
        setHasChildren(false);
        
        setName(datasource.getName());
        setType(datasource.getType());
        setBound(datasource.isBound());
        setTranslatorName(datasource.getTranslatorName());

        Properties settings = getUriBuilder().createSettings(SettingNames.CONNECTION_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().connectionUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().connectionUri(LinkType.PARENT, settings)));
    }

	@Override
	public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
	}

    @Override
    public String getType() {
        Object type = tuples.get(TYPE_LABEL);
        return type != null ? type.toString() : null;
    }

    @Override
    public boolean isBound() {
        Object label = tuples.get(BOUND_LABEL);
        return label != null ? Boolean.parseBoolean(label.toString()) : false;
    }

    public void setName(String name) {
        tuples.put(NAME_LABEL, name);
    }

    public void setType(String type) {
        tuples.put(TYPE_LABEL, type);
    }

    public void setBound(boolean bound) {
        tuples.put(BOUND_LABEL, bound);
    }

    @Override
    public String getTranslatorName() {
        Object name = tuples.get(TRANSLATOR_LABEL);
        return name != null ? name.toString() : null;
    }

    public void setTranslatorName(String name) {
        tuples.put(TRANSLATOR_LABEL, name);
    }
}
