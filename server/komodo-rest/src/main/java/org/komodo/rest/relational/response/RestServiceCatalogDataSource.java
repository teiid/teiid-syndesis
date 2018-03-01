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
package org.komodo.rest.relational.response;

import java.net.URI;
import java.util.Properties;

import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.servicecatalog.ServiceCatalogLexicon;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.ServiceCatalogDataSource;
import org.komodo.utils.ArgCheck;

public class RestServiceCatalogDataSource extends RestBasicEntity implements ServiceCatalogDataSource {
    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.protectPrefix(ServiceCatalogLexicon.DataService.NAME);

    /**
     * Label used to describe type
     */
    public static final String TYPE_LABEL = KomodoService.protectPrefix(ServiceCatalogLexicon.DataService.TYPE);

    /**
     * Label used to describe bound 
     */
    public static final String BOUND_LABEL = KomodoService.protectPrefix(ServiceCatalogLexicon.DataService.BOUND);

    /**
     * Label used to describe bound
     */
    public static final String TRANSLATOR_LABEL = KomodoService.protectPrefix(ServiceCatalogLexicon.DataService.TRANSLATOR);

    /**
     * Constructor for use when deserializing
     */
    public RestServiceCatalogDataSource() {
        super();
    }
    
	public RestServiceCatalogDataSource(URI baseUri, KomodoObject parent, ServiceCatalogDataSource datasource,
			UnitOfWork uow) throws KException {
		super(baseUri, parent, uow);
		
        ArgCheck.isNotNull(datasource, "datasource"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(datasource.getName());
        setkType(KomodoType.SERVICE_CATALOG_DATA_SOURCE);
        setHasChildren(false);
        
        setName(datasource.getName());
        setType(datasource.getType());
        setBound(datasource.isBound());
        setTranslatorName(datasource.getTranslatorName());

        Properties settings = getUriBuilder().createSettings(SettingNames.CONNECTION_NAME, getId());
        URI parentUri = getUriBuilder().mServerConnectionsUri();
        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);

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
