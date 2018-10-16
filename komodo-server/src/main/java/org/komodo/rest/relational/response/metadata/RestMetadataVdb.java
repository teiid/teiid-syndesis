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
package org.komodo.rest.relational.response.metadata;

import java.net.URI;
import java.util.Properties;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A VDB that can be used by GSON to build a JSON document representation.
 */
public final class RestMetadataVdb extends RestVdb {

    /**
     * Constructor for use when deserializing
     */
    public RestMetadataVdb() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param vdb the vdb
     * @param uow the transaction
     * @param exportXml true to export the xml
     *
     * @throws KException if error occurs
     */
    public RestMetadataVdb(URI baseUri, Vdb vdb, UnitOfWork uow, boolean exportXml) throws KException {
        super(baseUri);

        setId(vdb.getName(uow));
        setkType(vdb.getTypeIdentifier(uow));
        setHasChildren(vdb.hasChildren(uow));
        setName(vdb.getName(uow));
        setDescription(vdb.getDescription(uow));

        setPreview(vdb.isPreview(uow));
        setConnectionType(vdb.getConnectionType(uow));
        setVersion(vdb.getVersion(uow));

        addExecutionProperties(uow, vdb);

        if (exportXml) {
            byte[] xmlBytes = vdb.export(uow, new Properties());
            setXml(new String(xmlBytes));
        }

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, getId());
        URI parentUri = getUriBuilder().mServerVdbsUri();
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, parentUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbUri(LinkType.PARENT, settings)));
        addLink(new RestLink(LinkType.IMPORTS, getUriBuilder().vdbUri(LinkType.IMPORTS, settings)));
        addLink(new RestLink(LinkType.MODELS, getUriBuilder().vdbUri(LinkType.MODELS, settings)));
        addLink(new RestLink(LinkType.TRANSLATORS, getUriBuilder().vdbUri(LinkType.TRANSLATORS, settings)));
        addLink(new RestLink(LinkType.DATA_ROLES, getUriBuilder().vdbUri(LinkType.DATA_ROLES, settings)));
    }
}
