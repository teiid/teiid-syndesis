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
import org.komodo.relational.connection.Connection;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * A connection that can be used by GSON to build a JSON document representation.
 */
public final class RestMetadataConnection extends RestConnection {

    /**
     * An empty array of connections.
     */
    public static final RestMetadataConnection[] NO_CONNECTIONS = new RestMetadataConnection[0];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestMetadataConnection() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param connection the connection
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestMetadataConnection(URI baseUri, Connection connection, UnitOfWork uow) throws KException {
        super(baseUri);

        ArgCheck.isNotNull(connection, "connection"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(connection.getName(uow));
        setkType(connection.getTypeIdentifier(uow));
        setHasChildren(connection.hasChildren(uow));
        setJndiName(connection.getJndiName(uow));
        setDriverName(connection.getDriverName(uow));
        setJdbc(connection.isJdbc(uow));

        addExecutionProperties(uow, connection);

        Properties settings = getUriBuilder().createSettings(SettingNames.CONNECTION_NAME, getId());
        URI parentUri = getUriBuilder().mServerConnectionsUri();
        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().connectionUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().connectionUri(LinkType.PARENT, settings)));
    }

}
