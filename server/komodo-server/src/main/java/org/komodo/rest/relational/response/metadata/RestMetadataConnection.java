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
