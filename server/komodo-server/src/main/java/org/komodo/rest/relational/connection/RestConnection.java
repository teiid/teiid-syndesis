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
package org.komodo.rest.relational.connection;

import java.net.URI;
import java.util.Properties;

import org.komodo.relational.connection.Connection;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A connection that can be used by GSON to build a JSON document representation.
 */
public class RestConnection extends RestBasicEntity {

    /**
     * Label used to describe jndi name
     */
    public static final String JNDI_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.JNDI_NAME);

    /**
     * Label used to describe driver name
     */
    public static final String DRIVER_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.DRIVER_NAME);

    /**
     * Label used to describe jdbc
     */
    public static final String JDBC_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.TYPE);

    /**
     * An empty array of connections.
     */
    public static final RestConnection[] NO_CONNECTIONS = new RestConnection[0];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestConnection() {
        // nothing to do
    }

    /**
     * Constructor for those connections needing more control over what basic properties
     * should be set
     *
     * @param baseUri
     * @throws KException
     */
    public RestConnection(URI baseUri) throws KException {
        super(baseUri);
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param connection the connection
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestConnection(URI baseUri, Connection connection, UnitOfWork uow) throws KException {
        super(baseUri, connection, uow, false);

        setJndiName(connection.getJndiName(uow));
        setDriverName(connection.getDriverName(uow));

        addExecutionProperties(uow, connection);

        Properties settings = getUriBuilder().createSettings(SettingNames.CONNECTION_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().connectionUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().connectionUri(LinkType.PARENT, settings)));
        createChildLink();
    }

    /**
     * @return the jndi name (can be empty)
     */
    public String getJndiName() {
        Object jndiName = tuples.get(JNDI_NAME_LABEL);
        return jndiName != null ? jndiName.toString() : null;
    }

    /**
     * @param jndiName
     *        the new jndi name (can be empty)
     */
    public void setJndiName(final String jndiName) {
        tuples.put(JNDI_NAME_LABEL, jndiName);
    }

    /**
     * @return the driver name (can be empty)
     */
    public String getDriverName() {
        Object driver = tuples.get(DRIVER_NAME_LABEL);
        return driver != null ? driver.toString() : null;
    }

    /**
     * @param driver
     *        the new driver
     */
    public void setDriverName(final String driver) {
        tuples.put(DRIVER_NAME_LABEL, driver);
    }

}
