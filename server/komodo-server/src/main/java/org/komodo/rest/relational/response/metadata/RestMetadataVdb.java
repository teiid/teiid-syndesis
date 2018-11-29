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
