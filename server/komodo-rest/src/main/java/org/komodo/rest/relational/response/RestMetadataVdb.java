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
import javax.ws.rs.core.MediaType;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A VDB that can be used by GSON to build a JSON document representation.
 */
public final class RestMetadataVdb extends RestBasicEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.protectPrefix(VdbLexicon.Vdb.NAME);

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.protectPrefix(VdbLexicon.Vdb.DESCRIPTION);

    /**
     * Label used to describe original file path
     */
    public static final String PREVIEW_LABEL = KomodoService.protectPrefix(VdbLexicon.Vdb.PREVIEW);

    /**
     * Label used to describe original file path
     */
    public static final String CONNECTION_TYPE_LABEL = KomodoService.protectPrefix(VdbLexicon.Vdb.CONNECTION_TYPE);

    /**
     * Label used to describe original file path
     */
    public static final String VERSION_LABEL = KomodoService.protectPrefix(VdbLexicon.Vdb.VERSION);

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

    @Override
    public boolean supports(MediaType mediaType) {
        if (MediaType.APPLICATION_JSON_TYPE.equals(mediaType))
            return true;

        return MediaType.APPLICATION_XML_TYPE.equals(mediaType) && getXml() != null;
    }

    /**
     * @return the name
     */
    public String getName() {
        Object name = tuples.get(NAME_LABEL);
        return name != null ? name.toString() : null;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        tuples.put(NAME_LABEL, name);
    }

    /**
     * @return the VDB description (can be empty)
     */
    public String getDescription() {
        Object description = tuples.get(DESCRIPTION_LABEL);
        return description != null ? description.toString() : null;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        tuples.put(DESCRIPTION_LABEL, description);
    }

    /**
     * @return the preview
     */
    public boolean isPreview() {
        Object preview = tuples.get(PREVIEW_LABEL);
        return preview != null ? Boolean.parseBoolean(preview.toString()) : false;
    }

    /**
     * @param preview the preview to set
     */
    public void setPreview(boolean preview) {
        tuples.put(PREVIEW_LABEL, preview);
    }

    /**
     * @return the connectionType
     */
    public String getConnectionType() {
        Object connectionType = tuples.get(CONNECTION_TYPE_LABEL);
        return connectionType != null ? connectionType.toString() : null;
    }

    /**
     * @param connectionType the connectionType to set
     */
    public void setConnectionType(String connectionType) {
        tuples.put(CONNECTION_TYPE_LABEL, connectionType);
    }

    /**
     * @return the version
     */
    public int getVersion() {
        Object version = tuples.get(VERSION_LABEL);
        return version != null ? Integer.parseInt(version.toString()) : 1;
    }

    /**
     * @param version the version to set
     */
    public void setVersion(int version) {
        tuples.put(VERSION_LABEL, version);
    }
}
