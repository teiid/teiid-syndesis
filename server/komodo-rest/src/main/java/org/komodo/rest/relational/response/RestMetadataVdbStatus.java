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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.komodo.rest.AbstractKEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * A Teiid Vdb Status object that can be used by GSON to build a JSON document representation.
 */
public class RestMetadataVdbStatus extends AbstractKEntity {

    /**
     * Label for the vdbs collection
     */
    public static final String VDBS_LABEL = "vdbs";

    private List<RestMetadataVdbStatusVdb> vdbProps = new ArrayList<>();

    /**
     * Constructor for use when deserializing
     */
    public RestMetadataVdbStatus() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param mServer the metadata instance
     *
     * @throws KException if error occurs
     */
    public RestMetadataVdbStatus(URI baseUri, MetadataInstance mServer) throws KException {
        super(baseUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().mServerVdbStatusUri()));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().mServerStatusUri()));

        if (mServer == null)
            return;

        Collection<TeiidVdb> vdbs;
        try {
            vdbs = mServer.getVdbs();
            for (TeiidVdb vdb :  vdbs) {
                RestMetadataVdbStatusVdb props = new RestMetadataVdbStatusVdb(vdb);
                vdbProps .add(props);
            }
        } catch (Exception e) {
            throw new KException(e);
        }
    }

    public List<RestMetadataVdbStatusVdb> getVdbProperties() {
        return vdbProps;
    }

    public void setVdbProperties(List<RestMetadataVdbStatusVdb> props) {
        if (props == null)
            props = new ArrayList<>();

        this.vdbProps = props;
    }
}
