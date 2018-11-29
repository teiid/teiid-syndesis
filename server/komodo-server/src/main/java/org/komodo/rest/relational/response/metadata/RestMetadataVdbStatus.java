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
