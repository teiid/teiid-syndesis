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

import org.komodo.relational.model.Model;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * A VDB model view that can be used by GSON to build a JSON model view representation.
 */
public final class RestVdbModelView extends RestBasicEntity {

    /**
     * Constructor for use when deserializing
     */
    public RestVdbModelView() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param view the view
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbModelView(URI baseUri, View view, UnitOfWork uow) throws KException {
        super(baseUri, view, uow, false);

        Model model = ancestor(view, Model.class, uow);
        ArgCheck.isNotNull(model);
        String modelName = model.getName(uow);

        Vdb vdb = ancestor(model, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.MODEL_NAME, modelName);
        getUriBuilder().addSetting(settings, SettingNames.VIEW_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbModelViewUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbModelViewUri(LinkType.PARENT, settings)));
        createChildLink();
    }
}
