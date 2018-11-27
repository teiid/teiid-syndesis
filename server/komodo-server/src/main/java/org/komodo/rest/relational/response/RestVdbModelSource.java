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
import org.komodo.relational.connection.Connection;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.vdb.VdbLexicon;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * A VDB model source that can be used by GSON to build a JSON model source representation.
 */
public final class RestVdbModelSource extends RestBasicEntity {

    /**
     * Label used to describe jndi name
     */
    public static final String JNDI_NAME_LABEL = KomodoService.protectPrefix(VdbLexicon.Source.JNDI_NAME);

    /**
     * Label used to describe translator
     */
    public static final String TRANSLATOR_LABEL = KomodoService.protectPrefix(VdbLexicon.Source.TRANSLATOR);

    /**
     * Label used to describe the origin connection
     */
    public static final String CONNECTION_LABEL = KomodoService.protectPrefix(VdbLexicon.Source.ORIGIN_CONNECTION);

    /**
     * Constructor for use when deserializing
     */
    public RestVdbModelSource() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param source the source
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbModelSource(URI baseUri, ModelSource source, UnitOfWork uow) throws KException {
        super(baseUri, source, uow, false);

        setJndiName(source.getJndiName(uow));
        String translatorName = source.getTranslatorName(uow);
        setTranslator(translatorName);
        Connection connection = source.getOriginConnection(uow);
        if (connection != null) {
            setConnection(connection.getAbsolutePath());
        }

        Model model = ancestor(source, Model.class, uow);
        ArgCheck.isNotNull(model);
        String modelName = model.getName(uow);

        Vdb vdb = ancestor(model, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.MODEL_NAME, modelName);
        getUriBuilder().addSetting(settings, SettingNames.TRANSLATOR_NAME, translatorName);
        getUriBuilder().addSetting(settings, SettingNames.SOURCE_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbModelSourceUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbModelSourceUri(LinkType.PARENT, settings)));
        createChildLink();

        Translator[] translators = vdb.getTranslators(uow, translatorName);
        if (translators != null && translators.length == 1) {
            getUriBuilder().addSetting(settings, SettingNames.TRANSLATOR_NAME, translatorName);
            addLink(new RestLink(LinkType.REFERENCE, getUriBuilder().vdbModelSourceUri(LinkType.REFERENCE, settings)));
        }
    }

    /**
     * @return the jndiName
     */
    public String getJndiName() {
        Object jndi = tuples.get(JNDI_NAME_LABEL);
        return jndi != null ? jndi.toString() : null;
    }

    /**
     * @param jndiName the jndiName to set
     */
    public void setJndiName(String jndiName) {
        tuples.put(JNDI_NAME_LABEL, jndiName);
    }

    /**
     * @return the translator
     */
    public String getTranslator() {
        Object translator = tuples.get(TRANSLATOR_LABEL);
        return translator != null ? translator.toString() : null;
    }

    /**
     * @param translator the translator to set
     */
    public void setTranslator(String translator) {
        tuples.put(TRANSLATOR_LABEL, translator);
    }

    /**
     * @return the associated connection
     */
    public String getConnection() {
        Object connection = tuples.get(CONNECTION_LABEL);
        return connection != null ? connection.toString() : null;
    }

    /**
     * @param connection the associated connection to set
     */
    public void setConnection(String connection) {
        tuples.put(CONNECTION_LABEL, connection);
    }
}
