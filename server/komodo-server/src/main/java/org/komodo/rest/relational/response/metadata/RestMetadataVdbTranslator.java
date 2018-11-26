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
import org.komodo.relational.vdb.Translator;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * A translator that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyTranslator",
 *     "description" : "translator description goes here",
 *     "type" : "customType",
 *     "properties" : [
 *         "green" : "lantern",
 *         "captain" : "america",
 *         "black" : "widow"
 *     ]
 * }
 * </code>
 * </pre>
 */
public final class RestMetadataVdbTranslator extends RestVdbTranslator {

    /**
     * An empty array of translators.
     */
    public static final RestMetadataVdbTranslator[] NO_TRANSLATORS = new RestMetadataVdbTranslator[ 0 ];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestMetadataVdbTranslator() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param translator the translator
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestMetadataVdbTranslator(URI baseUri, Translator translator, UnitOfWork uow) throws KException {
        super(baseUri);

        ArgCheck.isNotNull(translator, "translator"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(translator.getName(uow));
        setkType(translator.getTypeIdentifier(uow));
        setHasChildren(translator.hasChildren(uow));
        setDescription(translator.getDescription(uow));
        setType(translator.getType(uow));

        addExecutionProperties(uow, translator);

        Properties settings = getUriBuilder().createSettings(SettingNames.TRANSLATOR_NAME, getId());
        URI parentUri = getUriBuilder().mServerTranslatorsUri();
        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbTranslatorUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbTranslatorUri(LinkType.PARENT, settings)));
    }
}
