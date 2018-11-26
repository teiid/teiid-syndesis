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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * A template that can be used by GSON to build a JSON document representation.
 */
public final class RestMetadataTemplate extends RestBasicEntity {

    /**
     * Is-Jdbc label
     */
    public static final String IS_JDBC_LABEL = "isJdbc";

    /**
     * Label used to describe entries
     */
    public static final String ENTRIES_LABEL = "entries";

    /**
     * An empty array of templates.
     */
    public static final RestMetadataTemplate[] NO_TEMPLATES = new RestMetadataTemplate[0];

    private List<String> entries = Collections.emptyList();

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestMetadataTemplate() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param template the template
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestMetadataTemplate(URI baseUri, Template template, UnitOfWork uow) throws KException {
        super(baseUri);

        ArgCheck.isNotNull(template, "template"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(template.getName(uow));
        setkType(template.getTypeIdentifier(uow));
        setHasChildren(template.hasChildren(uow));
        setJdbc(template.isJdbc(uow));

        Properties settings = getUriBuilder().createSettings(SettingNames.TEMPLATE_NAME, getId());
        URI parentUri = getUriBuilder().mServerTemplatesUri();
        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);

        List<TemplateEntry> templateEntries = template.getEntries(uow);
        if (templateEntries != null) {
            entries = new ArrayList<String>();
            for (TemplateEntry entry : templateEntries) {
                entries.add(entry.getName(uow));
            }
        }

        addLink(new RestLink(LinkType.SELF, getUriBuilder().templateUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().templateUri(LinkType.PARENT, settings)));
        addLink(new RestLink(LinkType.TEMPLATE_ENTRIES, getUriBuilder().templateUri(LinkType.TEMPLATE_ENTRIES, settings)));
    }

    /**
     * @return jdbc flag
     */
    public boolean isJdbc() {
        Object value = tuples.get(IS_JDBC_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param jdbc
     */
    public void setJdbc(boolean jdbc) {
        tuples.put(IS_JDBC_LABEL, jdbc);
    }

    public List<String> getEntries() {
        return Collections.unmodifiableList(entries);
    }

    public void setEntries(String[] entriesNames) {
        if (entriesNames == null)
            this.entries = Collections.emptyList();
        else
            this.entries = Arrays.asList(entriesNames);
    }

}
