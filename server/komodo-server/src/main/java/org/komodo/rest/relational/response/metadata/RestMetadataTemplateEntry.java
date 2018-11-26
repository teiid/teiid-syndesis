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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
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

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * A templateEntry that can be used by GSON to build a JSON document representation.
 */
public final class RestMetadataTemplateEntry extends RestBasicEntity {

    /**
     * Required label
     */
    public static final String REQUIRED_LABEL = "required";

    /**
     * Modifiable label
     */
    public static final String MODIFIABLE_LABEL = "modifiable";

    /**
     * Masked label
     */
    public static final String MASKED_LABEL = "masked";

    /**
     * Constrained to allowed values label
     */
    public static final String CONSTRAINED_ALLOWED_VALUES_LABEL = "constrainedToAllowedValues";

    /**
     * Advanced label
     */
    public static final String ADVANCED_LABEL = "advanced";

    /**
     * Type class name label
     */
    public static final String TYPE_CLASS_NAME_LABEL = "typeClassName";

    /**
     * Display label
     */
    public static final String DISPLAY_NAME_LABEL = "displayName";

    /**
     * Description label
     */
    public static final String DESCRIPTION_LABEL = "description";

    /**
     * Default value label
     */
    public static final String DEFAULT_VALUE_LABEL = "defaultValue";

    /**
     * Category label
     */
    public static final String CATEGORY_LABEL = "category";

    /**
     * Allowed values label
     */
    public static final String ALLOWED_VALUES_LABEL = "allowedValues";

    /**
     * Custom properties label
     */
    public static final String CUSTOM_PROPERTIES_LABEL = "customProperties";

    /**
     * An empty array of templateEntrys.
     */
    public static final RestMetadataTemplateEntry[] NO_ENTRIES = new RestMetadataTemplateEntry[ 0 ];

    @JsonProperty(CUSTOM_PROPERTIES_LABEL)
    private Map<String, String> customProperties;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestMetadataTemplateEntry() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param templateEntry the templateEntry
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestMetadataTemplateEntry(URI baseUri, TemplateEntry templateEntry, UnitOfWork uow) throws KException {
        super(baseUri);

        ArgCheck.isNotNull(templateEntry, "templateEntry"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(templateEntry.getName(uow));
        setkType(templateEntry.getTypeIdentifier(uow));
        setHasChildren(templateEntry.hasChildren(uow));

        this.setAllowedValues(templateEntry.getAllowedValues(uow));
        this.setCategory(templateEntry.getCategory(uow));
        this.setCustomProperties(templateEntry.getCustomProperties(uow));
        this.setDefaultValue(templateEntry.getDefaultValue(uow));
        this.setDescription(templateEntry.getDescription(uow));
        this.setDisplayName(templateEntry.getDisplayName(uow));
        this.setTypeClassName(templateEntry.getTypeClassName(uow));
        this.setAdvanced(templateEntry.isAdvanced(uow));
        this.setConstrainedToAllowedValues(templateEntry.isConstrainedToAllowedValues(uow));
        this.setMasked(templateEntry.isMasked(uow));
        this.setModifiable(templateEntry.isModifiable(uow));
        this.setRequired(templateEntry.isRequired(uow));

        Template template = ancestor(templateEntry, Template.class, uow);
        ArgCheck.isNotNull(template);
        String templateName = template.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.TEMPLATE_NAME, templateName);
        getUriBuilder().addSetting(settings, SettingNames.TEMPLATE_NAME, templateName);
        getUriBuilder().addSetting(settings, SettingNames.TEMPLATE_ENTRY_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().templateEntryUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().templateEntryUri(LinkType.PARENT, settings)));
    }

    /**
     * @return required flag
     */
    public boolean isRequired() {
        Object value = tuples.get(REQUIRED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param required
     */
    public void setRequired(boolean required) {
        tuples.put(REQUIRED_LABEL, required);
    }

    /**
     * @return modifiable flag
     */
    public boolean isModifiable() {
        Object value = tuples.get(MODIFIABLE_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param modifiable
     */
    public void setModifiable(boolean modifiable) {
        tuples.put(MODIFIABLE_LABEL, modifiable);
    }

    /**
     * @return masked flag
     */
    public boolean isMasked() {
        Object value = tuples.get(MASKED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param masked
     */
    public void setMasked(boolean masked) {
        tuples.put(MASKED_LABEL, masked);
    }

    /**
     * @return is constrained to allowed values flag
     */
    public boolean isConstrainedToAllowedValues() {
        Object value = tuples.get(CONSTRAINED_ALLOWED_VALUES_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param constrainedToAllowedValues
     */
    public void setConstrainedToAllowedValues(boolean constrainedToAllowedValues) {
        tuples.put(CONSTRAINED_ALLOWED_VALUES_LABEL, constrainedToAllowedValues);
    }

    /**
     * @return advanced flag
     */
    public boolean isAdvanced() {
        Object value = tuples.get(ADVANCED_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param advanced
     */
    public void setAdvanced(boolean advanced) {
        tuples.put(ADVANCED_LABEL, advanced);
    }

    /**
     * @return the type class name
     */
    public String getTypeClassName() {
        Object value = tuples.get(TYPE_CLASS_NAME_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param typeClassName
     */
    public void setTypeClassName(String typeClassName) {
        tuples.put(TYPE_CLASS_NAME_LABEL, typeClassName);
    }

    /**
     * @return the display name
     */
    public String getDisplayName() {
        Object value = tuples.get(DISPLAY_NAME_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param displayName
     */
    public void setDisplayName(String displayName) {
        tuples.put(DISPLAY_NAME_LABEL, displayName);
    }

    /**
     * @return the description
     */
    public String getDescription() {
        Object value = tuples.get(DESCRIPTION_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param description
     */
    public void setDescription(String description) {
        tuples.put(DESCRIPTION_LABEL, description);
    }

    /**
     * @return the default value
     */
    public Object getDefaultValue() {
        Object value = tuples.get(DEFAULT_VALUE_LABEL);
        return value != null ? value : null;
    }

    /**
     * @param defaultValue
     */
    public void setDefaultValue(Object defaultValue) {
        tuples.put(DEFAULT_VALUE_LABEL, defaultValue);
    }

    /**
     * @return the customProperties
     */
    public Map<String, String> getCustomProperties() {
        if (customProperties == null)
            return Collections.emptyMap();

        return Collections.unmodifiableMap(this.customProperties);
    }

    public void setCustomProperties(Properties customProperties) {
        if (this.customProperties == null)
            this.customProperties = new HashMap<>();

        if (customProperties != null) {
            for (Object keyObj : customProperties.keySet()) {
                String key = keyObj.toString();
                this.customProperties.put(key, customProperties.getProperty(key));
            }
        }
    }

    /**
     * @return the category
     */
    public String getCategory() {
        Object value = tuples.get(CATEGORY_LABEL);
        return value != null ? value.toString() : null;
    }

    /**
     * @param category
     */
    public void setCategory(String category) {
        tuples.put(CATEGORY_LABEL, category);
    }

    /**
     * @return the allowed values
     */
    public Object[] getAllowedValues( ) {
        Object[] values = (Object[]) tuples.get(ALLOWED_VALUES_LABEL);
        return values != null ? values : null;
    }

    /**
     * @param allowedValues
     */
    public <T> void setAllowedValues(Collection<T> allowedValues) {
        if (allowedValues == null || allowedValues.isEmpty())
            return;

        tuples.put(ALLOWED_VALUES_LABEL, allowedValues.toArray(new Object[0]));
    }
}
