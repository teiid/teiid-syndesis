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
package org.komodo.rest.relational.json;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Properties;
import org.komodo.rest.relational.response.metadata.RestMetadataTemplateEntry;
import org.komodo.utils.StringUtils;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestMetadataTemplateEntry}s.
 */
public final class MetadataTemplateEntrySerializer extends BasicEntitySerializer<RestMetadataTemplateEntry> {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    @Override
    protected boolean isComplete(final RestMetadataTemplateEntry entry) {
        return ! StringUtils.isBlank(entry.getId()) && entry.getkType() != null;
    }

    @Override
    protected RestMetadataTemplateEntry createEntity() {
        return new RestMetadataTemplateEntry();
    }

    @Override
    protected String readExtension(String name, RestMetadataTemplateEntry entity, JsonReader in) {
        if (RestMetadataTemplateEntry.CUSTOM_PROPERTIES_LABEL.equals(name)) {
            Properties customProperties = new Properties();
            Map<String, String> properties = BUILDER.fromJson(in, Map.class);
            for (Map.Entry<String, String> property : properties.entrySet()) {
                customProperties.setProperty(property.getKey(), property.getValue());
            }
            entity.setCustomProperties(customProperties);
            return RestMetadataTemplateEntry.CUSTOM_PROPERTIES_LABEL;
        }

        return null;
    }

    @Override
    protected void writeExtensions(JsonWriter out, RestMetadataTemplateEntry entity) throws IOException {
        if (! entity.getCustomProperties().isEmpty()) {
            out.name(RestMetadataTemplateEntry.CUSTOM_PROPERTIES_LABEL);
            BUILDER.toJson(entity.getCustomProperties(), STRING_MAP_TYPE, out);
        }
    }
}
