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
import org.komodo.rest.relational.response.metadata.RestMetadataTemplate;
import org.komodo.utils.StringUtils;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestMetadataTemplate}s.
 */
public final class MetadataTemplateSerializer extends BasicEntitySerializer<RestMetadataTemplate> {

    @Override
    protected boolean isComplete(RestMetadataTemplate entity) {
        return ! StringUtils.isBlank(entity.getId()) && entity.getkType() != null;
    }

    @Override
    protected RestMetadataTemplate createEntity() {
        return new RestMetadataTemplate();
    }

    @Override
    public String readExtension(String name, RestMetadataTemplate entity, JsonReader in) {
        if (RestMetadataTemplate.ENTRIES_LABEL.equals(name)) {
            String[] entries = BUILDER.fromJson(in, String[].class);
            entity.setEntries(entries);
            return RestMetadataTemplate.ENTRIES_LABEL;
        }

        return null;
    }

    @Override
    public void writeExtensions(final JsonWriter out, final RestMetadataTemplate entity) throws IOException {
        if (entity.getEntries().size() > 0) {
            out.name(RestMetadataTemplate.ENTRIES_LABEL);
            BUILDER.toJson(entity.getEntries().toArray(new String[0]), String[].class, out);
        }
    }
}
