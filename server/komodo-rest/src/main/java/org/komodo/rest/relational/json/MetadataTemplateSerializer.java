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
