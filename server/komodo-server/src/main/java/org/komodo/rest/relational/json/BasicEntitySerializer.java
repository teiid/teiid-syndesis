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

import java.io.IOException;

import org.komodo.rest.RestBasicEntity;
import org.komodo.utils.StringUtils;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestBasicEntity}s.
 * @param <T> the specific type of {@link RestBasicEntity}
 */
public class BasicEntitySerializer<T extends RestBasicEntity> extends AbstractEntitySerializer<T> {

    @SuppressWarnings( "unchecked" )
    protected T createEntity() {
        return (T) new RestBasicEntity();
    }

    @Override
    protected String readExtension(String name, T entity, JsonReader in) {
        // Do nothing by default
        return null;
    }

    @Override
    protected void writeExtensions(final JsonWriter out, final T entity) throws IOException {
        // Do nothing by default
    }

    @Override
    protected boolean isComplete(T entity) {
        return ! StringUtils.isBlank(entity.getId());
    }
}
