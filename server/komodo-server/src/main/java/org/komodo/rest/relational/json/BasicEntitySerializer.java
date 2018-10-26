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
        return ! StringUtils.isBlank(entity.getId()) && ! StringUtils.isBlank(entity.getDataPath()) &&
                       entity.getkType() != null;
    }
}
