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

import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbTranslator;
import org.komodo.utils.StringUtils;

/**
 * A GSON serializer/deserializer for {@link RestVdbTranslator}s.
 */
public final class MetadataVdbTranslatorSerializer extends BasicEntitySerializer<RestMetadataVdbTranslator> {

    @Override
    protected boolean isComplete(final RestMetadataVdbTranslator translator) {
        return !StringUtils.isBlank(translator.getId()) && !StringUtils.isBlank(translator.getType());
    }

    @Override
    protected RestMetadataVdbTranslator createEntity() {
        return new RestMetadataVdbTranslator();
    }
}
