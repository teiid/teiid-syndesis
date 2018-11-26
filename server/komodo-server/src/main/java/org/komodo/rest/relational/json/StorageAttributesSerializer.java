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

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.spi.repository.DocumentType;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status KomodoStorageObject}s.
 */
public final class StorageAttributesSerializer extends AbstractContentSerializer<KomodoStorageAttributes> {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public KomodoStorageAttributes read( final JsonReader in ) throws IOException {
        final KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            if (readContent(in, storageAttr, name) != null)
                continue;

            switch ( name ) {
                case KomodoStorageAttributes.STORAGE_TYPE_LABEL:
                    storageAttr.setStorageType(in.nextString());
                    break;
                case KomodoStorageAttributes.ARTIFACT_PATH_LABEL:
                    storageAttr.setArtifactPath(in.nextString());
                    break;
                case KomodoStorageAttributes.PARAMETERS_LABEL:
                    Map<String, String> parameters = BUILDER.fromJson(in, Map.class);
                    for (Map.Entry<String, String> parameter : parameters.entrySet()) {
                        storageAttr.setParameter(parameter.getKey(), parameter.getValue());
                    }
                    break;
                case KomodoStorageAttributes.DOCUMENT_TYPE_LABEL:
                    String docTypeValue = in.nextString();
                    DocumentType docType = DocumentType.documentType(docTypeValue);
                    storageAttr.setDocumentType(docType);
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return storageAttr;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final KomodoStorageAttributes value ) throws IOException {

        out.beginObject();

        out.name(KomodoStorageAttributes.STORAGE_TYPE_LABEL);
        out.value(value.getStorageType());

        out.name(KomodoStorageAttributes.ARTIFACT_PATH_LABEL);
        out.value(value.getArtifactPath());

        writeContent(out, value);

        String docType = value.getDocumentType();
        if (docType != null) {
            out.name(KomodoStorageAttributes.DOCUMENT_TYPE_LABEL);
            out.value(docType.toString());
        }

        if (! value.getParameters().isEmpty()) {
            out.name(KomodoStorageAttributes.PARAMETERS_LABEL);
            BUILDER.toJson(value.getParameters(), STRING_MAP_TYPE, out);
        }

        out.endObject();
    }

}
