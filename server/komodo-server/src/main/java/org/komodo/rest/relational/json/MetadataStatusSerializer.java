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
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.rest.relational.response.metadata.RestMetadataStatus;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public class MetadataStatusSerializer extends AbstractEntitySerializer<RestMetadataStatus> {

    @Override
    protected RestMetadataStatus createEntity() {
        return new RestMetadataStatus();
    }

    @Override
    protected String readExtension(String name, RestMetadataStatus status, JsonReader in) {
        if (RestMetadataStatus.DATA_SOURCE_DRIVERS_LABEL.equals(name)) {
            RestConnectionDriver[] drivers = BUILDER.fromJson(in, RestConnectionDriver[].class);
            status.setDataSourceDrivers(drivers);
            return name;
        }

        return null;
    }

    @Override
    protected void writeExtensions(JsonWriter out, RestMetadataStatus entity) throws IOException {
        out.name(RestMetadataStatus.DATA_SOURCE_DRIVERS_LABEL);
        BUILDER.toJson(entity.getDataSourceDrivers().toArray(new RestConnectionDriver[0]), RestConnectionDriver[].class, out);
    }

    @Override
    protected boolean isComplete(RestMetadataStatus entity) {
        return entity.getVersion() != null;
    }
}
