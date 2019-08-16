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
package org.komodo.datasources;

import java.util.Map;

public class ODataV4Definition extends DataSourceDefinition {

    @Override
    public String getType() {
        return "webservice";
    }

    @Override
    public String getPomDendencies() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getTranslatorName() {
        return "odata4";
    }

    @Override
    public boolean isTypeOf(Map<String, String> properties) {
        if ((properties != null) && (properties.get("URL") != null)) {
            return true;
        }
        return false;
    }

    @Override
    public Map<String, String> getInternalTeiidDataSourceProperties(DefaultSyndesisDataSource source) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, String> getPublishedImageDataSourceProperties(DefaultSyndesisDataSource scd) {
        throw new UnsupportedOperationException();
    }
}
