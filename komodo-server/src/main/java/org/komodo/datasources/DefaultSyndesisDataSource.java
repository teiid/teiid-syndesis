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

import org.komodo.metadata.internal.TeiidDataSourceImpl;

public class DefaultSyndesisDataSource {
    private String id;
    private String syndesisName;
    private volatile String komodoName;
    private String translator;
    private Map<String, String> properties;
    private DataSourceDefinition definition;

    public String getId() {
        return id;
    }

    public String getSyndesisName() {
        return syndesisName;
    }

    public String getType() {
        return definition.getType();
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setSyndesisName(String syndesisName) {
        this.syndesisName = syndesisName;
    }

    public String getTranslatorName() {
        return translator;
    }

    public void setTranslatorName(String translator) {
        this.translator = translator;
    }

    public DataSourceDefinition getDefinition() {
        return definition;
    }

    public void setDefinition(DataSourceDefinition definition) {
        this.definition = definition;
    }

    public Map<String, String> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }

    public String getProperty(String key) {
        return this.properties.get(key);
    }

    public TeiidDataSourceImpl createDataSource() {
        return this.definition.createDatasource(this.komodoName, this);
    }

    /**
     * If bound returns the unique Komodo datasource name, which is also a valid
     * schema name.  It will already be cleansed of problematic characters.
     * @return
     */
    public String getKomodoName() {
        return komodoName;
    }

    public void setKomodoName(String komodoName) {
        this.komodoName = komodoName;
    }
}
