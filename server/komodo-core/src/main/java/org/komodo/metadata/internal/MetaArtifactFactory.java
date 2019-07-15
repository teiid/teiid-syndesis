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
package org.komodo.metadata.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Properties;

import org.komodo.metadata.runtime.TeiidDataSource;
import org.komodo.metadata.runtime.TeiidPropertyDefinition;
import org.komodo.metadata.runtime.TeiidTranslator;
import org.komodo.metadata.runtime.TeiidVdb;
import org.teiid.adminapi.PropertyDefinition;
import org.teiid.adminapi.Translator;
import org.teiid.adminapi.VDB;

public class MetaArtifactFactory {

    /**
     * The prefix used before the workspace identifier when creating a Preview VDB name.
     */
    public static final String PREVIEW_PREFIX = "PREVIEW_"; //$NON-NLS-1$

    public TeiidDataSource createDataSource(String name, Properties dataSource) {

        TeiidDataSource teiidDataSource = new TeiidDataSourceImpl(name, dataSource); //$NON-NLS-1$

        return teiidDataSource;
    }

    public TeiidTranslator createTranslator(Translator translator) {
        TeiidTranslatorImpl teiidTranslator = new TeiidTranslatorImpl(translator);
        return teiidTranslator;
    }

    public TeiidVdb createVdb(VDB vdb) throws Exception {
        TeiidVdb teiidVdb = new TeiidVdbImpl(vdb);
        return teiidVdb;
    }

    @SuppressWarnings( "unchecked" )
    public TeiidPropertyDefinition createPropertyDefinition(PropertyDefinition propDef) {
        TeiidPropertyDefinition teiidPropDef = new TeiidPropertyDefinition();

        Collection<Object> allowedValues = propDef.getAllowedValues();
        List<String> av = new ArrayList<>();
        for(Object value : allowedValues)
            av.add(value.toString());
        teiidPropDef.setAllowedValues(av);

        teiidPropDef.setCategory(propDef.getCategory());
        teiidPropDef.setDefaultValue(propDef.getDefaultValue());
        teiidPropDef.setDescription(propDef.getDescription());
        teiidPropDef.setDisplayName(propDef.getDisplayName());
        teiidPropDef.setName(propDef.getName());
        teiidPropDef.setProperties(propDef.getProperties());
        teiidPropDef.setPropertyTypeClassName(propDef.getPropertyTypeClassName());

        teiidPropDef.setAdvanced(propDef.isAdvanced());
        teiidPropDef.setConstrainedToAllowedValues(propDef.isConstrainedToAllowedValues());
        teiidPropDef.setMasked(propDef.isMasked());
        teiidPropDef.setModifiable(propDef.isModifiable());
        teiidPropDef.setRequired(propDef.isRequired());

        if (propDef.getRequiresRestart() != null) {
            TeiidPropertyDefinition.RestartType restartType = TeiidPropertyDefinition.RestartType.findRestartType(propDef.getRequiresRestart().name());
            teiidPropDef.setRequiresRestart(restartType);
        }

        return teiidPropDef;
    }

}
