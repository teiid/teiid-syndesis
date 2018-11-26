/*************************************************************************************
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
package org.komodo.importer;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.komodo.utils.ModelType;

/**
 * ImportOptions - hold options that may be set for the import
 */
public class ImportOptions {

    private static final String DEFAULT_ARTEFACT_NAME = "<imported>"; //$NON-NLS-1$

    private static final String DEFAULT_VDB_FILE_PATH = "<not available>"; //$NON-NLS-1$

    /**
     * Options for handling the import of a node that already exists
     */
    public static enum ExistingNodeOptions {
        /**
         * Replace the existing node
         */
        OVERWRITE,

        /**
         * Do not proceed further
         */
        RETURN,

        /**
         * Create a new sister node with slightly edited name
         */
        CREATE_NEW
    }

    /**
     * Option keys applicable to import ddl into a model
     */
    public static enum OptionKeys {
        /**
         * Name of artefact
         */
        NAME(DEFAULT_ARTEFACT_NAME),

        /**
         * Type of model, default is PHYSICAL
         */
        MODEL_TYPE(ModelType.Type.PHYSICAL),

        /**
         * Location of vdb file
         */
        VDB_FILE_PATH(DEFAULT_VDB_FILE_PATH),

        /**
         * What to do if there is already a vdb with the same name
         */
        HANDLE_EXISTING(ExistingNodeOptions.OVERWRITE);

        private Object defaultValue;

        /**
         * Constructor
         */
        private OptionKeys(Object defaultValue) {
            this.defaultValue = defaultValue;
        }

        /**
         * @return default value of this model option
         */
        public Object defaultValue() {
            return defaultValue;
        }
    }

    private final Map<OptionKeys, Object> options = new HashMap<OptionKeys, Object>();

    /**
     * @param optionName name of the option
     * @return the option for the given model option
     */
    public Object getOption(OptionKeys optionName) {
        if (optionName == null)
            return null;

        Object value = options.get(optionName);
        if (value != null)
            return value;

        // Value is null so return defaults if applicable
        return optionName.defaultValue();
    }

    /**
     * @param optionName name of the option
     * @param value value of the option
     */
    public void setOption(OptionKeys optionName, Object value) {
        options.put(optionName, value);
    }

    /**
     * Merge the options in the given option set into this option set
     *
     * @param newOptions new set of options
     */
    public void mergeOptions(ImportOptions newOptions) {
        for (Entry<OptionKeys, Object> entry : newOptions.options.entrySet()) {
            this.setOption(entry.getKey(), entry.getValue());
        }
    }

}
