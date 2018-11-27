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
package org.komodo.relational;

import java.io.InputStream;
import org.komodo.spi.repository.validation.ValidationRulesProvider;

/**
 * A validation rules provider for relational objects.
 */
public class RelationalValidationRulesProvider implements ValidationRulesProvider {

    private static final String VALIDATION_RULES_FILE_NAME = "relationalValidationRulesDefault.xml"; //$NON-NLS-1$

    /**
     * Constructs a rules provider for relational objects.
     */
    public RelationalValidationRulesProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.ValidationRulesProvider#provideRules()
     */
    @Override
    public InputStream provideRules() throws Exception {
        return getClass().getClassLoader().getResourceAsStream( VALIDATION_RULES_FILE_NAME );
    }

}
