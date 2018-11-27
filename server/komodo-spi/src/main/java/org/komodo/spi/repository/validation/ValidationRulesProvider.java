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
package org.komodo.spi.repository.validation;

import java.io.InputStream;
import org.komodo.spi.repository.ValidationManager;

/**
 * A provider of {@link Rule validation rules}.
 */
public interface ValidationRulesProvider {

    /**
     * The provided stream will be closed by the {@link ValidationManager validation manager}.
     *
     * @return the stream of the XML file containing the rule definitions (cannot be <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    InputStream provideRules() throws Exception;

}
