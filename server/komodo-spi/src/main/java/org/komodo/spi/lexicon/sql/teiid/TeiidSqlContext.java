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
package org.komodo.spi.lexicon.sql.teiid;

import org.komodo.spi.lexicon.sql.teiid.TeiidSqlLexicon;

/**
 * Wrapping interface for passing parameters into {@link TeiidSqlLexicon#redirect}
 */
public interface TeiidSqlContext {

    /**
     * @param key
     * @return object indexed against given key
     */
    Object get(String key);

    /**
     * Adds an object to the context with the given key
     *
     * @param key
     * @param obj
     */
    void add(String key, Object obj);
}
