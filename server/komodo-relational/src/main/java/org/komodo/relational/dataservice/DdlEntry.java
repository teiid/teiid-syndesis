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
package org.komodo.relational.dataservice;

import org.komodo.relational.resource.DdlFile;
import org.komodo.spi.repository.KomodoType;

/**
 * Represents a data service entry for a DDL file.
 */
public interface DdlEntry extends DataServiceResourceEntry< DdlFile > {

    /**
     * The type identifier.
     */
    KomodoType IDENTIFIER = KomodoType.DDL_ENTRY;

    /**
     * An empty collection of DDL entries.
     */
    DdlEntry[] NO_ENTRIES = new DdlEntry[ 0 ];

}
