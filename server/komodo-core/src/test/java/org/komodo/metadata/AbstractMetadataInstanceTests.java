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
package org.komodo.metadata;

import org.komodo.spi.metadata.MetadataInstance;
import org.komodo.spi.metadata.TeiidConnectionProvider;
import org.mockito.Mockito;
import org.teiid.adminapi.Admin;

public class AbstractMetadataInstanceTests {

    private static MetadataInstance METADATA_INSTANCE;

    protected MetadataInstance getMetadataInstance() throws Exception {
    	if (METADATA_INSTANCE == null) {
		    Admin admin = Mockito.mock(Admin.class);
		    TeiidConnectionProvider provider = Mockito.mock(TeiidConnectionProvider.class);
		    Mockito.when(provider.getAdmin()).thenReturn(admin);
		    METADATA_INSTANCE = new DefaultMetadataInstance(provider);
    	}
        return METADATA_INSTANCE;
    }

}
