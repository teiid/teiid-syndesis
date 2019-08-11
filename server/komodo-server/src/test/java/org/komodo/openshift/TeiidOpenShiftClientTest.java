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

package org.komodo.openshift;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.komodo.KEngine;
import org.komodo.UnitOfWork;
import org.komodo.WorkspaceManager;
import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.metadata.MetadataInstance;
import org.komodo.rest.KomodoConfigurationProperties;
import org.mockito.Mockito;

public class TeiidOpenShiftClientTest {

	@Test public void testSetKomodoName() throws Exception {
        MetadataInstance metadata = Mockito.mock(MetadataInstance.class);
        KEngine kengine = Mockito.mock(KEngine.class);
		Mockito.when(kengine.createTransaction(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean(),
				Mockito.anyString())).thenReturn(Mockito.mock(UnitOfWork.class));
		Mockito.when(kengine.getWorkspaceManager()).thenReturn(Mockito.mock(WorkspaceManager.class));
        
		TeiidOpenShiftClient client = new TeiidOpenShiftClient(metadata, new EncryptionComponent("blah"), new KomodoConfigurationProperties(), kengine);
		
		DefaultSyndesisDataSource dsd = new DefaultSyndesisDataSource();
		
		client.setUniqueKomodoName(dsd, "views", "x");
		
		assertEquals("views1", dsd.getKomodoName());
		
		client.setUniqueKomodoName(dsd, "View", "x");
		
		assertEquals("View", dsd.getKomodoName());

		client.setUniqueKomodoName(dsd, "?syS.", "x");
		
		assertEquals("syS1", dsd.getKomodoName());
	}
	
}
