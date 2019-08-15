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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.KException;
import org.komodo.metadata.MetadataInstance.ValidationResult;
import org.teiid.adminapi.impl.VDBMetadataParser;
import org.teiid.runtime.EmbeddedConfiguration;

public class DefaultMetadataInstanceTest {

	DefaultMetadataInstance metadataInstance;
	TeiidServer server;

	@Before
	public void init() {
		EmbeddedConfiguration ec = new EmbeddedConfiguration();
		server = new TeiidServer();
		server.start(ec);

		metadataInstance = new DefaultMetadataInstance(server);
	}
	
	@After
	public void tearDown() {
		server.stop();
	}

	@Test(expected = KException.class)
	public void shouldNotValidateNoVdb() throws KException {
		metadataInstance.validate("x", "create view v as select 1");
	}

	@Test
	public void shouldValidate() throws Exception {
		String vdb = "<vdb name=\"myservice\" version=\"1\">\n" + 
				"    <model visible=\"true\" name=\"accounts\" type=\"VIRTUAL\">\n" +
				"      <metadata type=\"DDL\">create view tbl (col) as select 1;</metadata>" +
				"    </model>    \n" + 
				"</vdb>";
		
		metadataInstance.deploy(VDBMetadataParser.unmarshell(new ByteArrayInputStream(vdb.getBytes("UTF-8"))));
		
		ValidationResult report = metadataInstance.validate("myservice", "create view v as select * from tbl");
		assertFalse(report.getReport().toString(), report.getReport().hasItems());
		
		report = metadataInstance.validate("myservice", "create view v as select * from tbl1");
		assertTrue(report.toString(), report.getReport().hasItems());
	}

}
