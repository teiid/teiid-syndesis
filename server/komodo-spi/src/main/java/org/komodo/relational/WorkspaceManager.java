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

import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Model;
import org.komodo.relational.profile.Profile;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;

public interface WorkspaceManager {

	Vdb createVdb(String vdbName) throws KException;

	Vdb findVdb(String vdbName) throws KException;

	Vdb[] findVdbs(String searchPattern) throws KException;

	void deleteVdb(Vdb vdb) throws KException;
	

	Dataservice createDataservice(String serviceName) throws KException;

	Dataservice findDataservice(String dataserviceName) throws KException;

	Dataservice[] findDataservices(String searchPattern) throws KException;

	void deleteDataservice(Dataservice dataservice) throws KException;


	Profile getUserProfile() throws KException;

		
	boolean isSchemaActive(Model schemaModel) throws KException;

}
