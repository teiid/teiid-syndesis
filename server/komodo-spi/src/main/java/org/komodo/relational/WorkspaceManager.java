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
import org.komodo.relational.profile.ViewEditorState;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.UnitOfWork;

public interface WorkspaceManager {

	Vdb findVdb(UnitOfWork uow, String vdbName) throws KException;

	Profile getUserProfile(UnitOfWork transaction) throws KException;

	Dataservice findDataservice(UnitOfWork uow, String dataserviceName) throws KException;

	Dataservice[] findDataservices(UnitOfWork uow, String searchPattern) throws KException;

	Vdb[] findVdbs(UnitOfWork uow, String string) throws KException;

	Vdb[] findVdbs(UnitOfWork uow) throws KException;

	Model findModel(UnitOfWork uow, Vdb vdb, String modelName) throws KException;

	void deleteVdb(UnitOfWork transaction, Vdb vdb) throws KException;
	
	void deleteDataservice(UnitOfWork transaction, Dataservice dataservice) throws KException;

	Dataservice createDataservice(UnitOfWork uow, String serviceName) throws KException;

	Vdb createVdb(UnitOfWork uow, String vdbName, String externalFilePath) throws KException;

	void refreshServiceVdb(UnitOfWork uow, Vdb serviceVdb, ViewEditorState[] editorStates) throws KException;

	String getKomodoWorkspaceAbsolutePath(UnitOfWork uow) throws KException;

	boolean isSchemaLoading(UnitOfWork uow, Model schemaModel) throws KException;

}
