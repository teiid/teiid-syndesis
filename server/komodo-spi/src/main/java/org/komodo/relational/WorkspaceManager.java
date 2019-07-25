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
import org.komodo.relational.dataservice.ViewEditorState;
import org.komodo.spi.KException;

public interface WorkspaceManager {
	
	String findSchema(String name) throws KException;
	
	boolean deleteSchema(String name) throws KException;
	
	void saveSchema(String name, String contents) throws KException;
	

	Dataservice createDataservice(String serviceName) throws KException;

	Dataservice findDataservice(String dataserviceName) throws KException;

	Dataservice[] findDataservices(String searchPattern) throws KException;

	void deleteDataservice(Dataservice dataservice) throws KException;


    /**
    *
    * @param stateId the id of the view editor state
    * @return the new view editor state (never <code>null</code>)
    * @throws KException
    *          if an error occurs
    */
   ViewEditorState addViewEditorState(  String stateId) throws KException;

   /**
    * @param namePatterns
    *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
    *
    * @return the view editor states (never <code>null</code> but can be empty)
    * @throws KException
    *         if an error occurs
    */
   ViewEditorState[] getViewEditorStates(  final String... namePatterns ) throws KException;

   /**
    * @param viewEditorStateId
    *        the id of the viewEditorState being removed (cannot be empty)
    * @return true if removed
    * @throws KException
    *         if an error occurs
    */
   boolean removeViewEditorState( 
                        final String viewEditorStateId ) throws KException;

   ViewEditorState getViewEditorState(String name) throws KException;

}
