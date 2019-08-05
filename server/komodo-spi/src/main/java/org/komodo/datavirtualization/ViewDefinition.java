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
package org.komodo.datavirtualization;

import java.util.List;

import org.komodo.StringConstants;

/**
 * Represents the configuration of a view definition
 */
public interface ViewDefinition  extends Named, StringConstants {

    /**
     * @param compositionName
     *        the name of the sql composition being added (cannot be empty)
     * @return the new sql composition (never <code>null</code>)
     */
    SqlComposition addComposition(  String compositionName );
    
    /**
     * @return the sql compositions (never <code>null</code> but can be empty)
     */
    List<SqlComposition> getCompositions( );

    /**
     * @return the description
     */
    String getDescription();
    
    /**
     * @param description value of description
     */
    void setDescription( String description);
    
    /**
     * @return the view DDL
     */
    String getDdl();
    
    /**
     * @param ddl value of view ddl
     */
    void setDdl( String ddl);

    /**
     * @param complete value for isComplete
     */
    void setComplete( boolean complete);
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return boolean value of isComplete
     */
    boolean isComplete();

    /**
     * @param userDefined value for isUserDefined
     */
    void setUserDefined( boolean userDefined);
    
    /**
     * @return boolean value of isUserDefined
     */
    boolean isUserDefined();
    
    /**
     * @return the string array of source paths
     */
    List<String> getSourcePaths( );
    
    /**
     * @param sourcePath 
     *        the name of the source path (cannot be empty)
     * @return the source paths
     */
    void addSourcePath(  String sourcePath );

    /**
     * @param columnName
     *        the name of the projected column being added (cannot be empty)
     * @return the new projected column (never <code>null</code>)
     */
    SqlProjectedColumn addProjectedColumn(  String columnName );
    
    /**
     * @return the sql projected columns (never <code>null</code> but can be empty)
     */
    List<SqlProjectedColumn> getProjectedColumns( );

	String getDataVirtualizationName();
	
	String getId();
	
	void clearState();

}
