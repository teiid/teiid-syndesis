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

import java.util.Calendar;

import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;

/**
 * A model of a dataservice instance
 */
public interface Dataservice extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Dataservice.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.DATASERVICE;

    /**
     * @param name
     * @return the service VDB name (may be <code>null</code> if not defined)
     * @throws KException
     *         if an error occurs
     */
    void setServiceVdbName( String name ) throws KException;

    /**
     * @return the service VDB name (may be <code>null</code> if not defined)
     * @throws KException
     *         if an error occurs
     */
    String getServiceVdbName( ) throws KException;

    /**
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( ) throws KException;

    /**
     * @return the last time the manifest was modified (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Calendar getLastModified( ) throws KException;

    /**
     * @return the name of the user who last modified the data service (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getModifiedBy( ) throws KException;

    /**
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( 
                         final String newDescription ) throws KException;

    /**
     * @param newLastModified
     *        the new value of the <code>last modified date</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLastModified( 
                          final Calendar newLastModified ) throws KException;

    /**
     * @param newModifiedBy
     *        the new value of the <code>modified by</code> property
     * @throws KException
     *         if an error occurs
     */
    void setModifiedBy( 
                        final String newModifiedBy ) throws KException;
    
}
