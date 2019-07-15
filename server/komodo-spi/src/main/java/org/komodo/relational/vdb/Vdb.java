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
package org.komodo.relational.vdb;

import java.util.Properties;

import org.komodo.relational.DeployStatus;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.model.Model;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.UnitOfWork;
import org.w3c.dom.Document;

/**
 * Represents a virtual database manifest.
 */
public interface Vdb extends Exportable, RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Vdb.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB;

    /**
     * Represents a VDB XML manifest file.
     */
    public interface VdbManifest extends Exportable {

        /**
         * @return the manifest as an XML document (never <code>null</code>)
         * @throws KException
         *         if an error occurs
         */
        Document asDocument() throws KException;

    }

    /**
     * The default value indicating if this VDB is a preview VDB. Value is {@value} .
     */
    boolean DEFAULT_PREVIEW = false;

    /**
     * The default version number. Value is {@value} .
     */
    int DEFAULT_VERSION = 1;

    /**
     * Teiid Names for special properties
     */
    String SECURITY_DOMAIN_TEIIDNAME = "security-domain";
    String QUERY_TIMEOUT_TEIIDNAME = "query-timeout";
    String PASSWORD_PATTERN_TEIIDNAME = "password-pattern";
    String GSS_PATTERN_TEIIDNAME = "gss-pattern";
    String AUTHENTICATION_TYPE_TEIIDNAME = "authentication-type";
    String ALLOWED_LANGUAGES_TEIIDNAME = "allowed-languages";

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param vdbName
     *        the name of the VDB being imported (cannot be empty)
     * @return the new VDB import (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbImport addImport( final UnitOfWork transaction,
                         final String vdbName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param modelName
     *        the name of the VDB being imported (cannot be empty)
     * @return the new VDB import (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Model addModel( final UnitOfWork transaction,
                    final String modelName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param translatorName
     *        the name of the translator being added (cannot be empty)
     * @param translatorType
     *        the type of translator (cannot be empty)
     * @return the new translator (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Translator addTranslator( final UnitOfWork transaction,
                              final String translatorName,
                              final String translatorType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param properties (can be <code>null</code> or empty)
     * @return the VDB XML manifest representing the current state of the VDB (never null)
     * @throws KException
     *         if an error occurs
     */
    VdbManifest createManifest( final UnitOfWork transaction, Properties properties ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the allowed languages (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getAllowedLanguages( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the authentication type (can be empty)
     * @throws KException
     */
    String getAuthenticationType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>connection type</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getConnectionType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the GSS pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getGssPattern( final UnitOfWork transaction ) throws KException;
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     * @return the VDB model, or null if it does not exist
     * @throws KException
     *         if an error occurs
     */
    Model getModel( final UnitOfWork transaction,
                       final String name ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDB models (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Model[] getModels( final UnitOfWork transaction,
                       final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDB imports (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    VdbImport[] getImports( final UnitOfWork transaction,
                            final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>original file path</code> property (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getOriginalFilePath( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the password pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getPasswordPattern( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the query timeout or -1 if not set
     * @throws KException
     *         if an error occurs
     */
    int getQueryTimeout( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the translators (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Translator[] getTranslators( final UnitOfWork transaction,
                                 final String... namePatterns ) throws KException;

    /**
     * A name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>Teiid VDB name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getVdbName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VERSION
     */
    int getVersion( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if a preview VDB
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_PREVIEW
     */
    boolean isPreview( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param importToRemove
     *        the name of the VDB import being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeImport( final UnitOfWork transaction,
                       final String importToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param modelToRemove
     *        the name of the model being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeModel( final UnitOfWork transaction,
                      final String modelToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param translatorToRemove
     *        the name of the translator being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeTranslator( final UnitOfWork transaction,
                           final String translatorToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowedLanguages
     *        the new allowed languages (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAllowedLanguages( final UnitOfWork transaction,
                              final String newAllowedLanguages ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAuthenticationType
     *        the new authentication type (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAuthenticationType( final UnitOfWork transaction,
                                final String newAuthenticationType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newConnectionType
     *        the new value of the <code>connection type</code> property
     * @throws KException
     *         if an error occurs
     */
    void setConnectionType( final UnitOfWork transaction,
                            final String newConnectionType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newGssPattern
     *        the new GSS pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setGssPattern( final UnitOfWork transaction,
                        final String newGssPattern ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newOriginalFilePath
     *        the new value of the <code>original file path</code> property (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setOriginalFilePath( final UnitOfWork transaction,
                              final String newOriginalFilePath ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPasswordPattern
     *        the new password pattern (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setPasswordPattern( final UnitOfWork transaction,
                             final String newPasswordPattern ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPreview
     *        the new value for the <code>preview</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_PREVIEW
     */
    void setPreview( final UnitOfWork transaction,
                     final boolean newPreview ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newQueryTimeout
     *        the new query timeout or a negative number to delete the current value
     * @throws KException
     *         if an error occurs
     */
    void setQueryTimeout( final UnitOfWork transaction,
                          final int newQueryTimeout ) throws KException;

    /**
     * Sets the name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newVdbName
     *        the new value of the <code>Teiid VDB name</code> property
     * @throws KException
     *         if an error occurs
     */
    void setVdbName( final UnitOfWork transaction,
                     final String newVdbName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newVersion
     *        the new value of the <code>version</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_VERSION
     */
    void setVersion( final UnitOfWork transaction,
                     final int newVersion ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.UnitOfWork.State#NOT_STARTED})
     *
     * @return the deployment status of this vdb
     */
    DeployStatus deploy(UnitOfWork uow);

}
