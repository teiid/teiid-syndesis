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

import java.util.Properties;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents an entry in a data service archive.
 *
 * @param <T>
 *        the entry type
 */
public interface DataServiceEntry< T extends Exportable & RelationalObject > extends Exportable, RelationalObject {

    /**
     * Indicates how the entry should uploaded and/or deployed.
     */
    public enum PublishPolicy {

        /**
         * Always publish the file.
         */
        ALWAYS( "always" ),

        /**
         * Only publish if not already published.
         */
        IF_MISSING( "ifMissing" ),

        /**
         * Never publish the file.
         */
        NEVER( "never" );

        /**
         * The default policy.
         *
         * @see #IF_MISSING
         */
        public static final PublishPolicy DEFAULT = IF_MISSING;

        /**
         * @param xml the XML value whose type is being requested (can be <code>null</code> or empty)
         * @return the appropriate type or <code>null</code> if not found
         */
        public static PublishPolicy fromXml( final String xml ) {
            for ( final PublishPolicy type : values() ) {
                if ( type.xml.equals( xml ) ) {
                    return type;
                }
            }

            return null;
        }

        private final String xml;

        private PublishPolicy( final String xmlValue ) {
            this.xml = xmlValue;
        }

        /**
         * @return the value appropriate for an XML document (never <code>null</code> or empty)
         */
        public String toXml() {
            return this.xml;
        }

    }

    /**
     * Empty resource content.
     */
    byte[] NO_CONTENT = new byte[0];

    /**
     * @return the archive path segment where the resource should be archived (can be <code>null</code> or empty if the resource
     *         should be located at the archive root)
     */
    String getArchiveFolder();

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    default byte[] export( final UnitOfWork transaction,
                           final Properties properties ) throws KException {
        final T resource = getReference( transaction );

        if ( resource == null ) {
            if ( getPublishPolicy( transaction ) != PublishPolicy.NEVER ) {
                throw new KException( Messages.getString( Relational.EXPORT_FAILED_NO_CONTENT, getAbsolutePath() ) );
            }

            return NO_CONTENT;
        }

        return resource.export( transaction, properties );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the entry path (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    String getEntryPath( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the entry's publish policy (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    PublishPolicy getPublishPolicy( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @return the referenced object or <code>null</code> if none exists
     * @throws KException
     *         if an error occurs
     */
    T getReference( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param newEntryPath
     *        the new entry path (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    void setEntryPath( final UnitOfWork transaction,
                               final String newEntryPath ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param newPublishPolicy
     *        the new publish policy (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setPublishPolicy( final UnitOfWork transaction,
                                   final PublishPolicy newPublishPolicy ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param reference
     *        the referenced object or <code>null</code> if removing an existing reference
     * @throws KException
     *         if an error occurs
     */
    void setReference( final UnitOfWork transaction,
                               final T reference ) throws KException;

}
