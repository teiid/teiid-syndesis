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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import org.komodo.relational.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.lexicon.LexiconConstants.JcrLexicon;
import org.komodo.spi.lexicon.LexiconConstants.NTLexicon;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data service file resource (i.e., a driver, UDF, or DDL file).
 */
public interface DataServiceResource extends Exportable, RelationalObject {

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the binary contents of this resource as an {@link InputStream} (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    default InputStream getContent( final UnitOfWork transaction ) throws KException {
        if ( !hasChild( transaction, JcrLexicon.JCR_CONTENT ) ) return null;

        final KomodoObject fileNode = getChild( transaction, JcrLexicon.JCR_CONTENT, NTLexicon.NT_RESOURCE );
        final Property property = fileNode.getProperty( transaction, JcrLexicon.JCR_DATA );
        return property.getBinaryValue( transaction );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param content
     *        the bytes of content (can be <code>null</code> if there is no content)
     * @throws KException
     *         if an error occurs
     */
    default void setContent( final UnitOfWork transaction,
                             final byte[] content ) throws KException {
        KomodoObject fileNode = null;

        if ( !hasChild( transaction, JcrLexicon.JCR_CONTENT ) ) {
            fileNode = addChild( transaction, JcrLexicon.JCR_CONTENT, NTLexicon.NT_RESOURCE );
        } else {
            fileNode = getChild( transaction, JcrLexicon.JCR_CONTENT );
        }

        final ByteArrayInputStream stream = new ByteArrayInputStream( content );
        fileNode.setProperty( transaction, JcrLexicon.JCR_DATA, stream );
    }

}
