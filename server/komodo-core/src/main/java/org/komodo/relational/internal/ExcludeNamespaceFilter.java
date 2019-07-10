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
package org.komodo.relational.internal;

import org.komodo.core.LexiconConstants.JcrLexicon;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.ArgCheck;

/**
 * A filter for excluding descriptors and properties from specific namespaces.
 */
public class ExcludeNamespaceFilter implements Filter {

    private final String[] prefixes;

    /**
     * @param namespacePrefix
     *        the namespace prefix (cannot be empty)
     * @param namespaceUri
     *        the namespace URI (cannot be empty)
     */
    public ExcludeNamespaceFilter( final String namespacePrefix,
                                   final String namespaceUri ) {
        ArgCheck.isNotNull( namespacePrefix, "namespacePrefix" ); //$NON-NLS-1$
        ArgCheck.isNotNull( namespaceUri, "namespaceUri" ); //$NON-NLS-1$

        this.prefixes = new String[ 2 ];
        this.prefixes[0] = ( namespacePrefix + StringConstants.COLON );
        this.prefixes[1] = ( StringConstants.OPEN_BRACE + JcrLexicon.Namespace.URI + StringConstants.CLOSE_BRACE );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectDescriptor(java.lang.String)
     */
    @Override
    public boolean rejectDescriptor( final String descriptorName ) {
        ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$
        return ( descriptorName.startsWith( this.prefixes[0] ) || descriptorName.startsWith( this.prefixes[1] ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectProperty(java.lang.String)
     */
    @Override
    public boolean rejectProperty( final String propName ) {
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
        return ( propName.startsWith( this.prefixes[0] ) || propName.startsWith( this.prefixes[1] ) );
    }

}
