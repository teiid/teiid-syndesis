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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.utils.ArgCheck;

/**
 * A filter for excluding specific {@link QName}s.
 */
public class ExcludeQNamesFilter implements Filter {

    private final List< String > qnames;

    /**
     * Constructs a filter.
     *
     * @param namesToExclude
     *        a collection of names being excluded (cannot be <code>null</code> or empty)
     */
    public ExcludeQNamesFilter( final String... namesToExclude ) {
        ArgCheck.isNotEmpty( namesToExclude, "qnames" ); //$NON-NLS-1$
        this.qnames = new ArrayList<>( namesToExclude.length );

        for ( final String name : namesToExclude ) {
            this.qnames.add( name );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectDescriptor(java.lang.String)
     */
    @Override
    public boolean rejectDescriptor( final String descriptorName ) {
        ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$
        return this.qnames.contains( descriptorName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectProperty(java.lang.String)
     */
    @Override
    public boolean rejectProperty( final String propName ) {
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
        return this.qnames.contains( propName );
    }

}
