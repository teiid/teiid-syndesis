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

import org.komodo.KException;
import org.komodo.StringConstants;

/**
 * A model of a datavirtualization instance
 */
public interface DataVirtualization extends Named {
	
	static String getServiceVdbName(String name) {
		return name.toLowerCase() + StringConstants.SERVICE_VDB_SUFFIX;
	}

    /**
     * @return the service VDB name (may be <code>null</code> if not defined)
     */
    default String getServiceVdbName( ) throws KException {
    	return getServiceVdbName(getName());
    }

    /**
     * @return the value of the <code>description</code> property (can be empty)
     */
    String getDescription( );

    /**
     * @param newDescription
     *        the new value of the <code>description</code> property
     */
    void setDescription( 
                         final String newDescription );
    
    /**
     * Get the id of the virtualization
     */
    String getId();

	void setName(String name);
    
}
