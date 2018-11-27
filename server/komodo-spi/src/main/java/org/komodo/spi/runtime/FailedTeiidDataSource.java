/*************************************************************************************
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
package org.komodo.spi.runtime;

import java.util.Properties;

/**
 *
 */
public class FailedTeiidDataSource implements TeiidDataSource {

	String modelName;
	String jndiName;
	int reasonCode;
	
	/**
	 * 
	 */
	public FailedTeiidDataSource(String modelName, String jndiName, int reasonCode) {
		this.modelName = modelName;
		this.jndiName = jndiName;
		this.reasonCode = reasonCode;
	}
	
	/**
	 * @return the modelName
	 */
	public String getModelName() {
		return this.modelName;
	}

	/**
	 * @return the jndiName
	 */
	public String getJndiName() {
		return this.jndiName;
	}

	/**
	 * @return the reasonCode
	 */
	public int getReasonCode() {
		return this.reasonCode;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getDisplayName()
	 */
	@Override
	public String getDisplayName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getName()
	 */
	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getType()
	 */
	@Override
	public String getType() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getProperties()
	 */
	@Override
	public Properties getProperties() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getPropertyValue(java.lang.String)
	 */
	@Override
	public String getPropertyValue(String name) {
		// TODO Auto-generated method stub
		return null;
	}

    @Override
    public String getConnectionUrl() {
        // TODO Auto-generated method stub
        return null;
    }
}
