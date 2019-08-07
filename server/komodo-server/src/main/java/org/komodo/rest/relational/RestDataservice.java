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
package org.komodo.rest.relational;

import java.util.Arrays;

import org.komodo.KException;
import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.openshift.BuildStatus;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * A Dataservice.
 */
@JsonSerialize(as = RestDataservice.class)
@JsonInclude(Include.NON_NULL)
public final class RestDataservice {

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = "tko__description";
    
    /**
     * Label used to describe dataservice viewNames
     */
    public static final String DATASERVICE_VIEW_DEFINITIONS_LABEL = "serviceViewDefinitions"; //$NON-NLS-1$
    
    public static final String DATASERVICE_NAME_LABEL = "keng__id"; //$NON-NLS-1$

    private String id;
    @JsonProperty(DATASERVICE_NAME_LABEL)
    private String name;
    @JsonProperty(DESCRIPTION_LABEL)
    private String description;
    private String serviceViewModel;
    @JsonProperty(DATASERVICE_VIEW_DEFINITIONS_LABEL)
    private String[] viewDefinitionNames;
    private String serviceVdbName;
    private String publishedState;
    private String podNamespace;
    private String publishPodName;
    private String odataHostName;
    
    /**
     * Constructor for use when deserializing
     */
    public RestDataservice() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param dataService the dataService
     * @throws KException if error occurs
     */
    public RestDataservice(DataVirtualization dataService, String vdbName) throws KException {
        setName(dataService.getName());
        
        setId(dataService.getId());

        setDescription(dataService.getDescription());

        setServiceVdbName(vdbName);

        // Initialize the published state to NOTFOUND
        setPublishedState(BuildStatus.Status.NOTFOUND.name());
    }

    /**
     * @return the VDB description (can be empty)
     */
    public String getDescription() {
    	return description;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
    	this.description = description;
    }

    /**
     * @return the service view model name (can be empty)
     */
    public String getServiceViewModel() {
    	return this.serviceViewModel;
    }

    /**
     * @param modelName the view model name to set
     */
    public void setServiceViewModel(String modelName) {
    	this.serviceViewModel = modelName;
    }

    /**
     * @return the service ViewDefinition names (can be empty)
     */
    public String[] getViewDefinitionNames() {
        return this.viewDefinitionNames;
    }

    /**
     * @param viewDefinitionNames the service view names to set
     */
    public void setViewDefinitionNames(final String[] viewDefinitionNames) {
        this.viewDefinitionNames = viewDefinitionNames;
    }

    /**
     * @return the service vdb name (can be empty)
     */
    public String getServiceVdbName() {
    	return this.serviceVdbName;
    }

    /**
     * @param serviceVdbName the service vdb name to set
     */
    public void setServiceVdbName(String serviceVdbName) {
        this.serviceVdbName = serviceVdbName;
    }

    /**
     * @return the service published state (never empty)
     */
    public String getPublishedState() {
    	return this.publishedState;
    }
    
    /**
     * @param publishedState the published state
     */
    public void setPublishedState(String publishedState) {
    	this.publishedState = publishedState;
    }

    /**
     * @return the pod namespace (can be empty)
     */
    public String getPodNamespace() {
    	return this.podNamespace;
    }

    /**
     * @param podNamesapce the service pod namespace to set
     */
    public void setPodNamespace(String podNamespace) {
    	this.podNamespace = podNamespace;
    }

    /**
     * @return the service pod name (can be empty)
     */
    public String getPublishPodName() {
    	return this.publishPodName;
    }

    /**
     * @param publishPodName the service pod name to set
     */
    public void setPublishPodName(String publishPodName) {
        this.publishPodName = publishPodName;
    }
    
    /**
     * @return the service pod name (can be empty)
     */
    public String getOdataHostName() {
    	return this.odataHostName;
    }

    /**
     * @param publishPodName the service pod name to set
     */
    public void setOdataHostName(String odataHostName) {
    	this.odataHostName = odataHostName;
    }
    
    public String getName() {
    	return this.name;
    }
    
    public void setName(String name) {
    	this.name = name;
    }
    
    public String getId() {
    	return this.id;
    }
    
    public void setId(String id) {
    	this.id = id;
    }

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((description == null) ? 0 : description.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((odataHostName == null) ? 0 : odataHostName.hashCode());
		result = prime * result + ((podNamespace == null) ? 0 : podNamespace.hashCode());
		result = prime * result + ((publishPodName == null) ? 0 : publishPodName.hashCode());
		result = prime * result + ((publishedState == null) ? 0 : publishedState.hashCode());
		result = prime * result + ((serviceVdbName == null) ? 0 : serviceVdbName.hashCode());
		result = prime * result + ((serviceViewModel == null) ? 0 : serviceViewModel.hashCode());
		result = prime * result + Arrays.hashCode(viewDefinitionNames);
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		RestDataservice other = (RestDataservice) obj;
		if (description == null) {
			if (other.description != null)
				return false;
		} else if (!description.equals(other.description))
			return false;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (odataHostName == null) {
			if (other.odataHostName != null)
				return false;
		} else if (!odataHostName.equals(other.odataHostName))
			return false;
		if (podNamespace == null) {
			if (other.podNamespace != null)
				return false;
		} else if (!podNamespace.equals(other.podNamespace))
			return false;
		if (publishPodName == null) {
			if (other.publishPodName != null)
				return false;
		} else if (!publishPodName.equals(other.publishPodName))
			return false;
		if (publishedState == null) {
			if (other.publishedState != null)
				return false;
		} else if (!publishedState.equals(other.publishedState))
			return false;
		if (serviceVdbName == null) {
			if (other.serviceVdbName != null)
				return false;
		} else if (!serviceVdbName.equals(other.serviceVdbName))
			return false;
		if (serviceViewModel == null) {
			if (other.serviceViewModel != null)
				return false;
		} else if (!serviceViewModel.equals(other.serviceViewModel))
			return false;
		if (!Arrays.equals(viewDefinitionNames, other.viewDefinitionNames))
			return false;
		return true;
	}
    
}
