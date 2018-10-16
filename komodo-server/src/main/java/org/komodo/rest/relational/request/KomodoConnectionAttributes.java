/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.relational.request;

import javax.ws.rs.core.MediaType;

import org.komodo.rest.KRestEntity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;


/**
 * Object to be serialised by GSON that encapsulates a connection object
 */
@JsonSerialize
@JsonInclude(value=Include.NON_NULL)
public class KomodoConnectionAttributes implements KRestEntity {

    /**
     * Label for the description
     */
    public static final String DESCRIPTION_LABEL = "description"; //$NON-NLS-1$

    /**
     * Label for the serviceCatalogSource
     */
    public static final String SERVICE_CATALOG_SOURCE_LABEL = "serviceCatalogSource"; //$NON-NLS-1$

    @JsonProperty(DESCRIPTION_LABEL)
    private String description;

    @JsonProperty(SERVICE_CATALOG_SOURCE_LABEL)
    private String serviceCatalogSource;

    /**
     * Default constructor for deserialization
     */
    public KomodoConnectionAttributes() {
        // do nothing
    }

    @Override
    @JsonIgnore
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    @JsonIgnore
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the ServiceCatalogSource
     */
    public String getServiceCatalogSource() {
        return this.serviceCatalogSource;
    }

    /**
     * @param serviceCatalogSource the ServiceCatalogSource to set
     */
    public void setServiceCatalogSource(String serviceCatalogSource) {
        this.serviceCatalogSource = serviceCatalogSource;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((serviceCatalogSource == null) ? 0 : serviceCatalogSource.hashCode());
        result = prime * result + ((description == null) ? 0 : description.hashCode());
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
        KomodoConnectionAttributes other = (KomodoConnectionAttributes)obj;
        if (serviceCatalogSource == null) {
            if (other.serviceCatalogSource != null)
                return false;
        } else if (!serviceCatalogSource.equals(other.serviceCatalogSource))
            return false;
        if (description == null) {
            if (other.description != null)
                return false;
        } else if (!description.equals(other.description))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoConnectionAttributes [description=" + this.description + ", ServiceCatalogSource=" + this.serviceCatalogSource + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
}
