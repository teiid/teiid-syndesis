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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.ws.rs.core.MediaType;

import org.komodo.rest.KRestEntity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;


/**
 * Object to be serialised by GSON that encapsulates properties for ViewsInfo
 */
@JsonSerialize
@JsonInclude(value=Include.NON_NULL)
public class KomodoViewsInfo implements KRestEntity {

    /**
     * Label for the viewNames
     */
    public static final String VIEW_NAMES_LABEL = "viewNames"; //$NON-NLS-1$

    /**
     * Label for the tablePaths
     */
    public static final String TABLE_PATHS_LABEL = "tablePaths"; //$NON-NLS-1$

    /**
     * Label for the modelSourcePaths
     */
    public static final String MODEL_SOURCE_PATHS_LABEL = "modelSourcePaths"; //$NON-NLS-1$

    @JsonProperty(VIEW_NAMES_LABEL)
    private List<String> viewNames;

    @JsonProperty(TABLE_PATHS_LABEL)
    private List<String> tablePaths;

    @JsonProperty(MODEL_SOURCE_PATHS_LABEL)
    private List<String> modelSourcePaths;

    /**
     * Default constructor for deserialization
     */
    public KomodoViewsInfo() {
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
     * @return the view names
     */
    public List<String> getViewNames() {
        if (viewNames == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(this.viewNames);
    }

    /**
     * set the view names
     * @param viewNames the view names
     */
    public void setViewNames(List<String> viewNames) {
        if (this.viewNames == null)
            this.viewNames = new ArrayList<>();

        this.viewNames.addAll(viewNames);
    }
    
    /**
     * @return the table paths
     */
    public List<String> getTablePaths() {
        if (tablePaths == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(this.tablePaths);
    }

    /**
     * set table paths
     * @param tablePaths the table paths
     */
    public void setTablePaths(List<String> tablePaths) {
        if (this.tablePaths == null)
            this.tablePaths = new ArrayList<>();

        this.tablePaths.addAll(tablePaths);
    }

    /**
     * @return the table paths
     */
    public List<String> getModelSourcePaths() {
        if (modelSourcePaths == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(this.modelSourcePaths);
    }

    /**
     * set modelSource paths
     * @param modelSourcePaths the modelSource paths
     */
    public void setModelSourcePaths(List<String> modelSourcePaths) {
        if (this.modelSourcePaths == null)
            this.modelSourcePaths = new ArrayList<>();

        this.modelSourcePaths.addAll(modelSourcePaths);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((tablePaths == null) ? 0 : tablePaths.hashCode());
        result = prime * result + ((modelSourcePaths == null) ? 0 : modelSourcePaths.hashCode());
        result = prime * result + ((viewNames == null) ? 0 : viewNames.hashCode());
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
        KomodoViewsInfo other = (KomodoViewsInfo)obj;
        if (tablePaths == null) {
            if (other.tablePaths != null)
                return false;
        } else if (!tablePaths.equals(other.tablePaths))
            return false;
        if (modelSourcePaths == null) {
            if (other.modelSourcePaths != null)
                return false;
        } else if (!modelSourcePaths.equals(other.modelSourcePaths))
            return false;
        if (viewNames == null) {
            if (other.viewNames != null)
                return false;
        } else if (!viewNames.equals(other.viewNames))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KomodoViewsInfo [");
        if(viewNames!=null) {
            sb.append(", viewNames length =" + viewNames.size());
        }
        if(tablePaths!=null) {
            sb.append(", tablePaths length =" + tablePaths.size());
        }
        if(modelSourcePaths!=null) {
            sb.append(", modelSourcePaths length =" + modelSourcePaths.size());
        }
        sb.append("]");
        return sb.toString();
    }
}


