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
package org.komodo.rest.relational.request;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.ws.rs.core.MediaType;

import org.komodo.rest.KRestEntity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;


/**
 * Object to be serialised by GSON that encapsulates properties for Dataservice single source request
 */
@JsonSerialize
@JsonInclude(value=Include.NON_NULL)
public class KomodoDataserviceSingleSourceAttributes implements KRestEntity {

    /**
     * Label for the DataService name
     */
    public static final String DATASERVICE_NAME_LABEL = "dataserviceName"; //$NON-NLS-1$

    /**
     * Label for the tablePath used for the update
     */
    public static final String DATASERVICE_TABLE_PATHS_LABEL = "tablePaths"; //$NON-NLS-1$

    /**
     * Label for the modelSourcePath used for the update
     */
    public static final String DATASERVICE_MODEL_SOURCE_PATH_LABEL = "modelSourcePath"; //$NON-NLS-1$

    /**
     * Label for the column names to include in the service view
     */
    public static final String DATASERVICE_COLUMN_NAMES_LABEL = "columnNames"; //$NON-NLS-1$

    /**
     * Label for the service view ddl
     */
    public static final String DATASERVICE_VIEW_DDL_LABEL = "viewDdl"; //$NON-NLS-1$

    @JsonProperty(DATASERVICE_NAME_LABEL)
    private String dataserviceName;

    @JsonProperty(DATASERVICE_TABLE_PATHS_LABEL)
    private List<String> tablePaths;

    @JsonProperty(DATASERVICE_COLUMN_NAMES_LABEL)
    private Map<String, List<String>> columnNames;

    @JsonProperty(DATASERVICE_MODEL_SOURCE_PATH_LABEL)
    private String modelSourcePath;

    @JsonProperty(DATASERVICE_VIEW_DDL_LABEL)
    private String viewDdl;

    /**
     * Default constructor for deserialization
     */
    public KomodoDataserviceSingleSourceAttributes() {
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
     * @return dataserviceName
     */
    public String getDataserviceName() {
        return dataserviceName;
    }

    /**
     * @param dataserviceName the dataservice name
     */
    public void setDataserviceName(String dataserviceName) {
        this.dataserviceName = dataserviceName;
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
     * @return model source path
     */
    public String getModelSourcePath() {
        return modelSourcePath;
    }

    /**
     * @param modelSourcePath the source model path
     */
    public void setModelSourcePath(String modelSourcePath) {
        this.modelSourcePath = modelSourcePath;
    }
    
    /**
     * @return the column names for each table
     */
    public Map<String,List<String>> getColumnNames() {
        if (columnNames == null)
            return Collections.emptyMap();

        return Collections.unmodifiableMap(this.columnNames);
    }

    /**
     * set the column names for each table
     * @param columnNames the column names
     */
    public void setColumnNames(Map<String,List<String>> columnNames) {
        if (this.columnNames == null)
            this.columnNames = new HashMap<>();

        this.columnNames.putAll(columnNames);
    }
    
    /**
     * @return view ddl
     */
    public String getViewDdl() {
        return viewDdl;
    }

    /**
     * @param ddl the view ddl
     */
    public void setViewDdl(String ddl) {
        this.viewDdl = ddl;
    }


    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((dataserviceName == null) ? 0 : dataserviceName.hashCode());
        result = prime * result + ((tablePaths == null) ? 0 : tablePaths.hashCode());
        result = prime * result + ((modelSourcePath == null) ? 0 : modelSourcePath.hashCode());
        result = prime * result + ((columnNames == null) ? 0 : columnNames.hashCode());
        result = prime * result + ((viewDdl == null) ? 0 : viewDdl.hashCode());
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
        KomodoDataserviceSingleSourceAttributes other = (KomodoDataserviceSingleSourceAttributes)obj;
        if (dataserviceName == null) {
            if (other.dataserviceName != null)
                return false;
        } else if (!dataserviceName.equals(other.dataserviceName))
            return false;
        if (tablePaths == null) {
            if (other.tablePaths != null)
                return false;
        } else if (!tablePaths.equals(other.tablePaths))
            return false;
        if (modelSourcePath == null) {
            if (other.modelSourcePath != null)
                return false;
        } else if (!modelSourcePath.equals(other.modelSourcePath))
            return false;
        if (columnNames == null) {
            if (other.columnNames != null)
                return false;
        } else if (!columnNames.equals(other.columnNames))
            return false;
        if (viewDdl == null) {
            if (other.viewDdl != null)
                return false;
        } else if (!viewDdl.equals(other.viewDdl))
            return false;
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KomodoDataserviceUpdateAttributes [dataserviceName=" + dataserviceName + ", modelSourcePath=" + modelSourcePath);
        if(tablePaths!=null) {
            sb.append(", tablePaths length =" + tablePaths.size());
        }
        if(columnNames!=null) {
            sb.append(", columnNames length =" + columnNames.size());
        }
        if(viewDdl!=null) {
            sb.append(", viewDdl =" + viewDdl);
        }
        sb.append("]");
        return sb.toString();
    }
}
