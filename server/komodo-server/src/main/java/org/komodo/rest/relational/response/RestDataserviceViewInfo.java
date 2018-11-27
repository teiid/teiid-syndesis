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
package org.komodo.rest.relational.response;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.relational.ViewBuilderCriteriaPredicate;
import org.komodo.rest.KRestEntity;

/**
 * A Dataservice View info object that can be used by GSON to build a JSON document representation.
 */
public final class RestDataserviceViewInfo implements KRestEntity {

    public static final String DDL_INFO = "DDL";
    public static final String LH_TABLE_INFO = "LHTABLE";
    public static final String RH_TABLE_INFO = "RHTABLE";
    public static final String CRITERIA_INFO = "CRITERIA";
    public static final String JOIN_INNER = "INNER";
    public static final String JOIN_LEFT_OUTER = "LEFT_OUTER";
    public static final String JOIN_RIGHT_OUTER = "RIGHT_OUTER";
    public static final String JOIN_FULL_OUTER = "FULL_OUTER";
    
    
    private String infoType;
    private String sourceVdbName;
    private String tableName;
    private List<String> columnNames;
    private String joinType;
    private List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>();
    private String viewDdl;
    private boolean viewEditable;
    
    /**
     * Constructor for use when deserializing
     */
    public RestDataserviceViewInfo() {
        super();
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    /**
     * @return the info type
     */
    public String getInfoType() {
        return infoType;
    }

    /**
     * @param type the info type
     */
    public void setInfoType(String type) {
        this.infoType = type;
    }
    
    /**
     * @return the view ddl
     */
    public String getViewDdl() {
        return viewDdl;
    }

    /**
     * @param ddl the View DDL
     */
    public void setViewDdl(String ddl) {
        this.viewDdl = ddl;
    }
    
    /**
     * @return 'true' if editor can recognize view
     */
    public boolean isViewEditable() {
        return viewEditable;
    }

    /**
     * @param canEdit 'true' if the editor can recognize the view
     */
    public void setViewEditable(boolean canEdit) {
        this.viewEditable = canEdit;
    }
    
    /**
     * @return the table name
     */
    public String getSourceVdbName() {
        return sourceVdbName;
    }

    /**
     * @param name the table name
     */
    public void setSourceVdbName(String name) {
        this.sourceVdbName = name;
    }
    
    /**
     * @return the table name
     */
    public String getTableName() {
        return tableName;
    }

    /**
     * @param name the table name
     */
    public void setTableName(String name) {
        this.tableName = name;
    }
    
    /**
     * @return the list of column names for the table
     */
    public String[] getColumnNames() {
        return columnNames != null ? (String[]) columnNames.toArray(new String[0]) : EMPTY_ARRAY;
    }

    /**
     * @param colNames the column names for a table
     */
    public void setColumnNames(Collection<String> colNames) {
        if (colNames == null || colNames.size() == 0)
            this.columnNames = Collections.emptyList();

        this.columnNames = new ArrayList<String>();
        for (Object value: colNames) {
            this.columnNames.add(value.toString());
        }
    }

    /**
     * @return the join type
     */
    public String getJoinType() {
        return joinType;
    }

    /**
     * @param joinType the join type
     */
    public void setJoinType(String joinType) {
        this.joinType = joinType;
    }
    
    /**
     * @return the criteria predicates
     */
    public List<ViewBuilderCriteriaPredicate> getCriteriaPredicates() {
        return this.criteriaPredicates;
    }

    /**
     * @param predicates the criteria predicates
     */
    public void setCriteriaPredicates(List<ViewBuilderCriteriaPredicate> predicates) {
        this.criteriaPredicates = predicates;
    }
    
    
}
