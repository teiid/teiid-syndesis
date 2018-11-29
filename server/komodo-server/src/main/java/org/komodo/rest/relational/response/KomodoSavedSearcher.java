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
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.utils.ArgCheck;


/**
 * Object to be serialised by GSON that encapsulates a search object
 */
public class KomodoSavedSearcher implements KRestEntity {

    /**
     * Label for the name
     */
    public static final String NAME_LABEL = "name"; //$NON-NLS-1$

    /**
     * Label for the query
     */
    public static final String QUERY_LABEL = "query"; //$NON-NLS-1$

    /**
     * Label for the parameters
     */
    public static final String PARAMETER_LABEL = "parameters"; //$NON-NLS-1$

    private String name;

    private String query;

    private List<String> parameters;

    /**
     * Default constructor for deserialization
     */
    public KomodoSavedSearcher() {
        // do nothing
    }

    /**
     * @param name the name of this search
     *
     */
    public KomodoSavedSearcher(String name) {
        ArgCheck.isNotNull(name);
        this.name = name;
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return the query
     */
    public String getQuery() {
        return this.query;
    }

    /**
     * @param query the query to set
     */
    public void setQuery(String query) {
        this.query = query;
    }

    /**
     * @return the parameters
     */
    public List<String> getParameters() {
        return this.parameters;
    }

    /**
     * Set the parameters of the object
     *
     * @param parameters the parameters
     */
    public void setParameters(Collection<String> parameters) {
        if (parameters == null || parameters.isEmpty())
            return;

        this.parameters = new ArrayList<String>(parameters);
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.query == null) ? 0 : this.query.hashCode());
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
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
        KomodoSavedSearcher other = (KomodoSavedSearcher)obj;
        if (this.query == null) {
            if (other.query != null)
                return false;
        } else
            if (!this.query.equals(other.query))
                return false;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "KomodoSearchObject [name=" + this.name + ", query=" + this.query + "]";
    }
}
