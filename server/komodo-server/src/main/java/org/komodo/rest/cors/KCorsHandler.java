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
package org.komodo.rest.cors;

import java.util.Set;

import org.komodo.StringConstants;

/**
 * Handler which intercepts / filters to resolve CORS-related issues
 * with client-server communication
 */
public interface KCorsHandler extends StringConstants {
    
    String ALLOW_HEADERS = "Content-Type, X-Requested-With, accept, Origin," + //$NON-NLS-1$
                                                     "Access-Control-Request-Method," + //$NON-NLS-1$
                                                     "Access-Control-Request-Headers, Authorization"; //$NON-NLS-1$

    String ALLOW_METHODS = "GET, POST, PUT, DELETE, OPTIONS, HEAD"; //$NON-NLS-1$

    /**
     * @return approved origins
     */
    Set<String> getAllowedOrigins();

    /**
     * @return true if credentials are allowed, false otherwise
     */
    boolean isAllowCredentials();

    /**
     * @param allowCredentials
     */
    void setAllowCredentials(boolean allowCredentials);

    /**
     * @return the Access-Control-Allow-Methods
     */
    String getAllowedMethods();

    /**
     * Comma delimited string for Access-Control-Allow-Methods
     *
     * @param allowedMethods
     */
    void setAllowedMethods(String allowedMethods);

    /**
     * @return the Access-Control-Allow-Headers
     */
    String getAllowedHeaders();

    /**
     * Comma delimited string for Access-Control-Allow-Headers
     *
     * @param allowedHeaders
     */
    void setAllowedHeaders(String allowedHeaders);

    /**
     * @return max age
     */
    int getCorsMaxAge();

    /**
     * @param corsMaxAge
     */
    void setCorsMaxAge(int corsMaxAge);

    /**
     * @return the exposed headers
     */
    String getExposedHeaders();

    /**
     * @param exposedHeaders
     */
    void setExposedHeaders(String exposedHeaders);
}
