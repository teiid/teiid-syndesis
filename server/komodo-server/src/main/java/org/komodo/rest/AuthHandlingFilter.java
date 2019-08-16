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
package org.komodo.rest;

import java.io.IOException;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.container.PreMatching;
import javax.ws.rs.ext.Provider;

//import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;
import org.komodo.utils.KLog;
import org.springframework.stereotype.Component;

@Provider
@PreMatching
@Component
public class AuthHandlingFilter implements ContainerRequestFilter, CredentialsProvider {

//    private static final Log LOGGER = LogFactory.getLog(AuthHandlingFilter.class);

    public static class AuthToken {
        private String token;

        public AuthToken(String token) {
            this.token = token;
        }

        @Override
        public String toString() {
            return token;
        }

        public String getHttpAuthentication() {
            return "Bearer " + toString();
        }
    }

    public static class OAuthCredentials {
        private AuthToken token;
        private String user;

        public OAuthCredentials(String token, String user) {
            this.token = new AuthToken(token);
            this.user = user;
        }

        public AuthToken getToken() {
            return token;
        }
        public String getUser() {
            return user;
        }
    }

    public static ThreadLocal<OAuthCredentials> threadOAuthCredentials  = new ThreadLocal<OAuthCredentials>();

    @Override
    public void filter(ContainerRequestContext requestContext) throws IOException {
        String accessToken = requestContext.getHeaderString("X-Forwarded-Access-Token");
        String user = requestContext.getHeaderString("X-Forwarded-User");
        if (KLog.getLogger().isTraceEnabled()) {
            KLog.getLogger().trace("URL =" + requestContext.getUriInfo());
            KLog.getLogger().trace("X-Forwarded-Access-Token = " + accessToken);
            KLog.getLogger().trace("X-Forwarded-User = " + user);
        }
        OAuthCredentials creds = new OAuthCredentials(accessToken, user);
        threadOAuthCredentials.set(creds);
//        LOGGER.info("  *** AuthHandlingFilter.filter() OAuth user = " + creds.user + "  Token = " + creds.getToken().toString());
    }

    @Override
    public OAuthCredentials getCredentials() {
        return threadOAuthCredentials.get();
    }

}
