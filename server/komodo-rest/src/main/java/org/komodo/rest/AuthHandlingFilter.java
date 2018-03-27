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
package org.komodo.rest;

import java.io.IOException;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.container.PreMatching;
import javax.ws.rs.ext.Provider;

import org.komodo.utils.KLog;

@Provider
@PreMatching
public class AuthHandlingFilter implements ContainerRequestFilter {

    public static class AuthToken {
        private String token;

        public AuthToken(String token) {
            this.token = token;
        }

        @Override
        public String toString() {
            return token;
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
		KLog.getLogger().trace("URL =" + requestContext.getUriInfo());
		KLog.getLogger().trace("X-Forwarded-Access-Token = " + accessToken);
		KLog.getLogger().trace("X-Forwarded-User = " + user);
		OAuthCredentials creds = new OAuthCredentials(accessToken, user);
		threadOAuthCredentials.set(creds);		
	}

}
