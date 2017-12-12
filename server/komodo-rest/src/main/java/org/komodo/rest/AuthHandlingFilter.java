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

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;


public class AuthHandlingFilter implements Filter {
	
	public static class OAuthCredentials {
		private String token;
		private String user;
		
		public OAuthCredentials(String token, String user) {
			this.token = token;
			this.user = user;
		}
		
		public String getToken() {
			return token;
		}
		public String getUser() {
			return user;
		}
	}
	
	public static ThreadLocal<OAuthCredentials> threadOAuthCredentials  = new ThreadLocal<OAuthCredentials>();

	@Override
	public void init(FilterConfig filterConfig) throws ServletException {
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
			throws IOException, ServletException {
		HttpServletRequest req = (HttpServletRequest)request;
		String accessToken = req.getHeader("X-Forwarded-Access-Token");
		String user = req.getHeader("X-Forwarded-User");
		System.out.println("URL =" + req.getRequestURI());
		System.out.println("X-Forwarded-Access-Token = " + accessToken);
		System.out.println("X-Forwarded-User = " + user);
		OAuthCredentials creds = new OAuthCredentials(accessToken, user);
		threadOAuthCredentials.set(creds);
		chain.doFilter(request, response);
	}

	@Override
	public void destroy() {
	}

}
