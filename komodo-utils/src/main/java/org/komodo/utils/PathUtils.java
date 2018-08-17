/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.utils;

import java.util.Properties;
import java.util.StringTokenizer;

/**
 * This is a common place to put Path utility methods
 *
 *  The Path string will be composed of key=value string pairs separated by forward slash ('/')
 */
public class PathUtils {
	/**
	 * The String "/"
	 */
	public static final String OPTION_SEPARATOR = "/"; //$NON-NLS-1$
	
	/**
	 * The String "="
	 */
	public static final String VALUE_SEPARATOR = "="; //$NON-NLS-1$
	
	/**
	 * Simple method parses the input path and returns a set of string {@link Properties}
	 * @param path
	 * @return properties object
	 */
	public static Properties getOptions(String path) {
		StringTokenizer tokenizer = new StringTokenizer(path, OPTION_SEPARATOR);
		
		Properties props = new Properties();
		
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            
            // Now we split this token via the "="
            StringTokenizer strTkzr = new StringTokenizer(token, VALUE_SEPARATOR);
            String key = strTkzr.nextToken();
            String value = strTkzr.nextToken();
            props.setProperty(key , value);
        }
        
        return props;

	}
	
	/**
	 * Simple method returns 
	 * @param path
	 * @param key
	 * @return String property value - may be null
	 */
	public static String getOption(String path, String key) {
		Properties props = getOptions(path);
		
		return props.getProperty(key);
	}
	
	/**
	 * Simple method returns the Table Option segments of the full path
	 *  EXAMPLE:  PATH = >>   connection=pgconnection1/schema=public/table=orders
	 *    TABLE OPTION = >>>  schema=public/table=orders 
	 * @param path
	 * @return String property value - may be null
	 */
	public static String getTableOption(String path) {
		StringTokenizer tokenizer = new StringTokenizer(path, OPTION_SEPARATOR);
		
		StringBuilder sb = new StringBuilder();
		int i=0;
        while (tokenizer.hasMoreTokens()) {
        	if( i > 1 ) sb.append(OPTION_SEPARATOR);
        	
            String token = tokenizer.nextToken();
            if( i > 0 ) {
	            // Now we split this token via the "="
	            StringTokenizer strTkzr = new StringTokenizer(token, VALUE_SEPARATOR);
	            String key = strTkzr.nextToken();
	            String value = strTkzr.nextToken();
	            sb.append((String)key).append('=').append(value);
            }
            i++;
        }

		return sb.toString();
	}
}
