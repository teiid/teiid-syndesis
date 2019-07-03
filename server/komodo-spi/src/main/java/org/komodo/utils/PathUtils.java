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
