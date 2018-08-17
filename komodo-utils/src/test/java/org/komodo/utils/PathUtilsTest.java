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

import static org.junit.Assert.*;

import java.util.Properties;

import org.junit.Test;

/**
 * This test class provides coverage for the PathUtil.java class.
 * 
 * The Path string will be composed of key=value string pairs separated by forward slash ('/')
 *
 */
public class PathUtilsTest {

	@Test
	public void testGetOptions_4_Properties() {
		String path = "connection=pgConn/schema=public/table=product/type=TABLE";
		
		Properties props = PathUtils.getOptions(path);
		assertEquals(4, props.size());
		assertEquals("public", PathUtils.getOption(path, "schema"));
	}
	
	@Test
	public void testGetOption() {
		String path = "connection=pgConn/schema=public/table=product/type=TABLE";
		String prop = PathUtils.getOption(path, "connection");
		assertEquals("pgConn", prop);
	}
}
