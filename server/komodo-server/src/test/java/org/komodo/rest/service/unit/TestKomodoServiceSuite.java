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
package org.komodo.rest.service.unit;

import org.junit.ClassRule;
import org.junit.rules.RuleChain;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.komodo.rest.service.ServiceResources;

@RunWith(Suite.class)
@SuiteClasses({
    TestKomodoConnectionService.class,
    TestKomodoDataserviceService.class,
    TestKomodoDriverService.class,
    TestKomodoImportExportService.class,
    TestKomodoSearchService.class,
    TestKomodoUtilService.class,
    TestKomodoVdbService.class
    })
public class TestKomodoServiceSuite {

    @ClassRule
    public static RuleChain chain = RuleChain
                           .outerRule(ServiceResources.getInstance())
                           .around(UnitServiceResources.getInstance());
}