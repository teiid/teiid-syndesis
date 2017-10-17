package org.komodo.core;
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


import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.core.internal.TestObjectOperations;
import org.komodo.core.internal.repository.search.TestObjectSearcher;
import org.komodo.core.internal.sequencer.TestSequencers;
import org.komodo.core.internal.sequencer.TestTeiidSqlSequencer;
import org.komodo.core.repository.ObjectImplTest;
import org.komodo.core.repository.TestLocalRepository;
import org.komodo.core.repository.TestLocalRepositoryPersistence;
import org.komodo.core.repository.TestMultiUsers;
import org.komodo.core.repository.validation.RuleFactoryTest;
import org.komodo.core.repository.validation.ValidationManagerImplTest;
import org.komodo.core.visitor.TestDdlNodeVisitor;
import org.komodo.core.visitor.TestDdlNodeVisitorIdentifiers;
import org.komodo.core.visitor.TestVdbExport;

@SuppressWarnings( "javadoc" )
@RunWith( Suite.class )
@Suite.SuiteClasses( {
    ObjectImplTest.class,
    TestLocalRepository.class,
    TestObjectSearcher.class,
    TestLocalRepositoryPersistence.class,
    ValidationManagerImplTest.class,
    RuleFactoryTest.class,
    TestObjectOperations.class,
    TestMultiUsers.class,
    TestVdbExport.class,
    TestDdlNodeVisitor.class,
    TestDdlNodeVisitorIdentifiers.class,
    TestTeiidSqlSequencer.class,
    TestSequencers.class
    } )
public class AllTests {
    // nothing to do
}
