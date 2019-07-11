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
package org.komodo.core.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.core.KomodoLexicon;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoObjectVisitor;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.OperationType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.SynchronousCallback;
import org.komodo.spi.repository.UnitOfWork;
import org.komodo.spi.repository.UnitOfWork.State;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class TestMultiUsers extends AbstractLocalRepositoryTest {

    private static final String ALICE = "alice";

    private static final String BOB = "bob";

    private UnitOfWork aliceTx;

    private UnitOfWork bobTx;

    private UnitOfWork writeTx(String user) throws Exception {
        SynchronousCallback callback = new TestTransactionListener();
        return createTransaction(user, txId(user, "tx"), false, callback);
    }

    @Before
    public void createUserTransactions() throws Exception {
        this.aliceTx = writeTx(ALICE);
        this.bobTx = writeTx(BOB);
    }

    @After
    public void cleanupUserTransactions() throws Exception {
        { // process current transactions if necessary
            if ( this.aliceTx != null ) {
                switch ( this.aliceTx.getState() ) {
                    case NOT_STARTED:
                    case RUNNING:
                        this.aliceTx.rollback();
                        break;
                    case COMMITTED:
                    case ERROR:
                    case ROLLED_BACK:
                    default:
                        break;
                }

                this.aliceTx = null;
            }
        }

        { // process current transactions if necessary
            if ( this.bobTx != null ) {
                switch ( this.bobTx.getState() ) {
                    case NOT_STARTED:
                    case RUNNING:
                        this.bobTx.rollback();
                        break;
                    case COMMITTED:
                    case ERROR:
                    case ROLLED_BACK:
                    default:
                        break;
                }

                this.bobTx = null;
            }
        }
    }

    private KomodoObject userWorkspace(String user) throws Exception {
        UnitOfWork userTx = writeTx(user);
        KomodoObject wksp = _repo.komodoWorkspace(userTx);
        commit(userTx, State.COMMITTED);
        return wksp;
    }

    private KomodoObject add(String user, String objName, String type) throws Exception {
        UnitOfWork userTx = writeTx(user);
        KomodoObject newObj = _repo.add(userTx, RepositoryImpl.komodoWorkspacePath(userTx), objName, type);
        commit(userTx, State.COMMITTED);

        return newObj;
    }

    private ObjectImpl convert(KomodoObject obj) throws Exception {
        return new ObjectImpl(((ObjectImpl)obj).getRepository(), obj.getAbsolutePath(), obj.getIndex());
    }

    @Test
    public void shouldCreateUserHomes() throws Exception {
        userWorkspace(ALICE);
        userWorkspace(BOB);
    }

    @Test
    public void shouldRestrictSearchesToUserDirectory() throws Exception {
        userWorkspace(ALICE);
        userWorkspace(BOB);

        add(ALICE, "vdb1", VdbLexicon.Vdb.VIRTUAL_DATABASE);
        add(ALICE, "vdb2", VdbLexicon.Vdb.VIRTUAL_DATABASE);

        add(BOB, "vdb1", VdbLexicon.Vdb.VIRTUAL_DATABASE);
        add(BOB, "vdb2", VdbLexicon.Vdb.VIRTUAL_DATABASE);
        add(BOB, "vdb3", VdbLexicon.Vdb.VIRTUAL_DATABASE);
        add(BOB, "vdb4", VdbLexicon.Vdb.VIRTUAL_DATABASE);
    }

    @Test
    public void usersCannotAddToEachOthersWorkspace() throws Exception {
        userWorkspace(ALICE);
        userWorkspace(BOB);

        String bobsHome = RepositoryImpl.komodoWorkspacePath(bobTx);
        String alicesHome = RepositoryImpl.komodoWorkspacePath(aliceTx);

        try {
            _repo.add(aliceTx, bobsHome, "vdb1", VdbLexicon.Vdb.VIRTUAL_DATABASE);
            fail("Alice should not be allowed to add stuff to bob's workspace");
        } catch (Exception ex) {
            assertEquals(
                          "Adding or removing children to the object at path \"" +
                              bobsHome + "\" is not allowed for the user \"" + ALICE + "\"",
                         ex.getMessage());
        }

        try {
            _repo.add(bobTx, alicesHome, "vdb1", VdbLexicon.Vdb.VIRTUAL_DATABASE);
            fail("Bob should not be allowed to add stuff to alice's workspace");
        } catch (Exception ex) {
            assertEquals(
                         "Adding or removing children to the object at path \"" +
                             alicesHome + "\" is not allowed for the user \"" + BOB + "\"",
                         ex.getMessage());
        }
    }

    @Test
    public void usersCannotRemoveObjectsFromEachOthersWorkspace() throws Exception {
        userWorkspace(ALICE);
        userWorkspace(BOB);

        add(ALICE, "vdb1", VdbLexicon.Vdb.VIRTUAL_DATABASE);
        add(BOB, "vdb1", VdbLexicon.Vdb.VIRTUAL_DATABASE);

        UnitOfWork aliceTx = writeTx(ALICE);
        UnitOfWork bobTx = writeTx(BOB);

        String bobsHome = RepositoryImpl.komodoWorkspacePath(bobTx);
        String alicesHome = RepositoryImpl.komodoWorkspacePath(aliceTx);
        String bobVdbPath = bobsHome + FORWARD_SLASH + "vdb1";
        String aliceVdbPath = alicesHome + FORWARD_SLASH + "vdb1";

        try {            
            _repo.remove(aliceTx, bobVdbPath);
            fail("Alice should not be allowed to remove stuff from bob's workspace");
        } catch (Exception ex) {
            assertEquals(
                         "The object at path \"" + bobVdbPath + "\" is inaccessible for the user \"" + ALICE + "\"",
                         ex.getMessage());
        }

        try {
            _repo.remove(bobTx, aliceVdbPath);
            fail("Bob should not be allowed to remove stuff from alice's workspace");
        } catch (Exception ex) {
            assertEquals(
                         "The object at path \"" + aliceVdbPath + "\" is inaccessible for the user \"" + BOB + "\"",
                         ex.getMessage());
        }
    }

    @Test
    public void usersCannotUseOtherUsersObjects() throws Exception {
        userWorkspace(ALICE);
        userWorkspace(BOB);

        String bobVdbName = "bobVdb";
        String bobModelName = "bobModel1";

        ObjectImpl bobVdb = convert(add(BOB, bobVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertNotNull(bobVdb);
        ObjectImpl bobModel = convert(add(BOB, bobModelName, VdbLexicon.Vdb.DECLARATIVE_MODEL));
        assertNotNull(bobModel);
       
        UnitOfWork aliceTx = writeTx(ALICE);

        String expReadErrorMsg = "The object at path \"" + bobVdb.getAbsolutePath() + "\" is inaccessible for the user \"" + ALICE + "\"";
        String expChildrenErrorMsg = "Adding or removing children to the object at path \"" + bobVdb.getAbsolutePath() + "\" is not allowed for the user \"" + ALICE + "\"";
        String expRemoveErrorMsg = "Removing object at path \"" + bobVdb.getAbsolutePath() + "\" is not allowed for the user \"" + ALICE + "\"";
        String expPropertySetErrorMsg = "Properties are not allowed to be set on the object at path \"" + bobVdb.getAbsolutePath() + "\" by the user \"" + ALICE + "\"";

        String failMsg = "Alice SHOULD NOT have permission to use bob's vdb";

        //
        // Adding a child
        //
        try {
            bobVdb.addChild(aliceTx, bobModelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expChildrenErrorMsg,
                         ex.getMessage());
        }

        //
        // Adding a descriptor
        //
        try {
            bobVdb.addDescriptor(aliceTx, "myDescriptor");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expPropertySetErrorMsg,
                         ex.getMessage());
        }

        //
        // get a child
        //
        try {
            bobVdb.getChild(aliceTx, bobModelName);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get a child
        //
        try {
            bobVdb.getChild(aliceTx, bobModelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get children
        //
        try {
            bobVdb.getChildren(aliceTx, bobModelName);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get children of type
        //
        try {
            bobVdb.getChildrenOfType(aliceTx, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get descriptor
        //
        try {
            bobVdb.getDescriptor(aliceTx, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get descriptors
        //
        try {
            bobVdb.getDescriptors(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get name
        //
        try {
            bobVdb.getName(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get parent
        //
        try {
            bobVdb.getParent(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get primary type
        //
        try {
            bobVdb.getPrimaryType(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get property
        //
        try {
            bobVdb.getProperty(aliceTx, "aproperty");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get property descriptor
        //
        try {
            bobVdb.getPropertyDescriptor(aliceTx, "aproperty");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get property descriptors
        //
        try {
            bobVdb.getPropertyDescriptors(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get property names
        //
        try {
            bobVdb.getPropertyNames(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // get type identifier
        //
        try {
            bobVdb.getTypeIdentifier(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // has child
        //
        try {
            bobVdb.hasChild(aliceTx, "achild");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // has child
        //
        try {
            bobVdb.hasChild(aliceTx, "achild", VdbLexicon.Vdb.DECLARATIVE_MODEL);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // has children
        //
        try {
            bobVdb.hasChildren(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // has descriptor
        //
        try {
            bobVdb.hasDescriptor(aliceTx, "descriptorName");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // has properties
        //
        try {
            bobVdb.hasProperties(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // has property
        //
        try {
            bobVdb.hasProperty(aliceTx, "propertyName");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // print
        //
        try {
            bobVdb.print(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }

        //
        // remove
        //
        try {
            bobVdb.remove(aliceTx);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expRemoveErrorMsg,
                         ex.getMessage());
        }

        //
        // remove child
        //
        try {
            bobVdb.removeChild(aliceTx, bobModelName);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expChildrenErrorMsg,
                         ex.getMessage());
        }

        //
        // remove descriptor
        //
        try {
            bobVdb.removeDescriptor(aliceTx, VdbLexicon.Vdb.DECLARATIVE_MODEL);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expPropertySetErrorMsg,
                         ex.getMessage());
        }

        //
        // rename
        //
        try {
            bobVdb.rename(aliceTx, "newname");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expPropertySetErrorMsg,
                         ex.getMessage());
        }

        //
        // set primary type
        //
        try {
            bobVdb.setPrimaryType(aliceTx, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expPropertySetErrorMsg,
                         ex.getMessage());
        }

        //
        // set property
        //
        try {
            bobVdb.setProperty(aliceTx, "property1", "value1");
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expPropertySetErrorMsg,
                         ex.getMessage());
        }

        //
        // accept
        //
        try {
            KomodoObjectVisitor visitor = new KomodoObjectVisitor() {

                @Override
                public OperationType getRequestType() {
                    return OperationType.READ_OPERATION;
                }

                @Override
                public Object visit(UnitOfWork transaction, KomodoObject object) throws Exception {
                    fail("Should never visit here!");
                    return null;
                }
            };

            bobVdb.accept(aliceTx, visitor);
            fail(failMsg);
        } catch (Exception ex) {
            assertEquals(
                         expReadErrorMsg,
                         ex.getMessage());
        }
    }

    @Test
    public void sysCanUseOtherUsersObjects() throws Exception {
        userWorkspace(ALICE);
        KomodoObject bobWksp = userWorkspace(BOB);

        String bobVdbName = "bobVdb";
        String bobModelName = "bobModel1";
        String nodeType = "mix:created";
        String property = JcrConstants.JCR_PRIMARY_TYPE;

        KomodoObject bobVdbObject = add(BOB, bobVdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        ObjectImpl bobVdb = convert(bobVdbObject);
        assertNotNull(bobVdb);
        ObjectImpl bobModel = convert(add(BOB, bobModelName, VdbLexicon.Vdb.DECLARATIVE_MODEL));
        assertNotNull(bobModel);

        UnitOfWork sysTx = sysTx();

        //
        // Adding a child
        //
        KomodoObject child = bobVdb.addChild(sysTx, bobModelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        assertNotNull(child);

        //
        // Adding a descriptor
        //
        bobVdb.addDescriptor(sysTx, nodeType);
        assertNotNull(bobVdbObject.getDescriptor(sysTx, nodeType));

        //
        // get a child
        //
        KomodoObject bobModelObject = bobVdb.getChild(sysTx, bobModelName);
        assertNotNull(bobModelObject);

        //
        // get a child
        //
        bobModelObject = bobVdb.getChild(sysTx, bobModelName, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        assertNotNull(bobModelObject);

        //
        // get children
        //
        KomodoObject[] bobModels = bobVdb.getChildren(sysTx, bobModelName);
        assertNotNull(bobModels);
        assertEquals(1, bobModels.length);

        //
        // get children of type
        //
        bobModels = bobVdb.getChildrenOfType(sysTx, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        assertNotNull(bobModels);
        assertEquals(1, bobModels.length);

        //
        // get descriptor
        //
        Descriptor descriptor = bobVdb.getDescriptor(sysTx, nodeType);
        assertNotNull(descriptor);

        //
        // get descriptors
        //
        Descriptor[] descriptors = bobVdb.getDescriptors(sysTx);
        assertNotNull(descriptors);
        assertTrue(descriptors.length > 0);

        //
        // get name
        //
        String name = bobVdb.getName(sysTx);
        assertEquals(bobVdbName, name);

        //
        // get parent
        //
        KomodoObject parent = bobVdb.getParent(sysTx);
        assertNotNull(parent);
        assertEquals(KomodoLexicon.Home.NODE_TYPE, parent.getPrimaryType(sysTx).getName());

        //
        // get primary type
        //
        Descriptor primaryType = bobVdb.getPrimaryType(sysTx);
        assertNotNull(primaryType);
        assertEquals(VdbLexicon.Vdb.VIRTUAL_DATABASE, primaryType.getName());

        //
        // get property names
        //
        String[] propertyNames = bobVdb.getPropertyNames(sysTx);
        assertNotNull(propertyNames);
        assertTrue(propertyNames.length > 0);

        //
        // get property descriptors
        //
        PropertyDescriptor[] propertyDescriptors = bobVdb.getPropertyDescriptors(sysTx);
        assertNotNull(propertyDescriptors);
        assertTrue(propertyDescriptors.length > 0);

        //
        // get property
        //
        assertNotNull(bobVdb.getProperty(sysTx, property));

        //
        // get property descriptor
        //
        assertNotNull(bobVdb.getPropertyDescriptor(sysTx, property));

        //
        // get type identifier
        //
        KomodoType type = bobVdb.getTypeIdentifier(sysTx);
        assertEquals(KomodoType.VDB, type);

        //
        // has child
        //
        assertTrue(bobVdb.hasChild(sysTx, bobModelName));

        //
        // has child
        //
        assertTrue(bobVdb.hasChild(sysTx, bobModelName, VdbLexicon.Vdb.DECLARATIVE_MODEL));

        //
        // has children
        //
        assertTrue(bobVdb.hasChildren(sysTx));

        //
        // has descriptor
        //
        assertTrue(bobVdb.hasDescriptor(sysTx, nodeType));

        //
        // has properties
        //
        assertTrue(bobVdb.hasProperties(sysTx));

        //
        // has property
        //
        assertTrue(bobVdb.hasProperty(sysTx, property));

        //
        // print
        //
        bobVdb.print(sysTx);

        //
        // set primary type
        //
        bobVdb.setPrimaryType(sysTx, VdbLexicon.Vdb.VIRTUAL_DATABASE);

        //
        // set property
        //
        bobVdb.setProperty(sysTx, "property1", "value1");

        //
        // accept
        //
        final boolean[] accept = new boolean[1];
        KomodoObjectVisitor visitor = new KomodoObjectVisitor() {

            @Override
            public OperationType getRequestType() {
                return OperationType.READ_OPERATION;
            }

            @Override
            public Object visit(UnitOfWork transaction, KomodoObject object) throws Exception {
                accept[0] = true;
                return 0;
            }
        };

        bobVdb.accept(sysTx, visitor);
        assertTrue(accept[0]);

        //
        // remove descriptor
        //
        bobVdb.removeDescriptor(sysTx, nodeType);

        //
        // remove child
        //
        bobVdb.removeChild(sysTx, bobModelName);
        assertFalse(bobVdbObject.hasChild(sysTx, bobModelName));

        //
        // rename
        //
        String newNameExt = "1";
        bobVdb.rename(sysTx, bobVdbName + newNameExt);
        assertTrue(bobWksp.hasChild(sysTx, bobVdbName + newNameExt));

        //
        // remove
        //
        bobVdb.remove(sysTx);
        assertFalse(bobWksp.hasChild(sysTx, bobVdbName + newNameExt));
    }
}
