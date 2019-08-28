package org.komodo.repository;

import static org.junit.Assert.*;

import javax.persistence.PersistenceException;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.datavirtualization.SourceSchema;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@DataJpaTest
public class SourceSchemaTest {

    @Autowired
    private TestEntityManager entityManager;

    @Autowired
    private WorkspaceManagerImpl workspaceManagerImpl;

    @Test
    public void testFindDeleteByName() {
        SourceSchema s = workspaceManagerImpl.createSchema("foo", "bar", "create ...");
        entityManager.flush();

        SourceSchema found = workspaceManagerImpl.findSchema(s.getId());
        assertEquals(s.getDdl(), found.getDdl());

        try {
            workspaceManagerImpl.createSchema("foo", "bar", "create ...");
            entityManager.flush();
            fail();
        } catch (DataIntegrityViolationException e) {
        }

        entityManager.clear();

        try {
            workspaceManagerImpl.createSchema("foo", "bar1", "create ...");
            entityManager.flush();
            fail();
        } catch (DataIntegrityViolationException | PersistenceException e) {
        }

        entityManager.clear();

        try {
            workspaceManagerImpl.createSchema("foo1", "baR", "create ...");
            entityManager.flush();
            fail();
        } catch (DataIntegrityViolationException | PersistenceException e) {
        }

        entityManager.clear();

        assertTrue(workspaceManagerImpl.deleteSchema(s.getId()));

        assertNull(workspaceManagerImpl.findSchema(s.getId()));

        assertFalse(workspaceManagerImpl.deleteSchema(s.getId()));

        entityManager.flush();
    }

}
