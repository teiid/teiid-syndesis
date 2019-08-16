package org.komodo.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.datavirtualization.SourceSchema;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@DataJpaTest
public class SourceSchemaTest {

    @Autowired
    private TestEntityManager entityManager;

    @Autowired
    private SourceSchemaRepository sourceSchemaRepository;

    @Autowired
    private WorkspaceManagerImpl workspaceManagerImpl;

    @Test
    public void testFindDeleteByName() {
        SourceSchema s = new SourceSchema("foo");
        s.setName("bar");
        s.setDdl("create ...");
        entityManager.persist(s);
        entityManager.flush();
     
        org.komodo.datavirtualization.SourceSchema found = sourceSchemaRepository.findById(s.getId()).orElse(null);
        assertEquals(s.getDdl(), found.getDdl());

        try {
            workspaceManagerImpl.createOrUpdateSchema(s.getId(), "foo", "create something...");
        } catch (IllegalArgumentException e) {
            //don't allow the schema name to change
        }

        workspaceManagerImpl.createOrUpdateSchema(s.getId(), "bar", "create something...");

        assertTrue(workspaceManagerImpl.deleteSchema(s.getId()));

        assertNull(sourceSchemaRepository.findById(s.getId()).orElse(null));

        assertFalse(workspaceManagerImpl.deleteSchema(s.getId()));

        entityManager.flush();
    }

}
