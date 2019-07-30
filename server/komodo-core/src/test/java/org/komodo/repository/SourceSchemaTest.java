package org.komodo.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
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
        s.setDdl("create ...");
        entityManager.persist(s);
        entityManager.flush();
     
        org.komodo.datavirtualization.SourceSchema found = sourceSchemaRepository.findByName(s.getName());
     
        assertEquals(s.getDdl(), found.getDdl());
        
        assertTrue(workspaceManagerImpl.deleteSchema(s.getName()));
        
        entityManager.flush();
    }


}
