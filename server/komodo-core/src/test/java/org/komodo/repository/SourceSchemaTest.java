package org.komodo.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

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
     
        org.komodo.datavirtualization.SourceSchema found = sourceSchemaRepository.findOne(s.getId());
     
        assertEquals(s.getDdl(), found.getDdl());
        
        workspaceManagerImpl.deleteSchema(s.getId());
        
        entityManager.flush();
        
        assertNull(sourceSchemaRepository.findOne(s.getId()));
    }


}
