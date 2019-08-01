package org.komodo.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@DataJpaTest
public class DataVirtualizationTest {
	
	@Autowired
    private TestEntityManager entityManager;
 
    @Autowired
    private DataVirtualizationRepository dataVirtualizationRepository;
    
    @Autowired
    private WorkspaceManagerImpl workspaceManagerImpl;
    
    @Test
    public void testFindDeleteByName() {
        DataVirtualization dv = new DataVirtualization("foo");
        entityManager.persist(dv);
        entityManager.flush();
     
        DataVirtualization found = dataVirtualizationRepository.findByName(dv.getName());
        
        assertNotNull(dataVirtualizationRepository.findByNameIgnoreCase(dv.getName().toUpperCase()));
        
        assertNotNull(found.getId());
     
        assertEquals(dv.getName(), found.getName());
        
        assertTrue(workspaceManagerImpl.deleteDataVirtualization(dv.getName()));
        
        entityManager.flush();
    }


}
