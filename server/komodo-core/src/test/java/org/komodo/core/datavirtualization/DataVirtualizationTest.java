package org.komodo.core.datavirtualization;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.core.datavirtualization.DataVirtualization;
import org.komodo.core.datavirtualization.DataVirtualizationRepository;
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
    
    @Test
    public void testFindDeleteByName() {
        DataVirtualization dv = new DataVirtualization("foo");
        entityManager.persist(dv);
        entityManager.flush();
     
        DataVirtualization found = dataVirtualizationRepository.findByName(dv.getName());
     
        assertEquals(dv.getName(), found.getName());
        
        assertEquals(1, dataVirtualizationRepository.deleteByName(dv.getName()));
    }


}
