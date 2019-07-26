package org.komodo.core.datavirtualization;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.core.datavirtualization.SourceSchema;
import org.komodo.core.datavirtualization.SourceSchemaRepository;
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
    
    @Test
    public void testFindDeleteByName() {
        SourceSchema s = new SourceSchema("foo");
        s.setDdl("create ...");
        entityManager.persist(s);
        entityManager.flush();
     
        org.komodo.relational.dataservice.SourceSchema found = sourceSchemaRepository.findByName(s.getName());
     
        assertEquals(s.getDdl(), found.getDdl());
        
        assertEquals(1, sourceSchemaRepository.deleteByName(s.getName()));
    }


}
