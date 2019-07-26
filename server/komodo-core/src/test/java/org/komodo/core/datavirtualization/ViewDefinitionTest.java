package org.komodo.core.datavirtualization;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@DataJpaTest
public class ViewDefinitionTest {
	
	@Autowired
    private TestEntityManager entityManager;
 
    @Autowired
    private ViewDefinitionRepository viewEditorStateRepository;
    
    @Test
    public void testFindDeleteByName() {
        ViewDefinition v = new ViewDefinition("x");
        v.setDdl("create ...");
        entityManager.persist(v);
        entityManager.flush();
     
        ViewDefinition found = viewEditorStateRepository.findByName(v.getName());
     
        assertEquals(v.getDdl(), found.getDdl());
        
        WorkspaceManagerImpl workspaceManagerImpl = new WorkspaceManagerImpl();
        workspaceManagerImpl.setViewEditorStateRepository(viewEditorStateRepository);
        
        workspaceManagerImpl.addViewDefiniton("y");
        workspaceManagerImpl.addViewDefiniton("x1");
        entityManager.flush();

        assertEquals(2, workspaceManagerImpl.getViewDefinitions("x").length);
    }

}
