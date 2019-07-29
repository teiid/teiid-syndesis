package org.komodo.repository;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

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
    
    @Autowired
    private WorkspaceManagerImpl workspaceManagerImpl;
    
    @Test
    public void testFindDeleteByName() {
        ViewDefinition v = new ViewDefinition("x");
        v.setDdl("create ...");
        entityManager.persist(v);
        entityManager.flush();
     
        ViewDefinition found = viewEditorStateRepository.findByName(v.getName());
     
        assertEquals(v.getDdl(), found.getDdl());
        
        workspaceManagerImpl.addViewDefiniton("y");
        workspaceManagerImpl.addViewDefiniton("x1").setComplete(true);
        entityManager.flush();

        assertEquals(2, workspaceManagerImpl.getViewDefinitions("x").length);
    }
    
    @Test
    public void testState() {
    	ViewDefinition v = new ViewDefinition("exising");
        v.setDdl("create ...");
        v.addSourcePath("x");
        v.addSqlComposition("comp");
        v = viewEditorStateRepository.save(v);
        entityManager.flush();
        entityManager.detach(v);
    	
        ViewDefinition found = viewEditorStateRepository.findByName("exising");
        assertEquals(1, found.getSqlCompositions().size());
        assertEquals(Arrays.asList("x"), found.getSourcePaths());
    }

}
