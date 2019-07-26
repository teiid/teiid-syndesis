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
public class ViewEditorStateTest {
	
	@Autowired
    private TestEntityManager entityManager;
 
    @Autowired
    private ViewEditorStateRepository viewEditorStateRepository;
    
    @Test
    public void testFindDeleteByName() {
        ViewEditorState v = new ViewEditorState("x");
        v.setDdl("create ...");
        entityManager.persist(v);
        entityManager.flush();
     
        ViewEditorState found = viewEditorStateRepository.findByName(v.getName());
     
        assertEquals(v.getDdl(), found.getDdl());
        
        WorkspaceManagerImpl workspaceManagerImpl = new WorkspaceManagerImpl();
        workspaceManagerImpl.setViewEditorStateRepository(viewEditorStateRepository);
        
        workspaceManagerImpl.addViewEditorState("y");
        workspaceManagerImpl.addViewEditorState("x1");
        entityManager.flush();

        assertEquals(2, workspaceManagerImpl.getViewEditorStates("x").length);
    }

}
