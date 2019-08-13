package org.komodo.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.WorkspaceManager.EntityNotFoundException;
import org.komodo.datavirtualization.DataVirtualization;
import org.komodo.datavirtualization.ViewDefinition;
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
    private WorkspaceManagerImpl workspaceManagerImpl;
    
    @Test
    public void testFindDeleteByName() throws Exception {
    	DataVirtualization dv = workspaceManagerImpl.createDataVirtualization("name");
    	
        ViewDefinition v = workspaceManagerImpl.createViewDefiniton(dv.getName(), "x");
        v.setDdl("create ...");
        
        entityManager.flush();
     
        ViewDefinition found = workspaceManagerImpl.findViewDefinition(v.getId());
     
        assertEquals(v.getDdl(), found.getDdl());
        
        workspaceManagerImpl.createViewDefiniton(dv.getName(), "y");
        
        workspaceManagerImpl.createViewDefiniton(dv.getName(), "x1").setComplete(true);
        
        entityManager.flush();
        
        assertEquals(3, workspaceManagerImpl.getViewDefinitions(dv.getName()).size());
        
        assertEquals(Arrays.asList("x", "y", "x1"), workspaceManagerImpl.getViewDefinitionsNames(dv.getName()));
        
        //x matching ignore case
        assertNotNull(workspaceManagerImpl.findViewDefinitionByNameIgnoreCase(dv.getName(), "X"));
        
        assertTrue(workspaceManagerImpl.deleteViewDefinition(v.getId()));
        
        workspaceManagerImpl.createViewDefiniton(dv.getName(), v.getName());
        
        entityManager.flush();
    }
    
    @Test
    public void testState() throws EntityNotFoundException {
    	DataVirtualization dv = workspaceManagerImpl.createDataVirtualization("name");
    	
    	ViewDefinition v = workspaceManagerImpl.createViewDefiniton(dv.getName(), "existing");
    	
        v.setDdl("create ...");
        v.addSourcePath("x");
        
        entityManager.flush();
        entityManager.detach(v);
    	
        ViewDefinition found = workspaceManagerImpl.findViewDefinition(v.getId());
        assertEquals("create ...", found.getDdl());
        assertEquals(Arrays.asList("x"), found.getSourcePaths());
    }

}
