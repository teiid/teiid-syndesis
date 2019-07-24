/*
 * Copyright Red Hat, Inc. and/or its affiliates
 * and other contributors as indicated by the @author tags and
 * the COPYRIGHT.txt file distributed with this work.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.relational;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.core.repository.KomodoObject;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.workspace.WorkspaceManagerImpl;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public class RelationalModelTest extends AbstractLocalRepositoryTest {

    protected static final String VDB_PATH = "/vdb/path/vdb.vdb";

    protected DataserviceImpl createDataservice() throws Exception {
        return createDataservice( getDefaultDataserviceName() );
    }

    protected DataserviceImpl createDataservice( final String serviceName ) throws Exception {
        return createDataservice( serviceName, null );
    }

    protected DataserviceImpl createDataservice( final String serviceName,
                                             final KomodoObject parent ) throws Exception {
        final WorkspaceManagerImpl mgr = WorkspaceManagerImpl.getInstance(_repo, getTransaction());
        final DataserviceImpl ds = mgr.createDataservice( getTransaction(), parent, serviceName );

        assertThat( ds.getPrimaryType( ).getName(), is( DataVirtLexicon.DataService.NODE_TYPE ) );
        assertThat( ds.getName( ), is( serviceName ) );
        return ds;
    }

    protected String getDefaultModelName() {
        return ( this.name.getMethodName() + "-Model" );
    }

    protected String getDefaultTableName() {
        return ( this.name.getMethodName() + "-Table" );
    }

    protected String getDefaultVdbName() {
        return ( this.name.getMethodName() + "-Vdb" );
    }

    protected String getDefaultSchemaName() {
        return ( this.name.getMethodName() + "-Schema" );
    }

    protected String getDefaultTeiidName() {
        return ( this.name.getMethodName() + "-Teiid" );
    }

    protected String getDefaultDataserviceName() {
        return ( this.name.getMethodName() + "-Dataservice" );
    }

    protected String getDefaultConnectionName() {
        return ( this.name.getMethodName() + "-Connection" );
    }

    protected String getDefaultFolderName() {
        return ( this.name.getMethodName() + "-Folder" );
    }
    
    protected String getDefaultViewEditorStateName() {
        return ( this.name.getMethodName() + "-ViewEditorState" );
    }

}
