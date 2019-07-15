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
package org.komodo.relational.dataservice.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;

import java.util.Calendar;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.internal.RelationalObjectImpl.Filter;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.workspace.WorkspaceManagerImpl;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataserviceImplTest extends RelationalModelTest {

    private static final String SERVICE_NAME = "myService";

    protected DataserviceImpl dataservice;
    private WorkspaceManagerImpl mgr;

    @Before
    public void init() throws Exception {
        this.dataservice = createDataservice( SERVICE_NAME );
        this.mgr = WorkspaceManagerImpl.getInstance( _repo, getTransaction() );

    }

    @Test
    public void shouldHaveName() throws Exception {
        assertThat( this.dataservice.getName( getTransaction() ), is( SERVICE_NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( getTransaction() );
        final String[] rawProps = this.dataservice.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( getTransaction() );
        final Filter[] filters = ((DataserviceImpl)this.dataservice).getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.dataservice.getPrimaryType( getTransaction() ).getName(), is( DataVirtLexicon.DataService.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.dataservice.getTypeIdentifier( getTransaction() ), is(KomodoType.DATASERVICE));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String descr = "This is a description";
        this.dataservice.setDescription(getTransaction(), descr);

        assertThat( this.dataservice.getDescription( getTransaction() ), is( descr ) );
    }

    @Test
    public void shouldSetModifiedBy() throws Exception {
        final String user = "elvis";
        this.dataservice.setModifiedBy( getTransaction(), user );
        assertThat( this.dataservice.getModifiedBy( getTransaction() ), is( user ) );
    }

    @Test
    public void shouldSetLastModified() throws Exception {
        final Calendar date = Calendar.getInstance();
        date.set( 2016, 8, 23, 13, 48, 33 );
        this.dataservice.setLastModified( getTransaction(), date );
        assertThat( this.dataservice.getLastModified( getTransaction() ), is( date ) );
    }

    @Test
    public void shouldAddAllChildren() throws Exception {
        final String vdbName = "MyServiceVdb";
        final int vdbVersion = 3;
        final VdbImpl serviceVdb = this.mgr.createVdb( getTransaction(), null, vdbName, "externalFilePath" );
        serviceVdb.setVdbName( getTransaction(), vdbName );
        serviceVdb.setVersion( getTransaction(), vdbVersion );
        commit(); // needed so that searching for reference will work

        final VdbImpl oldServiceVdb = this.dataservice.setServiceVdb( getTransaction(), serviceVdb );
        assertThat( oldServiceVdb, is( nullValue() ) );
        final ServiceVdbEntryImpl entry = this.dataservice.getServiceVdbEntry( getTransaction() );
        assertThat( entry, is( notNullValue() ) );

        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
    }

    @Test
    public void shouldSetServiceVdb() throws Exception {
        final String vdbName = "MyServiceVdb";
        final int vdbVersion = 3;
        final VdbImpl vdb = this.mgr.createVdb( getTransaction(), null, vdbName, "externalFilePath" );
        vdb.setVdbName( getTransaction(), vdbName );
        vdb.setVersion( getTransaction(), vdbVersion );
        commit(); // needed so that searching for reference will work

        final VdbImpl oldServiceVdb = this.dataservice.setServiceVdb( getTransaction(), vdb );
        assertThat( oldServiceVdb, is( nullValue() ) );
        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ), is( notNullValue() ) );

        final ServiceVdbEntryImpl entry = this.dataservice.getServiceVdbEntry( getTransaction() );
        assertThat( entry.getVdbName( getTransaction() ), is( vdbName ) );
        assertThat( entry.getVdbVersion( getTransaction() ), is( Integer.toString( vdbVersion ) ) );

        assertThat( this.dataservice.hasChild( getTransaction(), vdbName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), vdbName, DataVirtLexicon.ServiceVdbEntry.NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), vdbName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.ServiceVdbEntry.NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ServiceVdbEntry.NODE_TYPE,
                                                        vdbName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldGetServiceVdbViewModelAndView() throws Exception {
        final String name = "childVdb";
        final int version = 2;
        final WorkspaceManagerImpl mgr = WorkspaceManagerImpl.getInstance( _repo, getTransaction() );
        final VdbImpl serviceVdb = mgr.createVdb( getTransaction(), null, name, "externalFilePath" );
        serviceVdb.setVersion( getTransaction(), version );

        // Add a physical model
        final ModelImpl physModel = serviceVdb.addModel( getTransaction(), "physicalModel" );
        physModel.setModelType( getTransaction(), Model.Type.PHYSICAL );

        // Add a virtual model
        final String serviceViewModel = "serviceViewModel";
        final ModelImpl virtualModel = serviceVdb.addModel( getTransaction(), serviceViewModel );
        virtualModel.setModelType( getTransaction(), Model.Type.VIRTUAL );

        final String[] serviceViews = new String[0];

        commit(); // need this so that VDB will be found by query that sets reference

        // Add VDB to data service
        final VdbImpl old = this.dataservice.setServiceVdb( getTransaction(), serviceVdb );
        assertThat( old, is( nullValue() ) ); // not replacing
        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ).getName( getTransaction() ), is( name ) );
        assertThat( this.dataservice.getServiceVdb( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataservice.getServiceViewModelName( getTransaction() ), is( serviceViewModel ) );
        assertThat( this.dataservice.getViewDefinitionNames( getTransaction() ), is( serviceViews ) );
    }

}
