/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.servicecatalog;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.io.File;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.komodo.core.AbstractLocalRepositoryTest;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.TeiidSwarmMetadataInstance;
import org.komodo.servicecatalog.datasources.DefaultServiceCatalogDataSource;
import org.komodo.servicecatalog.datasources.MySQLDefinition;
import org.komodo.servicecatalog.datasources.PostgreSQLDefinition;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.runtime.ServiceCatalogDataSource;
import org.mockito.Mockito;
import org.teiid.core.util.ObjectConverterUtil;

import io.fabric8.kubernetes.api.model.EnvVar;
import io.fabric8.kubernetes.api.model.EnvVarSource;
import io.fabric8.kubernetes.api.model.SecretKeySelector;

public class TestVDBPublisher extends AbstractLocalRepositoryTest {

    @Before
    public void setup() throws KException, Exception {
        final InputStream vdbStream = getClass().getClassLoader().getResourceAsStream("myservice-vdb.xml");
        assertThat( vdbStream, is( notNullValue() ) );

        final String name = "myservice";
        final VdbImporter importer = new VdbImporter( _repo );
        final ImportOptions importOptions = new ImportOptions();
        importOptions.setOption( OptionKeys.NAME, name );
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        importer.importVdb( getTransaction(), vdbStream, workspace, importOptions, new ImportMessages() );

        commit(); // commit the import
    }

    private TeiidOpenShiftClient testDataSetup() throws KException {
        AuthHandlingFilter.threadOAuthCredentials.set(new OAuthCredentials("token", "user"));
        TeiidSwarmMetadataInstance metadata = Mockito.mock(TeiidSwarmMetadataInstance.class);

        HashSet<ServiceCatalogDataSource> sources = new HashSet<>();
        sources.add(getMySQLDS());
        sources.add(getPostgreSQL());

        TeiidOpenShiftClient client = new TeiidOpenShiftClient(metadata) {
            @Override
            public Set<ServiceCatalogDataSource> getServiceCatalogSources() throws KException {
                return sources;
            }
            @Override
            public DefaultServiceCatalogDataSource getServiceCatalogDataSource(String dsName) throws KException {
                if (dsName.equals("accounts-xyz")) {
                    return getPostgreSQL();
                } else {
                    return getMySQLDS();
                }
            }
        };
        return client;
    }

    private DefaultServiceCatalogDataSource getMySQLDS() {
        DefaultServiceCatalogDataSource ds1 = new DefaultServiceCatalogDataSource();
        ds1.setName("inventory-abc");
        ds1.setBound(true);
        ds1.setTranslatorName("mysql5");
        ds1.setDefinition(new MySQLDefinition());

        HashMap<String, String> parameterData = new HashMap<>();
        parameterData.put("DATABASE_SERVICE_NAME", "mariadb");
        parameterData.put("MEMORY_LIMIT", "512Mi");
        parameterData.put("MYSQL_DATABASE", "sampledb");
        parameterData.put("NAMESPACE", "openshift");
        parameterData.put("VOLUME_CAPACITY", "1Gi");
        ds1.setParameters(new DecodedSecret("parameters-mysql", parameterData));

        HashMap<String, String> credentialData = new HashMap<>();
        credentialData.put("database-password", "my-pass");
        credentialData.put("database-name", "sampledb");
        credentialData.put("database-root-password", "root");
        credentialData.put("database-user", "johnny");
        ds1.setCredentials(new DecodedSecret("mysecrect-mysql", credentialData));
        return ds1;
    }

    private DefaultServiceCatalogDataSource getPostgreSQL() {
        DefaultServiceCatalogDataSource ds2 = new DefaultServiceCatalogDataSource();
        ds2.setName("accounts-xyz");
        ds2.setBound(true);
        ds2.setTranslatorName("postgresql");
        ds2.setDefinition(new PostgreSQLDefinition());

        HashMap<String, String> parameterData2 = new HashMap<>();
        parameterData2.put("DATABASE_SERVICE_NAME", "postgresql");
        parameterData2.put("MEMORY_LIMIT", "512Mi");
        parameterData2.put("POSTGRESQL_DATABASE", "sampledb");
        parameterData2.put("NAMESPACE", "openshift");
        parameterData2.put("VOLUME_CAPACITY", "1Gi");
        parameterData2.put("VERSION", "9.5");
        ds2.setParameters(new DecodedSecret("parameters-pg", parameterData2));

        HashMap<String, String> credentialData2 = new HashMap<>();
        credentialData2.put("database-password", "my-pass");
        credentialData2.put("database-name", "sampledb");
        credentialData2.put("database-user", "johnny");
        ds2.setCredentials(new DecodedSecret("mysecrect-pg", credentialData2));
        return ds2;
    }

    @Test
    public void testGeneratePomXML() throws Exception {
        TeiidOpenShiftClient generator = testDataSetup();

        final Vdb[] vdbs = WorkspaceManager.getInstance( _repo, getTransaction() ).findVdbs( getTransaction() );
        assertThat( vdbs.length, is(1));

        String pom = generator.generatePomXml(getTransaction(), vdbs[0], false);
        assertEquals(ObjectConverterUtil.convertFileToString(new File("src/test/resources/generated-pom.xml")), pom);
    }

    @Test
    public void testGenerateDeploymentYML() throws Exception {
        TeiidOpenShiftClient generator = testDataSetup();

        final Vdb[] vdbs = WorkspaceManager.getInstance( _repo, getTransaction() ).findVdbs( getTransaction() );
        assertThat( vdbs.length, is(1));

        Collection<EnvVar> variables = generator
                .getEnvironmentVaribalesForVDBDataSources(getTransaction(), vdbs[0]);
        assertThat( variables.size(), is(13));
        String javaOptions= " -Dswarm.datasources.data-sources.accounts-xyz.driver-name=postgresql "
                + "-Dswarm.datasources.data-sources.accounts-xyz.user-name=$(MYSECRECT_PG_USERNAME) "
                + "-Dswarm.datasources.data-sources.accounts-xyz.jndi-name=java:/accountsDS "
                + "-Dswarm.datasources.data-sources.accounts-xyz.password=$(MYSECRECT_PG_PASSWORD) "
                + "-Dswarm.datasources.data-sources.accounts-xyz.connection-url="
                + "jdbc:postgresql://$(PARAMETERS_PG_DATABASE_SERVICE_NAME):5432/$(MYSECRECT_PG_DATABASE_NAME)"
                + " -XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap"
                + " -Djava.net.preferIPv4Addresses=true -Djava.net.preferIPv4Stack=true";

        assertThat(variables, hasItem(new EnvVar("PARAMETERS_PG_NAMESPACE", "openshift", null)));
        assertThat(variables, hasItem(new EnvVar("PARAMETERS_PG_VOLUME_CAPACITY", "1Gi", null)));
        assertThat(variables, hasItem(new EnvVar("PARAMETERS_PG_DATABASE_SERVICE_NAME", "postgresql", null)));
        assertThat(variables, hasItem(new EnvVar("PARAMETERS_PG_VERSION", "9.5", null)));
        assertThat(variables, hasItem(new EnvVar("PARAMETERS_PG_POSTGRESQL_DATABASE", "sampledb", null)));
        assertThat(variables, hasItem(new EnvVar("PARAMETERS_PG_MEMORY_LIMIT", "512Mi", null)));
        
        assertThat(variables, hasItem(new EnvVar("MYSECRECT_PG_DATABASE_USER", null,
                new EnvVarSource(null, null, null, new SecretKeySelector("database-user", "mysecrect-pg", null)))));
        assertThat(variables, hasItem(new EnvVar("MYSECRECT_PG_DATABASE_NAME", null,
                new EnvVarSource(null, null, null, new SecretKeySelector("database-name", "mysecrect-pg", null)))));
        assertThat(variables, hasItem(new EnvVar("MYSECRECT_PG_DATABASE_PASSWORD", null,
                new EnvVarSource(null, null, null, new SecretKeySelector("database-password", "mysecrect-pg", null)))));

        assertThat(variables, hasItem(new EnvVar("JAVA_OPTIONS", javaOptions, null)));
        
        assertThat(variables, hasItem(new EnvVar("AB_JOLOKIA_OFF", "true", null)));
        assertThat(variables, hasItem(new EnvVar("AB_OFF", "true", null)));
        assertThat(variables, hasItem(new EnvVar("GC_MAX_METASPACE_SIZE", "256", null)));
    }
}
