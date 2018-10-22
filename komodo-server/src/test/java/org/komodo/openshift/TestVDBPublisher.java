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
package org.komodo.openshift;

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
import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.datasources.MySQLDefinition;
import org.komodo.datasources.PostgreSQLDefinition;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.AuthHandlingFilter;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.TeiidSwarmMetadataInstance;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.runtime.SyndesisDataSource;
import org.mockito.Mockito;
import org.teiid.core.util.ObjectConverterUtil;

import io.fabric8.kubernetes.api.model.ConfigMapKeySelectorBuilder;
import io.fabric8.kubernetes.api.model.EnvVar;
import io.fabric8.kubernetes.api.model.EnvVarBuilder;
import io.fabric8.kubernetes.api.model.EnvVarSourceBuilder;

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

        HashSet<SyndesisDataSource> sources = new HashSet<>();
        sources.add(getMySQLDS());
        sources.add(getPostgreSQL());

        TeiidOpenShiftClient client = new TeiidOpenShiftClient(metadata) {
            @Override
            public Set<SyndesisDataSource> getSyndesisSources(OAuthCredentials authToken) throws KException {
                return sources;
            }
            @Override
            public DefaultSyndesisDataSource getServiceCatalogDataSource(OAuthCredentials authToken, String dsName) throws KException {
                if (dsName.equals("accounts-xyz")) {
                    return getPostgreSQL();
                } else {
                    return getMySQLDS();
                }
            }
        };
        return client;
    }

    private DefaultSyndesisDataSource getMySQLDS() {
        DefaultSyndesisDataSource ds1 = new DefaultSyndesisDataSource();
        ds1.setName("inventory-abc");
        ds1.setBound(true);
        ds1.setTranslatorName("mysql5");
        ds1.setDefinition(new MySQLDefinition());

        HashMap<String, String> credentialData = new HashMap<>();
        credentialData.put("password", "my-pass");
        credentialData.put("schema", "sampledb");
        credentialData.put("url", "jdbc:mysql://localhost:1521/sampledb");
        credentialData.put("user", "johnny");
        ds1.setProperties(credentialData);
        return ds1;
    }

    private DefaultSyndesisDataSource getPostgreSQL() {
        DefaultSyndesisDataSource ds2 = new DefaultSyndesisDataSource();
        ds2.setName("accounts-xyz");
        ds2.setBound(true);
        ds2.setTranslatorName("postgresql");
        ds2.setDefinition(new PostgreSQLDefinition());

        HashMap<String, String> credentialData = new HashMap<>();
        credentialData.put("password", "my-pass");
        credentialData.put("schema", "sampledb");
        credentialData.put("url", "jdbc:mysql://localhost:1521/sampledb");
        credentialData.put("user", "johnny");
        ds2.setProperties(credentialData);
        return ds2;
    }

    @Test
    public void testGeneratePomXML() throws Exception {
        TeiidOpenShiftClient generator = testDataSetup();

        final Vdb[] vdbs = WorkspaceManager.getInstance( _repo, getTransaction() ).findVdbs( getTransaction() );
        assertThat( vdbs.length, is(1));

        final OAuthCredentials authToken = AuthHandlingFilter.threadOAuthCredentials.get();
        String pom = generator.generatePomXml(authToken, getTransaction(), vdbs[0], false);
        assertEquals(ObjectConverterUtil.convertFileToString(new File("src/test/resources/generated-pom.xml")), pom);
    }

    @Test
    public void testGenerateDeploymentYML() throws Exception {
        TeiidOpenShiftClient generator = testDataSetup();

        final Vdb[] vdbs = WorkspaceManager.getInstance( _repo, getTransaction() ).findVdbs( getTransaction() );
        assertThat( vdbs.length, is(1));

        final OAuthCredentials authToken = AuthHandlingFilter.threadOAuthCredentials.get();
        PublishConfiguration config = new PublishConfiguration();
        Collection<EnvVar> variables = generator
                .getEnvironmentVariablesForVDBDataSources(authToken, getTransaction(), vdbs[0], config);
        assertThat( variables.size(), is(9));
        String javaOptions= " -Dswarm.datasources.data-sources.accounts-xyz.driver-name=postgresql"
                + " -Dswarm.datasources.data-sources.accounts-xyz.user-name=$(ACCOUNTS_XYZ_USER)"
                + " -Dswarm.datasources.data-sources.accounts-xyz.jndi-name=java:/accountsDS"
                + " -Dswarm.datasources.data-sources.accounts-xyz.password=$(ACCOUNTS_XYZ_PASSWORD)"
                + " -Dswarm.datasources.data-sources.accounts-xyz.connection-url=$(ACCOUNTS_XYZ_URL)"
                + " -XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap"
                + " -Djava.net.preferIPv4Addresses=true -Djava.net.preferIPv4Stack=true"
                + " -XX:ParallelGCThreads=1 -XX:ConcGCThreads=1"
                + " -Djava.util.concurrent.ForkJoinPool.common.parallelism=1"
                + " -Dio.netty.eventLoopThreads=2";

        assertThat(variables, hasItem(new EnvVar("ACCOUNTS_XYZ_USER", "johnny", null)));
        assertThat(variables, hasItem(new EnvVar("ACCOUNTS_XYZ_URL", "jdbc:mysql://localhost:1521/sampledb", null)));
        assertThat(variables, hasItem(new EnvVar("ACCOUNTS_XYZ_PASSWORD", "my-pass", null)));

        assertThat(variables, hasItem(new EnvVarBuilder().withName(EncryptionComponent.SYNDESIS_ENC_KEY)
                .withValueFrom(new EnvVarSourceBuilder().withConfigMapKeyRef(new ConfigMapKeySelectorBuilder()
                        .withName("syndesis-server-config").withKey("encrypt.key").build()).build()).build()));

        assertThat(variables, hasItem(new EnvVar("AB_JOLOKIA_OFF", "true", null)));
        assertThat(variables, hasItem(new EnvVar("AB_OFF", "true", null)));
        assertThat(variables, hasItem(new EnvVar("GC_MAX_METASPACE_SIZE", "256", null)));

        assertThat(variables, hasItem(new EnvVar("JAVA_OPTIONS", javaOptions, null)));
        
    }
}
