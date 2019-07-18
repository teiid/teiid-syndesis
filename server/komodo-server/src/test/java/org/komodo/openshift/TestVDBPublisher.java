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
import java.util.Properties;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.komodo.datasources.DefaultSyndesisDataSource;
import org.komodo.datasources.MySQLDefinition;
import org.komodo.datasources.PostgreSQLDefinition;
import org.komodo.metadata.MetadataInstance;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.AuthHandlingFilter;
import org.komodo.rest.AuthHandlingFilter.OAuthCredentials;
import org.komodo.rest.KomodoConfigurationProperties;
import org.komodo.spi.KException;
import org.mockito.Mockito;
import org.teiid.core.util.ObjectConverterUtil;

import io.fabric8.kubernetes.api.model.EnvVar;

public class TestVDBPublisher extends RelationalModelTest {

    @Before
    public void setup() throws KException, Exception {
        final InputStream vdbStream = getClass().getClassLoader().getResourceAsStream("myservice-vdb.xml");
        assertThat( vdbStream, is( notNullValue() ) );

        final String name = "myservice";

		/*
		 * <vdb name="myservice" version="1"> <model visible="true" name="accounts">
		 * <source name="accounts-xyz" translator-name="postgresql"
		 * connection-jndi-name="java:/accountsDS"/> </model> </vdb>
		 */

        Vdb vdb = createVdb(name);
        Model accounts = vdb.addModel(getTransaction(), "accounts");
        accounts.setVisible(getTransaction(), true);
        ModelSource source = accounts.addSource(getTransaction(), "accounts-xyz");
        source.setTranslatorName(getTransaction(), "postgresql");
        source.setJndiName(getTransaction(), "java:/accountsDS");

        commit(); // commit the import
    }

	private TeiidOpenShiftClient testDataSetup() throws KException {
        AuthHandlingFilter.threadOAuthCredentials.set(new OAuthCredentials("token", "user"));
        MetadataInstance metadata = Mockito.mock(MetadataInstance.class);

        HashSet<DefaultSyndesisDataSource> sources = new HashSet<>();
        sources.add(getMySQLDS());
        sources.add(getPostgreSQL());

		TeiidOpenShiftClient client = new TeiidOpenShiftClient(metadata, new EncryptionComponent("blah"),
				new KomodoConfigurationProperties()) {
            @Override
            public Set<DefaultSyndesisDataSource> getSyndesisSources(OAuthCredentials authToken) throws KException {
                return sources;
            }
            @Override
            public DefaultSyndesisDataSource getSyndesisDataSource(OAuthCredentials authToken, String dsName) throws KException {
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
    public void testDecryption() throws Exception {
        EncryptionComponent ec = new EncryptionComponent("GpADvcFIBgqMUwSfvljdQ1N5qeQFNXaAToht2O4kgBW2bIalkcPWphs54C4e7mjq");
        Properties credentialData = new Properties();
        credentialData.put("password", "»ENC:7965a258e2f0029b0e5e797b81917366ed11608f195755fc4fcfebecfca4781917de289fb8579d306741b5ec5680a686");
        credentialData.put("schema", "sampledb");
        credentialData.put("url", "jdbc:mysql://localhost:1521/sampledb");
        credentialData.put("user", "johnny");
        Properties decrypted = ec.decrypt(credentialData);
        assertThat(credentialData.getProperty("password").equals(decrypted.getProperty("password")), is(false));
    }

    @Test
    public void testGeneratePomXML() throws Exception {
        TeiidOpenShiftClient generator = testDataSetup();

        final Vdb[] vdbs = findVdbs();
        assertThat( vdbs.length, is(1));

        final OAuthCredentials authToken = AuthHandlingFilter.threadOAuthCredentials.get();
        String pom = generator.generatePomXml(authToken, getTransaction(), vdbs[0], false);
        assertEquals(ObjectConverterUtil.convertFileToString(new File("src/test/resources/generated-pom.xml")), pom);
    }
    
    @Test
    public void testGenerateDataSource() throws Exception {
        TeiidOpenShiftClient generator = testDataSetup();

        final Vdb[] vdbs = findVdbs();
        assertThat( vdbs.length, is(1));

        InputStream dsIs = generator.buildDataSourceBuilders(vdbs[0], getTransaction());
        String ds = ObjectConverterUtil.convertToString(dsIs);
        assertEquals(ObjectConverterUtil.convertFileToString(new File("src/test/resources/generated-ds.txt")), ds);
    }    

    @Test
    public void testGenerateDeploymentYML() throws Exception {
        TeiidOpenShiftClient generator = testDataSetup();

        final Vdb[] vdbs = findVdbs();
        assertThat( vdbs.length, is(1));

        final OAuthCredentials authToken = AuthHandlingFilter.threadOAuthCredentials.get();
        PublishConfiguration config = new PublishConfiguration();
        Collection<EnvVar> variables = generator
                .getEnvironmentVariablesForVDBDataSources(authToken, getTransaction(), vdbs[0], config);
        assertThat( variables.size(), is(9));
        
        String javaOptions= 
                  " -XX:+UnlockExperimentalVMOptions -XX:+UseCGroupMemoryLimitForHeap"
                + " -Djava.net.preferIPv4Addresses=true -Djava.net.preferIPv4Stack=true"
                + " -XX:ParallelGCThreads=1 -XX:ConcGCThreads=1"
                + " -Djava.util.concurrent.ForkJoinPool.common.parallelism=1"
                + " -Dio.netty.eventLoopThreads=2";
        
        assertThat(variables, hasItem(generator.envFromSecret("myservice-secret", "spring.datasource.accounts-xyz.username")));
        assertThat(variables, hasItem(generator.envFromSecret("myservice-secret", "spring.datasource.accounts-xyz.jdbc-url")));
        assertThat(variables, hasItem(generator.envFromSecret("myservice-secret", "spring.datasource.accounts-xyz.password")));

//        assertThat(variables, hasItem(new EnvVarBuilder().withName(EncryptionComponent.SYNDESIS_ENC_KEY)
//                .withValueFrom(new EnvVarSourceBuilder().withConfigMapKeyRef(new ConfigMapKeySelectorBuilder()
//                        .withName("syndesis-server-config").withKey("encrypt.key").build()).build()).build()));

        assertThat(variables, hasItem(new EnvVar("GC_MAX_METASPACE_SIZE", "256", null)));

        assertThat(variables, hasItem(new EnvVar("JAVA_OPTIONS", javaOptions, null)));
    }

}
