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
package org.komodo.rest;

import static org.komodo.rest.Messages.Error.KOMODO_ENGINE_STARTUP_TIMEOUT;

import java.io.File;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Random;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.transaction.TransactionManager;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.komodo.core.KEngine;
import org.komodo.core.repository.LocalRepository;
import org.komodo.metadata.DefaultMetadataInstance;
import org.komodo.metadata.TeiidConnectionProvider;
import org.komodo.openshift.EncryptionComponent;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.connections.SyndesisConnectionMonitor;
import org.komodo.rest.connections.SyndesisConnectionSynchronizer;
import org.komodo.spi.KException;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.ApplicationProperties;
import org.komodo.spi.repository.PersistenceType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.crypto.encrypt.Encryptors;
import org.springframework.security.crypto.encrypt.TextEncryptor;
import org.teiid.runtime.EmbeddedConfiguration;

@Configuration
@EnableConfigurationProperties(KomodoConfigurationProperties.class)
public class KomodoAutoConfiguration {
	
    @Value("${encrypt.key}")
    private String encryptKey;
    
    @Autowired(required=false)
    private TransactionManager transactionManager;
    
    @Autowired
    private KomodoConfigurationProperties config;

    @Bean
    public TextEncryptor getTextEncryptor() {
        return Encryptors.text(encryptKey, "deadbeef");
    }

    @Bean
    @ConditionalOnMissingBean
    public TeiidConnectionProvider teiidConnectionProvider() {
        TeiidConnectionProviderImpl connectionProvider = new TeiidConnectionProviderImpl();
        return connectionProvider;
    }

    @Bean
    @ConditionalOnMissingBean
    public DefaultMetadataInstance metadata(@Autowired TeiidConnectionProvider cp) {
        TeiidMetadataInstance metadata = new TeiidMetadataInstance(cp);
        return metadata;
    }

    @Bean
    @ConditionalOnMissingBean
    public KEngine kEngine(@Autowired DefaultMetadataInstance metadata) throws WebApplicationException {
        final KEngine kengine = new KEngine();
        boolean started;
        try {
            String workingDir = System.getProperty("user.dir");
            if (new File(workingDir+"/target").exists()) {
                Random r = new Random();
                workingDir = workingDir+"/target/"+Math.abs(r.nextLong());
            }
            // Set the komodo data directory prior to starting the engine
            System.setProperty(SystemConstants.ENGINE_DATA_DIR, workingDir+"/komodo/data"); //$NON-NLS-1$

            initPersistenceEnvironment();

            // configure metadata
            kengine.setMetadataInstance(metadata);

            // configure repository
            LocalRepository repository = new LocalRepository();
            kengine.setDefaultRepository(repository);

            started = kengine.startAndWait();
        } catch (Exception e) {
            throw new WebApplicationException( e, Status.INTERNAL_SERVER_ERROR );
        }

        if ( !started ) {
            throw new RuntimeException(Messages.getString( KOMODO_ENGINE_STARTUP_TIMEOUT, 1, TimeUnit.MINUTES));
        } else {
            try {
	        	// monitor to track connections from the syndesis
				TeiidOpenShiftClient TOSClient = new TeiidOpenShiftClient(
						(TeiidMetadataInstance) kengine.getMetadataInstance(),
						new EncryptionComponent(getTextEncryptor()), this.config);
	        	SyndesisConnectionSynchronizer sync = new SyndesisConnectionSynchronizer(TOSClient);
	        	SyndesisConnectionMonitor scm = new SyndesisConnectionMonitor(sync);
	        	ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1);
	        	executor.schedule(scm, 15, TimeUnit.SECONDS);
            } catch (KException e) {
                throw new WebApplicationException( e, Status.INTERNAL_SERVER_ERROR );
            }
        }
        return kengine;
    }

    @Bean
    @ConditionalOnMissingBean    
    public TeiidServer teiidServer() {

        // turning off PostgreSQL support
        System.setProperty("org.teiid.addPGMetadata", "false");
        System.setProperty("org.teiid.hiddenMetadataResolvable", "false");
        System.setProperty("org.teiid.allowAlter", "true");

        final TeiidServer server = new TeiidServer();

    	EmbeddedConfiguration config = new EmbeddedConfiguration();
    	if (this.transactionManager != null) {
    		config.setTransactionManager(this.transactionManager);
    	}
        server.start(config);
        return server;
    }

    @Bean
    @ConditionalOnMissingBean
    public TeiidOpenShiftClient openShiftClient(@Autowired KEngine kengine, @Autowired TextEncryptor enc) {
        try {
            return new TeiidOpenShiftClient((TeiidMetadataInstance) kengine.getMetadataInstance(),
                    new EncryptionComponent(enc), this.config);
        } catch (KException e) {
            throw new RuntimeException(e);
        }
    }

    private void initPersistenceEnvironment() {
        //
        // If the env variable REPOSITORY_PERSISTENCE_TYPE is defined then respect
        // its value and set the persistence type accordingly. If not defined then assume
        // PGSQL is required.
        //
        PersistenceType persistenceType = PersistenceType.PGSQL;
        String pType = ApplicationProperties.getRepositoryPersistenceType();
        if (pType == null || PersistenceType.H2.name().equals(pType)) {
            persistenceType = PersistenceType.H2;
        }

        if (ApplicationProperties.getRepositoryPersistenceHost() == null) {
            ApplicationProperties.setRepositoryPersistenceHost("localhost");
        }

        if (ApplicationProperties.getRepositoryPersistenceURL() == null) {
            ApplicationProperties.setRepositoryPersistenceURL(persistenceType.getConnUrl());
        }

        if (ApplicationProperties.getRepositoryPersistenceBinaryStoreURL() == null) {

            //
            // If the connection url has been defined then prefer that before the default
            //
            String binaryStoreUrl = ApplicationProperties.getRepositoryPersistenceURL();
            if (binaryStoreUrl == null) {
                //
                // Connection Url not defined so assume the default
                //
                binaryStoreUrl = persistenceType.getBinaryStoreUrl();
            }

            ApplicationProperties.setRepositoryPersistenceBinaryStoreURL(binaryStoreUrl);
        }

        if (ApplicationProperties.getRepositoryPersistenceDriver() == null) {
            ApplicationProperties.setRepositoryPersistenceDriver(persistenceType.getDriver());
        }

        String persistenceUser = ApplicationProperties.getRepositoryPersistenceUser();
        if (ApplicationProperties.getRepositoryPersistenceDefaultUser().equals(persistenceUser)) {
            //
            // Either the default user is being used or more importantly the user has not been set
            // To ensure komodo does not complain about a lack of user, ie. exceptions concerning
            // ${komodo.user} set the user accordingly.
            //
            ApplicationProperties.setRepositoryPersistenceUser(persistenceUser);
        }

        String persistencePasswd = ApplicationProperties.getRepositoryPersistencePassword();
        if (ApplicationProperties.getRepositoryPersistenceDefaultPassword().equals(persistencePasswd)) {
            //
            // Either the default password is being used or more importantly the password has not been set
            // To ensure komodo does not complain about a lack of password, ie. exceptions concerning
            // ${komodo.password} set the password accordingly.
            //
            ApplicationProperties.setRepositoryPersistencePassword(persistencePasswd);
        }

        //
        // No need to check repo storage for H2 as its generated upon first repository connection
        // Other persistence types are external so do require this.
        //
        if(persistenceType.isExternal()) {
            checkRepoStorage();
        }
    }

    private void validateRepositoryProperties() {
        if (ApplicationProperties.getRepositoryPersistenceDriver() == null
                || ApplicationProperties.getRepositoryPersistenceURL() == null) {
            throw new RuntimeException("Properties required to make connection to the repository storage missing");
        }
    }

    /**
     * Check there is a store for repository persistence.
     * @throws WebApplicationException
     */
    private void checkRepoStorage() {
        validateRepositoryProperties();
        try {
            Class.forName(ApplicationProperties.getRepositoryPersistenceDriver());
        } catch (Exception ex) {
            throw new WebApplicationException("Failed to initialise repository persistence driver", ex);
        }

        java.sql.Connection connection = null;
        String connUrl = ApplicationProperties.getRepositoryPersistenceURL();

        //
        // If HOST has been defined then ensure that connection url is evaluated first to replace the
        // property name with its value. Otherwise connection url remains unchanged
        //
        connUrl = ApplicationProperties.substitute(connUrl, SystemConstants.REPOSITORY_PERSISTENCE_HOST);

        String user = ApplicationProperties.getRepositoryPersistenceUser();
        String password = ApplicationProperties.getRepositoryPersistencePassword();
        try  {
            connection = DriverManager.getConnection(connUrl, user, password);
        } catch (Exception ex) {
            throw new WebApplicationException("Failed to connect to persistence storage: " + connUrl + ";" + user, ex);
        } finally {
            if(connection != null) {
                try {
                    connection.close();
                } catch (SQLException e) {
                    // nothing to do
                }
            }
        }
    }
}
