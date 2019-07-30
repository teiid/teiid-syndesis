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

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.transaction.TransactionManager;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.komodo.KEngine;
import org.komodo.metadata.MetadataInstance;
import org.komodo.openshift.EncryptionComponent;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.rest.connections.SyndesisConnectionMonitor;
import org.komodo.rest.connections.SyndesisConnectionSynchronizer;
import org.komodo.rest.service.KomodoMetadataService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.security.crypto.encrypt.Encryptors;
import org.springframework.security.crypto.encrypt.TextEncryptor;
import org.teiid.runtime.EmbeddedConfiguration;

@Configuration
@EnableConfigurationProperties(KomodoConfigurationProperties.class)
@ComponentScan(basePackages = {"org.komodo.repository", "org.komodo.metadata.internal"})
public class KomodoAutoConfiguration implements ApplicationListener<ContextRefreshedEvent> {
	
    @Value("${encrypt.key}")
    private String encryptKey;
    
    @Autowired(required=false)
    private TransactionManager transactionManager;
    
    @Autowired
    private KomodoConfigurationProperties config;
    
    @Autowired
    private KEngine kengine;
    
    @Autowired
    private MetadataInstance metadataInstance;
    
    private ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1);
    
    @Bean
    public TextEncryptor getTextEncryptor() {
        return Encryptors.text(encryptKey, "deadbeef");
    }
    
    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        boolean started;
        try {
            started = kengine.startAndWait();
        } catch (Exception e) {
            throw new WebApplicationException( e, Status.INTERNAL_SERVER_ERROR );
        }

        if ( !started ) {
            throw new RuntimeException(Messages.getString( KOMODO_ENGINE_STARTUP_TIMEOUT, 1, TimeUnit.MINUTES));
        } else {
        	// monitor to track connections from the syndesis
			TeiidOpenShiftClient TOSClient = new TeiidOpenShiftClient(
					metadataInstance, new EncryptionComponent(getTextEncryptor()),
					this.config);
			SyndesisConnectionSynchronizer sync = new SyndesisConnectionSynchronizer(TOSClient, event.getApplicationContext().getBean(KomodoMetadataService.class));
        	SyndesisConnectionMonitor scm = new SyndesisConnectionMonitor(sync, executor);
    		this.executor.scheduleAtFixedRate(()->scm.connect(), 5, 15, TimeUnit.SECONDS);
        }
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
        return new TeiidOpenShiftClient(metadataInstance, new EncryptionComponent(enc),
                this.config);
    }

}
