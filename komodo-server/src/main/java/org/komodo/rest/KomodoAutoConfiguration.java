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

import java.util.concurrent.ScheduledThreadPoolExecutor;

import javax.transaction.TransactionManager;

import org.komodo.KEngine;
import org.komodo.metadata.MetadataInstance;
import org.komodo.metadata.internal.DefaultMetadataInstance;
import org.komodo.metadata.internal.TeiidServer;
import org.komodo.openshift.EncryptionComponent;
import org.komodo.openshift.TeiidOpenShiftClient;
import org.komodo.repository.WorkspaceManagerImpl;
import org.komodo.rest.connections.SyndesisConnectionSynchronizer;
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
@ComponentScan(basePackageClasses = {WorkspaceManagerImpl.class, DefaultMetadataInstance.class, SyndesisConnectionSynchronizer.class})
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

    @Bean(name = "connectionExecutor")
    public ScheduledThreadPoolExecutor connectionExecutor() {
        return executor;
    }

    @Bean
    public TextEncryptor getTextEncryptor() {
        return Encryptors.text(encryptKey, "deadbeef");
    }

    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        try {
            kengine.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Bean
    @ConditionalOnMissingBean
    public TeiidServer teiidServer() {

        // turning off PostgreSQL support
        System.setProperty("org.teiid.addPGMetadata", "false");
        System.setProperty("org.teiid.hiddenMetadataResolvable", "true");
        System.setProperty("org.teiid.allowAlter", "false");

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
                this.config, kengine);
    }

}
