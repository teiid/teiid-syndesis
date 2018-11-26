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
package org.komodo.rest.service.unit;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;

import javax.ws.rs.core.UriBuilder;

import org.jboss.resteasy.plugins.server.tjws.TJWSEmbeddedJaxrsServer;
import org.junit.rules.ExternalResource;
import org.komodo.importer.ImportMessages;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.spi.repository.ApplicationProperties;
import org.komodo.spi.repository.PersistenceType;
import org.komodo.test.utils.TestUtilities;

public class UnitServiceResources extends ExternalResource {
    private static UnitServiceResources instance;
    
    public synchronized static UnitServiceResources getInstance() {
        if (instance == null) {
            instance = new UnitServiceResources();
            ApplicationProperties.setRepositoryPersistenceType(PersistenceType.H2.name());
            instance.initResources();
        }
        return instance;
    };
    
    @Override
    protected void before() throws Throwable {
        //initResources();
    }

    @Override
    protected void after() {
        destroyResources();
    }

    private static KomodoRestV1Application _restApp;
    private static TJWSEmbeddedJaxrsServer _server;
    private static URI _appUri;
    private static KomodoRestUriBuilder _uriBuilder;

    void loadVdbs() {
        try {
            ImportMessages msgs = _restApp.importVdb(TestUtilities.allElementsExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());

            msgs = _restApp.importVdb(TestUtilities.portfolioExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());

            msgs = _restApp.importVdb(TestUtilities.partsWithKeysExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());

            msgs = _restApp.importVdb(TestUtilities.tweetExample(), AbstractServiceTest.USER_NAME);
            assertTrue(msgs.getErrorMessages().isEmpty());
        } catch (Exception e) {
            fail(e.getMessage());
        }
    }

    private void initResources()  {
        System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$ START SERVERS $$$$$$$$$$$$$$$$$$$$$");
        _restApp = new KomodoRestV1Application();
        loadVdbs();

        _server = new TJWSEmbeddedJaxrsServer();
        _server.setPort(AbstractServiceTest.TEST_PORT);

        _server.getDeployment().setApplication(_restApp);
        _server.start();

        //System.setProperty("org.jboss.resteasy.port", Integer.toString(AbstractServiceTest.TEST_PORT));
        URI baseUri = URI.create("http://localhost:8080/");
        //
        // Note this lacks the /v1 context since the embedded server does not
        // seem to detect context from the application
        //
        _appUri = UriBuilder.fromUri(baseUri).scheme("http").build();
        _uriBuilder = new KomodoRestUriBuilder(_appUri);
    }

    private void destroyResources() {
        System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$ STOP SERVERS $$$$$$$$$$$$$$$$$$$$$");
        if (_server != null)
            _server.stop();

        if (_restApp != null)
            _restApp.stop();
    }

    public KomodoRestV1Application restApp() {
        return _restApp;
    }

    public TJWSEmbeddedJaxrsServer server() {
        return _server;
    }

    public URI appUri() {
        return _appUri;
    }

    public KomodoRestUriBuilder uriBuilder() {
        return _uriBuilder;
    }
}
