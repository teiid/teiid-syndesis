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
package org.komodo.rest.service.unit;

import static org.junit.Assert.assertTrue;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.jboss.resteasy.plugins.server.tjws.TJWSEmbeddedJaxrsServer;
import org.junit.rules.ExternalResource;
import org.komodo.importer.ImportMessages;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.service.AbstractServiceTest;
import org.komodo.test.utils.TestUtilities;

public class UnitServiceResources extends ExternalResource {

    private static int refCount = 0;

    private static UnitServiceResources instance;

    public static UnitServiceResources getInstance() {
        if (refCount == 0)
            instance = new UnitServiceResources();

        return instance;
    };

    @Override
    protected void before() throws Throwable {
        try {
            if (refCount > 0)
                return;

            initResources();

        } finally {
            //
            // Ensures before is only called once when:
            // a) test class is running on its own
            // b) test suite is running and all classes share this instance
            //
            refCount++;
        }
    }

    @Override
    protected void after() {
        refCount--;
    
        if (refCount > 0)
            return; // Still other classes using it so don't tear down yet
    
        destroyResources();
    }

    private static KomodoRestV1Application _restApp;
    private static TJWSEmbeddedJaxrsServer _server;
    private static URI _appUri;
    private static KomodoRestUriBuilder _uriBuilder;

    void loadVdbs() throws Exception {
        ImportMessages msgs = _restApp.importVdb(TestUtilities.allElementsExample(), AbstractServiceTest.USER_NAME);
        assertTrue(msgs.getErrorMessages().isEmpty());

        msgs = _restApp.importVdb(TestUtilities.portfolioExample(), AbstractServiceTest.USER_NAME);
        assertTrue(msgs.getErrorMessages().isEmpty());

        msgs = _restApp.importVdb(TestUtilities.partsWithKeysExample(), AbstractServiceTest.USER_NAME);
        assertTrue(msgs.getErrorMessages().isEmpty());

        msgs = _restApp.importVdb(TestUtilities.tweetExample(), AbstractServiceTest.USER_NAME);
        assertTrue(msgs.getErrorMessages().isEmpty());
    }

    private void initResources() throws Exception {
        _restApp = new KomodoRestV1Application();
        loadVdbs();

        _server = new TJWSEmbeddedJaxrsServer();
        _server.setPort(AbstractServiceTest.TEST_PORT);

        _server.getDeployment().setApplication(_restApp);
        _server.start();

        System.setProperty("org.jboss.resteasy.port", Integer.toString(AbstractServiceTest.TEST_PORT));
        URI baseUri = URI.create("http://localhost:8080");
        //
        // Note this lacks the /v1 context since the embedded server does not
        // seem to detect context from the application
        //
        _appUri = UriBuilder.fromUri(baseUri).scheme("http").build();
        _uriBuilder = new KomodoRestUriBuilder(_appUri);
    }

    private void destroyResources() {
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
